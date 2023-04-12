library(ptaxsim)
library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(data.table)

# Create the DB connection with the default name expected by PTAXSIM functions
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db")

#get data by municipality for pins and tifs
base_url <- "https://datacatalog.cookcountyil.gov/resource/tx2p-k2g9.json"

cook_muni <- GET(
  base_url,
  query = list(
    year = 2021,
    `$select` = paste0(
      c("pin", "class", "tax_code", "cook_municipality_name"),
      collapse = ","
    ),
    `$limit` = 2000000L
  )
)

muni_pins <- fromJSON(rawToChar(cook_muni$content)) %>%
  mutate(name_mini = if_else(cook_municipality_name == 'Unincorporated',
                             'Unincorporated',
                             str_extract(cook_municipality_name, '(?<=Of ).*'))
         ) %>% tibble()


muni_tif <- 
  readxl::read_excel('ptaxsim/data-raw/tif/distribution/2021 TIF Agency Distribution Report_0.xlsx') %>% 
  select(`TIF Agency`, `TIF Tax Code`, `TIF Name`) %>% 
  mutate(name_mini = stringr::str_to_title(
    str_extract(`TIF Name`, '(?<=OF )[^-]*')
  )) %>% tibble() %>%
  mutate(name_mini = case_when(
    name_mini == 'Arlington Hts' ~ 'Arlington Heights',
    name_mini == 'East Hazelcrest' ~ 'East Hazel Crest',
    name_mini == 'Hazelcrest' ~ 'Hazel Crest',
    name_mini == 'Hoffman Est' ~ 'Hoffman Estates',
    name_mini == 'So Chicago Hts' ~ 'South Chicago Heights',
    
    TRUE ~ name_mini
  ))

#get EAV, caps for TIF pins
tif_dists_full <- tbl(ptaxsim_db_conn, 'tif_distribution') %>% 
  filter(year == 2021) %>%
  collect() %>%
  tibble()

 all_tif_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue::glue_sql("
    SELECT DISTINCT pin
    FROM pin
    WHERE tax_code_num IN ({tax_codes*}) and year == 2021
  ",
                 .con = ptaxsim_db_conn,
                 tax_codes = unique(tif_dists_full$tax_code_num)
  )
) %>%
  pull(pin)

all_tif_pins_dt <- lookup_pin(2021, pin = all_tif_pins) %>%
  mutate(tax_code_num = lookup_tax_code(year, pin))

all_tif_pins_summ <- all_tif_pins_dt %>%
  left_join(tif_dists_full %>% select(year, tax_code_num, agency_num)) %>%
  group_by(year, agency_num, tax_code_num) %>%
  summarize(total_eav = sum(eav)) %>%
  left_join(tif_dists_full) %>%
  mutate(amt_to_tif = total_eav - tax_code_frozen_eav) %>%
  ungroup()

rm(ptaxsim_db_conn)


calc_tif_stats <- function(tif_agency_num){
  ptaxsim_db_conn <<- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db")
  
  #based on https://ccao-data-science---modeling.gitlab.io/packages/ptaxsim/articles/tifs.html
  targ_tif <- tif_agency_num
  targ_muni <- muni_tif %>% filter(`TIF Agency` %in% targ_tif) %>% 
    distinct(name_mini) %>% pull(name_mini)
  targ_pins <- muni_pins %>% filter(name_mini == targ_muni) %>% pull(pin)
  targ_tax_codes <- tif_dists_full %>% 
    filter(year == 2021, agency_num %in% targ_tif) %>%
    pull(tax_code_num)
  
  # 1. Update TIF distribution amounts
  
  tif_dt_cntr <- lookup_tif(
    2021,
    lookup_tax_code(2021, targ_pins)
  ) %>%
    # Set only the targ TIF share to 0, leave all others untouched
    mutate(tif_share = ifelse(agency_num %in% targ_tif, 0, tif_share))
  
  # 2. Recalculate tax bases with EAV recovered from the TIF
  
  # Get the unaltered levy and base for all PINs in muni
  tif_agency_cntr <- lookup_agency(
    2021,
    lookup_tax_code(2021, targ_pins)
  )
  
  # For each agency, get the amount recovered from the TIF using the
  # summary table we created earlier (all_tif_pins_summ)
  tif_agency_amt_to_add <- tif_agency_cntr %>%
    filter(tax_code %in% targ_tax_codes) %>%
    distinct(year, agency_num, tax_code) %>%
    left_join(
      all_tif_pins_summ %>%
        filter(tax_code_num %in% targ_tax_codes) %>% 
        select(year, amt_to_tif, tax_code_num),
      by=join_by(year, tax_code==tax_code_num)
    ) %>% 
    mutate(amt_to_tif = if_else(amt_to_tif < 0, as.integer(0), amt_to_tif)) %>% #if below cap no change
    group_by(year, agency_num) %>%
    summarize(amt_to_tif = sum(amt_to_tif))
    
  
  # Update the base by adding the recovered amount to the each district's tax base
  tif_agency_cntr_updated <- tif_agency_cntr %>%
    left_join(tif_agency_amt_to_add, by = c("year", "agency_num")) %>%
    rowwise() %>%
    mutate(agency_total_eav = sum(agency_total_eav, amt_to_tif, na.rm = TRUE)) %>%
    select(-amt_to_tif) %>%
    setDT(key = c("year", "tax_code", "agency_num"))
  
  # 3. Recalculate bills with the the updated TIF distributions and tax bases
  
  # Calculate unaltered, original bills for comparison
  tif_bills <- tax_bill(2021, targ_pins)
  
  # Calculate counterfactual tax bills where the TIF does not exist
  tif_bills_cntr <- tax_bill(
    year_vec = 2021,
    pin_vec = targ_pins,
    agency_dt = tif_agency_cntr_updated,
    tif_dt = tif_dt_cntr
  )
  
  #change in taxing districts in muni only which tif is in
  taxing_dist_change <- tif_bills %>%
    select(year, pin, agency_num,
           agency_name, with_tif=final_tax,
           with_tif_rate = agency_tax_rate,
           eav) %>%
    left_join(
      tif_bills_cntr %>%
        select(year, pin, agency_num,
               agency_name, no_tif=final_tax,
               no_tif_rate = agency_tax_rate),
      by = c("year", "pin", "agency_name", "agency_num")
    ) %>%
    group_by(year, agency_num, agency_name) %>%
    summarize(med_notif = median(no_tif),
              med_tif = median(with_tif),
              total_notif = sum(no_tif),
              total_tif = sum(with_tif),
              eav = sum(eav),
              rate_notif = median(no_tif_rate),
              rate_tif = median(with_tif_rate),
              prop_cnt = n()) %>%
    filter(agency_num %in% (tif_agency_amt_to_add %>% pull(agency_num)))
  
  #taxbill change
  taxbill_changes_group <- tif_bills %>%
    group_by(year, pin, class) %>%
    summarize(with_tif = sum(final_tax)) %>%
    left_join(
      tif_bills_cntr %>%
        group_by(year, pin, class) %>%
        summarize(no_tif = sum(final_tax)),
      by = c("year", "pin", "class")
    ) %>% ungroup() %>%
    mutate(class_group = case_when(
      str_sub(class, 1, 1) == '2' ~ 'Residential',
      str_sub(class, 1, 1) %in% c('3', '5') ~ 'Commercial',
      TRUE ~ 'Other'
    )) %>%
    group_by(class_group) %>%
    summarize(med_notif = median(no_tif),
              med_tif = median(with_tif),
              mean_notif = mean(no_tif),
              mean_tif = mean(with_tif),
              prop_cnt = n())
  
  #burdened amount
  burdened_amt <- tif_agency_amt_to_add %>%
    left_join(tif_agency_cntr %>% distinct(year, agency_num, agency_name, 
                                           agency_major_type, agency_minor_type, agency_total_eav)) %>%
    mutate(burdened_pct = amt_to_tif / agency_total_eav) 
  
  #labeling
  if(length(targ_tif > 1)){
    labl_tif <- targ_tif[[1]]
  }
  
  return(
    list(
      taxing_dist_change %>% mutate(targ_tif = labl_tif),
      taxbill_changes_group %>% mutate(targ_tif = labl_tif),
      burdened_amt %>% mutate(targ_tif = labl_tif)
    )
  )
}

#since there's some issues with sub/nested tifs currently using the clerk data to get
#the full list of tifs https://gitlab.com/ccao-data-science---modeling/packages/ptaxsim/-/issues/39

clerk_tif <- 
  readxl::read_excel('ptaxsim/data-raw/tif/distribution/2021 TIF Agency Distribution Report_0.xlsx')

joined_tif <- clerk_tif %>% 
  distinct(`TIF Agency`, `TIF Name`, `TIF Total Frozen EAV`, .keep_all=TRUE) %>%
  select(`TIF Agency`, `TIF Name`)

loop_tif <- joined_tif %>% distinct(`TIF Name`) %>% pull(`TIF Name`)

library(foreach)
library(doParallel)

# calc stats by each individual tif

my.cluster <- parallel::makeCluster(
  16, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)

output <- foreach( i=loop_tif,
                   .packages=c('tidyverse', 'ptaxsim', 'data.table', 'DBI', 'RSQLite'),
                   .verbose=TRUE) %dopar% {
                     agencies <- joined_tif %>%
                       filter(`TIF Name` == {i}) %>%
                       pull(`TIF Agency`)
                     rslt <- calc_tif_stats(agencies)
                     rslt
                   }

parallel::stopCluster(cl = my.cluster)

m1 <- tibble()
m2 <- tibble()
m3 <- tibble()

for(i in 1:length(output)){
  m1 <- bind_rows(m1, output[[i]][[1]])
  m2 <- bind_rows(m2, output[[i]][[2]])
  m3 <- bind_rows(m3, output[[i]][[3]])
}

m1 %>% write_csv('all_tif_agency_sum.csv')
m2 %>% write_csv('all_tif_class_group.csv')
m3 %>% write_csv('all_tif_burdened.csv')

# calc stats by each muni

loop_muni <- muni_tif %>% distinct(name_mini) %>% pull(name_mini)

my.cluster <- parallel::makeCluster(
  16, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)

output <- foreach( i=loop_muni,
                   .packages=c('tidyverse', 'ptaxsim', 'data.table', 'DBI', 'RSQLite'),
                   .verbose=TRUE) %dopar% {
                     agencies <- muni_tif %>% distinct(name_mini, `TIF Agency`) %>%
                       filter(name_mini == {i}) %>%
                       pull(`TIF Agency`)
                     rslt <- calc_tif_stats(agencies)
                     rslt
                   }

parallel::stopCluster(cl = my.cluster)

m1 <- tibble()
m2 <- tibble()
m3 <- tibble()

for(i in 1:length(output)){
  m1 <- bind_rows(m1, output[[i]][[1]])
  m2 <- bind_rows(m2, output[[i]][[2]])
  m3 <- bind_rows(m3, output[[i]][[3]])
}

m1 %>%
  left_join(muni_tif %>% 
              distinct(targ_tif=`TIF Agency`, name_mini)) %>% write_csv('muni_agency_sum.csv')
m2 %>%
  left_join(muni_tif %>% 
              distinct(targ_tif=`TIF Agency`, name_mini)) %>% write_csv('muni_class_group.csv')
m3 %>%
  left_join(muni_tif %>% 
              distinct(targ_tif=`TIF Agency`, name_mini)) %>% write_csv('muni_burdened.csv')
