library(ptaxsim)
library(tidyverse)

# Create the DB connection with the default name expected by PTAXSIM functions
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db")


detailed_exemptions <- tbl(ptaxsim_db_conn, 'pin') %>% 
  select(pin, year, contains('exe')) %>%
  collect()

all_pins <- detailed_exemptions %>% distinct(pin) %>% pull(pin)

all_yrs <- tibble()
for(i in 2006:2021){
  print(i)
  cur_yr <- tax_bill(i, all_pins, simplify = FALSE) %>%
    group_by(year, pin, class, tax_code, av, eav, exe_total) %>% 
      summarise(across(contains('exe'), sum),
                total_tax_rate = sum(agency_tax_rate)) %>% 
    left_join(detailed_exemptions)
  all_yrs <- bind_rows(all_yrs, cur_yr)
}

all_yrs %>% write_csv('exemptions_06_21.csv')



