# Board of Review Analysis

This repository contains reports which analyze the quality of assessments produced by the Cook County Assessor on behalf of the Cook County Board of Review. These reports and code are subject to further changes or corrections.

## Neighborhoods

[Report here](reports/neighborhoods_analysis.html)

The Cook County Assessor issues and utilizes [860 neighborhood boundaries](https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Neighborhood-Boundaries/pcdw-pxtg) for the purpose of assessing properties. This report analyzes the quality of asssessments within and between neighborhoods.

## Exemptions

[Report here](reports/exemptions_analysis.html)

This report utilizes line item tax bills from 2006 to 2021 to calculate the value of different tax exemptions over time in different geographic areas.
 
## Pilsen

[Report here](reports/pilsen_analysis.html)

This report analyzes recent assessment quality in Pilsen and proposes a possible redelineation of the neighborhood.

[Sales Ratio study only](reports/77141.html)

## Regressivity

Sales ratios studies for Assessor mailed values are shown for Cook County and each triennial separately. Assessment quality is best evaluated in reassessment years. The quality of assessments in other years reflects appeals and market trends between reassessments.

- [Cook County Mailed](reports/county.html)
- [City Mailed](reports/city_tri.html) (reassessed in 2018 and 2021)
- [North Mailed](reports/north_tri.html) (reassessed in 2019 and 2022)
- [South Mailed](reports/south_tri.html) (reassessed in 2020)

## TIF Districts

Tax increment financing (TIF) is a "special funding tool...used to promote public and private investment" in specific areas. TIFs are funded by capturing future property value increases through freezing property values for 23 or 35 years. 

In practice, TIFs have strange impacts on the property tax system. For example, any appeals or exemptions granted to properties in a TIF which is above its initial frozen value have no impact on tax rates and **only** reduce the amount of revenue that a TIF collects. This analysis attempts to quantify the impacts of TIFs by presenting alternate (counterfactual) tax bills as if they did not exist.

[Report here](reports/tif_analysis.html)


A [recent article](https://wirepoints.org/chicagos-commercial-doom-loop-could-result-in-a-property-tax-hike-on-homeowners-as-large-as-22-wirepoints/) estimated changes in Chicago tax bills if there was a 50% decrease in downtown commercial property values. They estimated that this would lead to a 22% increase in values, but taking into account that many commercial downtown properties are in TIFs the change in median Chicago residential tax bills is about 13.5%. This is since reductions in equalized assessed value (eav) for TIFs which are above their frozen eavs lead to a reduction only in TIF revenue with no impact on the larger tax base.

[This report](reports/tifs_and_commercial.html) shows how various changes to commercial valuations impact tax bills.
