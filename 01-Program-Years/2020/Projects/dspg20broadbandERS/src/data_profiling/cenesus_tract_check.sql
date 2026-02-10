select *, round((100.00 * (1.00 * (chr_5 + chr_6 + chr_7 + chr_8 + chr_9 + chr_10))/tot_records), 2) pct_complete
from
(
  select fips_code,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code) tot_records,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code and length(census_tract) < 5) chr_lt_5,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code and length(census_tract) = 5) chr_5,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code and length(census_tract) = 6) chr_6,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code and length(census_tract) = 7) chr_7,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code and length(census_tract) = 8) chr_8,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code and length(census_tract) = 9) chr_9,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code and length(census_tract) = 10) chr_10,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code and length(census_tract) > 10) chr_gt_10,
         (select count(*) from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = c.fips_code and (census_tract IS NULL or census_tract = '')) empty 
  from corelogic_usda.corelogic_usda_current_tax_2020_06_27 c
         where fips_code like '510%' group by fips_code
) t;