get_state_fips <- function(state_abbrevs = "") {
  tigris::fips_codes %>% 
    filter(state %in% state_abbrevs) %>% 
    select(state, state_code, state_name) %>% 
    unique()
}
