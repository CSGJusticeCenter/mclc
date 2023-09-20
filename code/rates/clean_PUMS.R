box::use(
    ./admin
  , dplyr[...]
  , tidycensus[fips_codes]
  , tidyr[...]
)




#' prep PUMS data 
#'
#' @return
#' @export
#'
#' @examples
prep <- function(){
  
  admin$mylog("Start - PUMS prep")
  
  
  cs_R <- readRDS(file.path(admin$sp_data_raw, "PUMS/PUMS_2015to2019.RDS")) %>% 
    select(
        RPTYEAR = year
      , RACE    = ncrp_race
      , POPEST  = n
      , STATE   = state
    ) %>% 
    # create 2020 column, use 2019 data (PUMS not available for 2020)
    pivot_wider(names_from = RPTYEAR, values_from = POPEST) %>% 
    mutate(`2020` = `2019`) %>% 
    pivot_longer(cols = -c(RACE, STATE), names_to = "RPTYEAR", values_to = "POPEST") %>% 
    mutate(RPTYEAR = as.numeric(RPTYEAR)) %>% 
    #  add factoring to race and source of population data
    mutate(
        RACE = factor(RACE, levels = admin$lev_RACE)
      , POPTYPE = "PUMS"
    ) %>% 
    # add state informaiton 
    mutate(
        FIPS = unique(tidycensus::fips_codes %>% select(c(state,state_code,state_name)))[["state_code"]][match(STATE, unique(tidycensus::fips_codes %>% select(c(state,state_code,state_name)))[["state_name"]])]
      , ABB  = unique(tidycensus::fips_codes %>% select(c(state,state_code,state_name)))[["state"]][match(STATE, unique(tidycensus::fips_codes %>% select(c(state,state_code,state_name)))[["state_name"]])]
      , FCT_NUM = as.numeric(FIPS)
    ) %>% 
    mutate_at(vars(STATE, ABB, FIPS), ~forcats::fct_reorder(factor(.), FCT_NUM)) %>% 
    select(all_of(admin$idcols), RPTYEAR, RACE, POPEST, POPTYPE) 
  
  
  cs_t <- cs_R %>% 
    group_by(STATE, FIPS, ABB, RPTYEAR, POPTYPE) %>%
    summarise(POPEST = sum(POPEST), .groups = "keep") %>% 
    ungroup()
  
  
  out <- list(
      "R"  = cs_R  #cross section by RACE
    , "t"  = cs_t  #no cross section, total by STATE/RPTYEAR 
  )
  
  admin$mylog("End   - PUMS prep")
  return(out)
  
  
  
}


