
box::use(
    ./admin
  , ./import
  , dplyr[...]
  , tidycensus[fips_codes]
  , stringr[str_sub]
  , forcats[fct_recode, fct_explicit_na]
  , purrr[map]
)

#' Recode RACE/OFFGENERAL categories to shorter text
#'
#' @param DF NCRP data set
#'
#' @return NCRP data set with re-coded factor levels for RACE and OFFGENERAL 
refct <- function(DF){
  
  DF |> 
    mutate( # recode factors to shorter names 
        RACE       = fct_recode(RACE      , !!!admin$NCRPlev_RACE())
      , OFFGENERAL = fct_recode(OFFGENERAL, !!!admin$NCRPlev_OFFGENERAL())
    ) |> 
    mutate( # combine NA into the 'Other/Missing' categories 
        RACE       = fct_explicit_na(RACE      , na_level = admin$lev_RACE[5])
      , OFFGENERAL = fct_explicit_na(OFFGENERAL, na_level = admin$lev_OFFGENERAL[6])
    ) 
  
}


#' Add additional state id's to NCRP data
#'
#' @param DF NCRP data set that has a STATE factor variable
#' 
#' @return Same DF with ABB
addSTATEids <- function(DF){
  
  DF %>% 
    #create other state id columns: FIPS, NAME, ABB 
    rowwise() %>%
    mutate(
        FIPS    = as.factor(str_sub(STATE, 2, 3))
      , FCT_NUM = as.numeric(as.character(FIPS))
      , ABB     = unique(tidycensus::fips_codes %>% select(c(state,state_code,state_name)))[["state"]][match(FIPS, unique(tidycensus::fips_codes %>% select(c(state,state_code,state_name)))[["state_code"]])]
      , STATE   = unique(tidycensus::fips_codes %>% select(c(state,state_code,state_name)))[["state_name"]][match(FIPS, unique(tidycensus::fips_codes %>% select(c(state,state_code,state_name)))[["state_code"]])]
    ) %>% 
    ungroup() %>% 
    #make state id columns the same factor levels (based on FIPS)
    mutate_at(vars(ABB, FIPS, STATE), ~forcats::fct_reorder(factor(.), FCT_NUM)) %>% 
    #remove numeric column and fips column 
    select(-FCT_NUM) %>% 
    #put id's column at the beginning 
    select(any_of(admin$idcols), everything())
  
}


#' Title
#'
#' @param DF 
#'
#' @return
#' @export
#'
#' @examples
admtype <- function(data = "A"){
  
  if (data == "A"){ RAW <- import$NCRP_A() }
  if (data == "N"){ RAW <- import$NCRP_N() }
  
  FILTERDF <- RAW %>% 
    filter(RPTYEAR >= 2015) %>% 
    refct() %>% 
    mutate(ADMTYPE = fct_recode(     ADMTYPE, !!!admin$NCRPlev_ADMTYPE()) ) %>% 
    mutate(ADMTYPE = fct_explicit_na(ADMTYPE, na_level = admin$lev_ADMTYPE[4]) ) 
  
  exgrid <- expand.grid(
      STATE      = unique(FILTERDF$STATE)
    , RPTYEAR    = range(FILTERDF$RPTYEAR)[1]:range(FILTERDF$RPTYEAR)[2]
    , RACE       = unique(FILTERDF$RACE)
    , OFFGENERAL = unique(FILTERDF$OFFGENERAL)
    , ADMTYPE    = unique(FILTERDF$ADMTYPE)
  ) %>% as_tibble()
  
  
  cs_list <- list(
      "ORA" = FILTERDF
  )
  
  var_list <- list(
     "ORA" = c("STATE", "RPTYEAR", "OFFGENERAL", "RACE", "ADMTYPE")
  )
  
  
  out <- purrr::map2(
    cs_list, var_list
    , ~.x %>% 
      count(across(all_of(.y))) %>% 
      rename("REVCNT" = n) %>% 
      full_join(., distinct(exgrid, across(all_of(.y))), by = .y) %>% 
      arrange(across(all_of(.y))) %>% 
      addSTATEids() %>% 
      as_tibble() 
    
  )
  
  return(out)
  
}



#' Prepare NCRP data
#'
#' @param data 
#'
#' @return list of different cross section fo counts 
#' @export
prep <- function(data = "A"){
  
  if (data == "A"){ RAW <- import$NCRP_A() }
  if (data == "N"){ RAW <- import$NCRP_N() }
  
  
  admin$mylog("Start - NCRP prep")
  
  admin$mylog("NCPR prep - Filter data to include revocations and 2015+ and recode RACE")
  FILTERDF <- RAW %>% 
    #filter down to revocations from 2015+ 
    filter(ADMTYPE == "(2) Parole return/revocation", RPTYEAR >= 2015) %>% 
    #re-factor RACE and OFFGENERAL cateogires 
    refct() 
  
  
  exgrid_OR <- expand.grid(
      STATE      = factor(levels(FILTERDF$STATE), levels = levels(FILTERDF$STATE))
    , RPTYEAR    = range(FILTERDF$RPTYEAR)[1]:range(FILTERDF$RPTYEAR)[2]
    , OFFGENERAL = factor(levels(FILTERDF$OFFGENERAL)[1:4], levels = levels(FILTERDF$OFFGENERAL)[1:4])
    , RACE       = factor(levels(FILTERDF$RACE)[1:3], levels = levels(FILTERDF$RACE)[1:3])
  ) %>% as_tibble()
  
  admin$mylog("NCRP prep - Create different cross sections by OFFGENERAL and RACE")

  cs_list <- list(
      "OR" = FILTERDF
    , "O"  = FILTERDF
    , "R"  = FILTERDF
    , "t"  = FILTERDF
  )
  
  var_list <- list(
      "OR" = c("STATE", "RPTYEAR", "OFFGENERAL", "RACE")
    , "O"  = c("STATE", "RPTYEAR", "OFFGENERAL"        )
    , "R"  = c("STATE", "RPTYEAR",               "RACE")
    , "t"  = c("STATE", "RPTYEAR"                      )
  )
  
  
  out <- purrr::map2(
    cs_list, var_list
    , ~.x %>% 
      count(across(all_of(.y))) %>% 
      rename("REVCNT" = n) %>% 
      full_join(., distinct(exgrid_OR, across(all_of(.y))), by = .y) %>% 
      arrange(across(all_of(.y))) %>% 
      addSTATEids() %>% 
      mutate(
          SUPPRESS = ifelse(!is.na(REVCNT) & REVCNT < 5, 1, 0)
        , S_REVCNT = ifelse(!is.na(REVCNT) & REVCNT < 5, 5, REVCNT)
      )

  )
  
  admin$mylog("End   - NCRP prep")
  
  return(out)
  
}


#' Prepare NCRP data
#'
#' @param data 
#'
#' @return list of different cross section fo counts 
#' @export
analytical <- function(data = "A"){
  
  if (data == "A"){ RAW <- import$NCRP_A() }
  if (data == "N"){ RAW <- import$NCRP_N() }
  
  
  admin$mylog("Start - NCRP prep")
  
  admin$mylog("NCPR prep - Filter 2015+ and recode RACE, OFFGEN, ADMTYPE")
  FILTERDF <- RAW %>% 
    filter(RPTYEAR >= 2015) %>% 
    refct() %>% 
    mutate(ADMTYPE = fct_recode(     ADMTYPE, !!!admin$NCRPlev_ADMTYPE()) ) %>% 
    mutate(ADMTYPE = fct_explicit_na(ADMTYPE, na_level = admin$lev_ADMTYPE[4]) ) 
  
  
  exgrid <- expand.grid(
      STATE      = unique(FILTERDF$STATE)
    , RPTYEAR    = range(FILTERDF$RPTYEAR)[1]:range(FILTERDF$RPTYEAR)[2]
    , RACE       = unique(FILTERDF$RACE)
    , OFFGENERAL = unique(FILTERDF$OFFGENERAL)
    , ADMTYPE    = unique(FILTERDF$ADMTYPE)
  ) %>% as_tibble()
  
  admin$mylog("NCRP prep - Create diffrent cross sections by OFFGENERAL and RACE")
  
  cs_list <- list(
      "ORA"= FILTERDF
    , "OR" = FILTERDF %>% filter(ADMTYPE == "Revocation")
    , "O"  = FILTERDF %>% filter(ADMTYPE == "Revocation")
    , "R"  = FILTERDF %>% filter(ADMTYPE == "Revocation")
    , "t"  = FILTERDF %>% filter(ADMTYPE == "Revocation")
  )
  
  var_list <- list(
      "ORA"= c("STATE", "RPTYEAR", "OFFGENERAL", "RACE", "ADMTYPE")
    , "OR" = c("STATE", "RPTYEAR", "OFFGENERAL", "RACE")
    , "O"  = c("STATE", "RPTYEAR", "OFFGENERAL"        )
    , "R"  = c("STATE", "RPTYEAR",               "RACE")
    , "t"  = c("STATE", "RPTYEAR"                      )
  )
  
  
  out <- purrr::map2(
    cs_list, var_list
    , ~.x %>% 
      count(across(all_of(.y))) %>% 
      rename("REVCNT" = n) %>% 
      full_join(., distinct(exgrid, across(all_of(.y))), by = .y) %>% 
      arrange(across(all_of(.y))) %>% 
      addSTATEids() %>% 
      mutate(
        SUPPRESS = ifelse(!is.na(REVCNT) & REVCNT < 5, 1, 0)
        , S_REVCNT = ifelse(!is.na(REVCNT) & REVCNT < 5, 5, REVCNT)
      )
    
  )
  
  admin$mylog("End   - NCRP prep")
  
  return(out)
  
}

