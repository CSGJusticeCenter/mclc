#REQUIRES UPDATING EACH YEAR
#There are three types of special missings:
# -99 = REFUSED TO ANSWER SURVEY
##      A value of -99 is assigned for states that left data blank or submitted NAs (did not submit any data)
# -88 = MISSING DATA INTENTIONALLY
##      A value of -88 is assigned for states that submitted 0's, NA's or left data blank *intentionally*
# -77 = DATA DOESN'T EXIST
##      A value of -77 is assigned for states where parole was abolished

states99 <- c("Alaska", 
              "New Mexico")

states88 <- c("Alabama",
              "Connecticut",
              "Delaware",
              "Georgia",
              "Illinois",
              "Iowa",
              "Kentucky",
              "Maine",
              "Maryland",
              "Massachusetts",
              "Michigan",
              "Minnesota",
              "Nebraska", 
              "Nevada",
              "New Hampshire",
              "New Jersey",
              "New York",
              "North Dakota",
              "Ohio",
              "Oklahoma",
              "Pennsylvania",
              "South Carolina",
              "Texas",
              "Vermont",
              "Washington",
              "West Virginia")

states77 <- c('Delaware',
              'Maine')

#for categorizing special missings
#replaceit() function arguments:
#whichstate: as a vector, list states to apply function to
#missval: the value we want to replace the raw data with (either 0 or a special missing)
#var (MANDATORY): set to states which is the variable for state in the raw data,
#pickyear (MANDATORY): set to year which is the variable for year in the raw data,
#whichyear: default set to global variable year (2018 to most current survey year), but may list years to apply function to,
#missit = 2: default will simply change ALL values to a hardcoded value of your choice for the states/years selected,
#            whereas missit = 0 will only change 0 values to value of your choice,
#            and missit = 'NA' will only change NA values to value of your choice
replaceit <- function(whichstate, missval, var, pickyear, x, whichyear = year, missit = 2) {
  #replace 0s/NAs with special missings, or want to change values across specific states and variables
  if (missit == 0) {
    x = ifelse(x == 0 & {{var}} %in% whichstate & {{pickyear}} %in% whichyear, missval, x)
  } else if (missit == 'NA') {
    x = ifelse(is.na(x) & {{var}} %in% whichstate & {{pickyear}} %in% whichyear, missval, x)
  } else if (missit == 2) {
    x = ifelse({{var}} %in% whichstate & {{pickyear}} %in% whichyear, missval, x)
  }
  return(x)
}

######################################################################################################

#clean up missing data and incorrectly reported data
#if the lowest level of aggregation is non-missing for both probation and parole (e.g., technical violation admissions probation/parole), then sum to create total (e.g., total technical violation admissions)
#if the lowest level of aggregation is missing for either or both probation and parole (e.g., technical violation admissions probation/parole), then summed total is MISSING for total (e.g., total technical violation admissions)
adm_pop_analysis_with_bjs1 <- adm_pop_analysis_with_bjs_orig %>%
  mutate(
    #fix column type
    total_prison_admissions = as.numeric(total_prison_admissions),
    total_prison_population = as.numeric(total_prison_population),
    
    #blank out 0s - there should never be a 0 value in the data (EXCEPT IF PAROLE HAS BEEN ABOLISHED - see below)
    across(!states & !year, ~replace(.,.== 0, NA)),
    
    #ALABAMA - for POPULATION metrics, blank out supervision total, parole total, prob. total, new off. total, new off. prob., new off. parole
    across(c(total_supervision_violation_population,
             probation_violation_population,
             parole_violation_population,
             total_new_offense_violation_population,
             new_offense_probation_violation_population,
             new_offense_parole_violation_population
             ),
           replaceit, whichstate = 'Alabama', missval = NA, var = states, pickyear = year
           ),
    
    #CONNECTICUT - for ADMISSIONS metrics, summed total is MISSING for total tech./total new off.
    total_technical_violation_admissions   = case_when(
      states == 'Connecticut' ~ as.numeric(technical_probation_violation_admissions   + technical_parole_violation_admissions),
      TRUE ~ as.numeric(total_technical_violation_admissions)
      ),
    total_new_offense_violation_admissions = case_when(
      states == 'Connecticut' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
      TRUE ~ as.numeric(total_new_offense_violation_admissions)
      ),    
    
    #DELAWARE - for ADMISSIONS/POPULATION metrics, all parole variables should be 0
    across(c(parole_violation_admissions,
             technical_parole_violation_admissions,
             new_offense_parole_violation_admissions,
             parole_violation_population,
             technical_parole_violation_population,
             new_offense_parole_violation_population
             ),
           replaceit, whichstate = 'Delaware', missval = 0, var = states, pickyear = year
           ),    
    
    #GEORGIA - for ADMISSIONS metrics, summed total is MISSING for total tech./total new off.
    total_technical_violation_admissions   = case_when(
      states == 'Georgia' ~ as.numeric(technical_probation_violation_admissions   + technical_parole_violation_admissions),
      TRUE ~ as.numeric(total_technical_violation_admissions)
      ),
    total_new_offense_violation_admissions = case_when(
      states == 'Georgia' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
      TRUE ~ as.numeric(total_new_offense_violation_admissions)
      ),  
    
    #ILLINOIS - for ADMISSIONS/POPULATION metrics, summed total is MISSING for total supervision/total tech./total new off.
    total_supervision_violation_admissions = case_when(
      states == 'Illinois' ~ as.numeric(probation_violation_admissions + parole_violation_admissions),
      TRUE ~ as.numeric(total_supervision_violation_admissions)
      ),
    total_technical_violation_admissions   = case_when(
      states == 'Illinois' ~ as.numeric(technical_probation_violation_admissions + technical_parole_violation_admissions),
      TRUE ~ as.numeric(total_technical_violation_admissions)
      ),
    total_new_offense_violation_admissions = case_when(
      states == 'Illinois' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
      TRUE ~ as.numeric(total_new_offense_violation_admissions)
      ), 
    total_supervision_violation_population = case_when(
      states == 'Illinois' ~ as.numeric(probation_violation_population + parole_violation_population),
      TRUE ~ as.numeric(total_supervision_violation_population)
      ),
    total_technical_violation_population   = case_when(
      states == 'Illinois' ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
      TRUE ~ as.numeric(total_technical_violation_population)
      ),
    total_new_offense_violation_population = case_when(
      states == 'Illinois' ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
      TRUE ~ as.numeric(total_new_offense_violation_population)
      ), 
    
    #IOWA - for POPULATION metrics, for 2020/2021 only, summed total is MISSING for total supervision/total tech., and total probation set to MISSING
    total_supervision_violation_population = case_when(
      states == 'Iowa' & year %in% c(2020,2021) ~ as.numeric(probation_violation_population + parole_violation_population),
      TRUE ~ as.numeric(total_supervision_violation_population)
      ),
    total_technical_violation_population   = case_when(
      states == 'Iowa' & year %in% c(2020,2021) ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
      TRUE ~ as.numeric(total_technical_violation_population)
      ),
    across(probation_violation_population,
           replaceit, whichstate = 'Iowa', whichyear = c(2020,2021), missval = NA, var = states, pickyear = year
           ), 
    
    #KENTUCKY - for ADMISSIONS metrics, summed total is MISSING for total supervision/total new offense, and total probation set to MISSING
    total_supervision_violation_admissions = case_when(
      states == 'Kentucky' ~ as.numeric(probation_violation_admissions + parole_violation_admissions),
      TRUE ~ as.numeric(total_supervision_violation_admissions)
      ),
    total_new_offense_violation_admissions   = case_when(
      states == 'Kentucky' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
      TRUE ~ as.numeric(total_new_offense_violation_admissions)
      ),
    across(probation_violation_admissions,
           replaceit, whichstate = 'Kentucky', missval = NA, var = states, pickyear = year    
           ),
    
    #MAINE - for ADMISSIONS metrics, total new off. set to MISSING, and all parole variables set to 0
    across(c(total_new_offense_violation_admissions
             ),
           replaceit, whichstate = 'Maine', missval = NA, var = states, pickyear = year    
           ),
    across(c(parole_violation_admissions,
             technical_parole_violation_admissions,
             new_offense_parole_violation_admissions,
             parole_violation_population,
             technical_parole_violation_population,
             new_offense_parole_violation_population
             ),
    replaceit, whichstate = 'Maine', missval = 0, var = states, pickyear = year
    ),
    new_offense_probation_violation_admissions = case_when(
      states == 'Maine' ~ as.numeric(probation_violation_admissions - technical_probation_violation_admissions),
      TRUE ~ as.numeric(new_offense_probation_violation_admissions)
      ),
    
    #MASSACHUSETTS - for ADMISSIONS/POPULATION metrics, summed total is MISSING for total tech./total new off.
    total_technical_violation_admissions   = case_when(
      states == 'Massachusetts' ~ as.numeric(technical_probation_violation_admissions + technical_parole_violation_admissions),
      TRUE ~ as.numeric(total_technical_violation_admissions)
      ),
    total_new_offense_violation_admissions = case_when(
      states == 'Massachusetts' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
      TRUE ~ as.numeric(total_new_offense_violation_admissions)
      ), 
    total_technical_violation_population   = case_when(
      states == 'Massachusetts' ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
      TRUE ~ as.numeric(total_technical_violation_population)
      ),
    total_new_offense_violation_population = case_when(
      states == 'Massachusetts' ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
      TRUE ~ as.numeric(total_new_offense_violation_population)
      ), 
    
    #MICHIGAN - for POPULATION metrics, summed total is MISSING for total supervision/total tech./total new off.
    total_supervision_violation_population = case_when(
      states == 'Michigan' ~ as.numeric(probation_violation_population + parole_violation_population),
      TRUE ~ as.numeric(total_supervision_violation_population)
      ),
    total_technical_violation_population   = case_when(
      states == 'Michigan' ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
      TRUE ~ as.numeric(total_technical_violation_population)
      ),
    total_new_offense_violation_population = case_when(
      states == 'Michigan' ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
      TRUE ~ as.numeric(total_new_offense_violation_population)
      ),     
    
    #MINNESOTA - for ADMISSIONS metrics, summed total is MISSING for total tech/total new off.
    #          - for POPULATION metrics, summed total is MISSING for total supervision/total new off., and set total probation to MISSING
    total_technical_violation_admissions   = case_when(
      states == 'Minnesota' ~ as.numeric(technical_probation_violation_admissions + technical_parole_violation_admissions),
      TRUE ~ as.numeric(total_technical_violation_admissions)
      ),
    total_new_offense_violation_admissions = case_when(
      states == 'Minnesota' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
      TRUE ~ as.numeric(total_new_offense_violation_admissions)
      ),     
    total_supervision_violation_population = case_when(
      states == 'Minnesota' ~ as.numeric(probation_violation_population + parole_violation_population),
      TRUE ~ as.numeric(total_supervision_violation_population)
      ),
    total_new_offense_violation_population = case_when(
      states == 'Minnesota' ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
      TRUE ~ as.numeric(total_new_offense_violation_population)
      ),      
    across(probation_violation_population,
           replaceit, whichstate = 'Minnesota', missval = NA, var = states, pickyear = year    
           ),    
    
   #NEBRASKA - for ADMISSIONS metrics, summed total is MISSING for total tech./total new off. 
   #         - for POPULATION metrics, for 2018/2019 only, summed total is MISSING for total tech/total new off.
   total_technical_violation_admissions   = case_when(
     states == 'Nebraska' ~ as.numeric(technical_probation_violation_admissions + technical_parole_violation_admissions),
     TRUE ~ as.numeric(total_technical_violation_admissions)
     ),
   total_new_offense_violation_admissions = case_when(
     states == 'Nebraska' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
     TRUE ~ as.numeric(total_new_offense_violation_admissions)
     ),  
   total_technical_violation_population   = case_when(
     states == 'Nebraska' & year %in% c(2018,2019) ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
     TRUE ~ as.numeric(total_technical_violation_population)
     ),
   total_new_offense_violation_population = case_when(
     states == 'Nebraska' & year %in% c(2018,2019) ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
     TRUE ~ as.numeric(total_new_offense_violation_population)
     ),  
    
  #NEW HAMPSHIRE - for ADMISSIONS/POPULATION metrics, set all tech. vars to MISSING 
  across(c(total_technical_violation_admissions,
           technical_probation_violation_admissions,
           technical_parole_violation_admissions,
           total_technical_violation_population,
           technical_probation_violation_population,
           technical_parole_violation_population
           ),
         replaceit, whichstate = 'New Hampshire', missval = NA, var = states, pickyear = year    
         ),
  
  #NEW JERSEY - for ADMISSIONS metrics, summed total is MISSING for total supervision/total tech./total new off.
  total_supervision_violation_admissions = case_when(
    states == 'New Jersey' ~ as.numeric(probation_violation_admissions + parole_violation_admissions),
    TRUE ~ as.numeric(total_supervision_violation_admissions)
    ),
  total_technical_violation_admissions   = case_when(
    states == 'New Jersey' ~ as.numeric(technical_probation_violation_admissions + technical_parole_violation_admissions),
    TRUE ~ as.numeric(total_technical_violation_admissions)
    ),
  total_new_offense_violation_admissions = case_when(
    states == 'New Jersey' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
    TRUE ~ as.numeric(total_new_offense_violation_admissions)
    ),  
  
  #NEW YORK - for ADMISSIONS/POPULATION metrics, summed total is MISSING for total supervision/total tech./total new off.
  total_supervision_violation_admissions = case_when(
    states == 'New York' ~ as.numeric(probation_violation_admissions + parole_violation_admissions),
    TRUE ~ as.numeric(total_supervision_violation_admissions)
    ),
  total_technical_violation_admissions   = case_when(
    states == 'New York' ~ as.numeric(technical_probation_violation_admissions + technical_parole_violation_admissions),
    TRUE ~ as.numeric(total_technical_violation_admissions)
    ),
  total_new_offense_violation_admissions = case_when(
    states == 'New York' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
    TRUE ~ as.numeric(total_new_offense_violation_admissions)
    ),
  total_supervision_violation_population = case_when(
    states == 'New York' ~ as.numeric(probation_violation_population + parole_violation_population),
    TRUE ~ as.numeric(total_supervision_violation_population)
    ),
  total_technical_violation_population   = case_when(
    states == 'New York' ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
    TRUE ~ as.numeric(total_technical_violation_population)
    ),
  total_new_offense_violation_population = case_when(
    states == 'New York' ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
    TRUE ~ as.numeric(total_new_offense_violation_population)
    ), 
  
  #NORTH DAKOTA - for ADMISSIONS/POPULATION metrics, set all technical violation data to MISSING
  across(c(total_technical_violation_admissions,
           technical_probation_violation_admissions,
           technical_parole_violation_admissions,
           total_technical_violation_population,
           technical_probation_violation_population,
           technical_parole_violation_population
           ),
         replaceit, whichstate = 'North Dakota', missval = NA, var = states, pickyear = year    
         ),
  
  #OHIO - for ADMISSIONS/POPULATION metrics, set all technical violation data to MISSING
  across(c(total_technical_violation_admissions,
           technical_probation_violation_admissions,
           technical_parole_violation_admissions,
           total_technical_violation_population,
           technical_probation_violation_population,
           technical_parole_violation_population
           ),
         replaceit, whichstate = 'Ohio', missval = NA, var = states, pickyear = year    
         ), 
  
  #OKLAHOMA = for ADMISSIONS/POPULATION metrics, summed total is MISSING for total tech./new off.
  total_technical_violation_admissions   = case_when(
    states == 'Oklahoma' ~ as.numeric(technical_probation_violation_admissions + technical_parole_violation_admissions),
    TRUE ~ as.numeric(total_technical_violation_admissions)
    ),
  total_new_offense_violation_admissions = case_when(
    states == 'Oklahoma' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
    TRUE ~ as.numeric(total_new_offense_violation_admissions)
    ),
  total_technical_violation_population   = case_when(
    states == 'Oklahoma' ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
    TRUE ~ as.numeric(total_technical_violation_population)
    ),
  total_new_offense_violation_population = case_when(
    states == 'Oklahoma' ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
    TRUE ~ as.numeric(total_new_offense_violation_population)
    ),  
  
  #PENNSYLVANIA - for ADMISSIONS/POPULATION metrics, summed total is MISSING for total supervision/total tech./total new off.
  total_supervision_violation_admissions = case_when(
    states == 'Pennsylvania' ~ as.numeric(probation_violation_admissions + parole_violation_admissions),
    TRUE ~ as.numeric(total_supervision_violation_admissions)
    ),
  total_technical_violation_admissions   = case_when(
    states == 'Pennsylvania' ~ as.numeric(technical_probation_violation_admissions + technical_parole_violation_admissions),
    TRUE ~ as.numeric(total_technical_violation_admissions)
    ),
  total_new_offense_violation_admissions = case_when(
    states == 'Pennsylvania' ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
    TRUE ~ as.numeric(total_new_offense_violation_admissions)
    ),
  total_supervision_violation_population = case_when(
    states == 'Pennsylvania' ~ as.numeric(probation_violation_population + parole_violation_population),
    TRUE ~ as.numeric(total_supervision_violation_population)
    ),
  total_technical_violation_population   = case_when(
    states == 'Pennsylvania' ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
    TRUE ~ as.numeric(total_technical_violation_population)
    ),
  total_new_offense_violation_population = case_when(
    states == 'Pennsylvania' ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
    TRUE ~ as.numeric(total_new_offense_violation_population)
    ),   
      
  #TEXAS - for POPULATION metrics, summed total is MISSING for total tech./total new off.
  total_technical_violation_population   = case_when(
    states == 'Texas' ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
    TRUE ~ as.numeric(total_technical_violation_population)
    ),
  total_new_offense_violation_population = case_when(
    states == 'Texas' ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
    TRUE ~ as.numeric(total_new_offense_violation_population)
    ),    

  #WASHINGTON - for ADMISSIONS/POPULATION metrics, all lowest aggregations should be missing, next level aggr. should be used instead of sum
  across(c(probation_violation_admissions,
           parole_violation_admissions,
           technical_parole_violation_admissions,
           technical_probation_violation_admissions,
           new_offense_parole_violation_admissions,
           new_offense_probation_violation_admissions,
           
           probation_violation_population,
           parole_violation_population,
           technical_parole_violation_population,
           technical_probation_violation_population,
           new_offense_parole_violation_population,
           new_offense_probation_violation_population
           ),
         ~ifelse(.data$states == 'Washington', NA, .x)
         ),   
    
  #WEST VIRGINIA - for POPULATION metrics, summed total is MISSING for total supervision/total tech./total new off.
  total_supervision_violation_population = case_when(
    states == 'Washington' ~ as.numeric(probation_violation_population + parole_violation_population),
    TRUE ~ as.numeric(total_supervision_violation_population)
    ),
  total_technical_violation_population   = case_when(
    states == 'Washington' ~ as.numeric(technical_probation_violation_population + technical_parole_violation_population),
    TRUE ~ as.numeric(total_technical_violation_population)
    ),
  total_new_offense_violation_population = case_when(
    states == 'Washington' ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
    TRUE ~ as.numeric(total_new_offense_violation_population)
    )
  )

#check summing calculations for all states
check.dat <- adm_pop_analysis_with_bjs1 %>%
  select(c("states","year",
           "total_technical_violation_admissions",  "technical_probation_violation_admissions",  "technical_parole_violation_admissions",
           "total_new_offense_violation_admissions","new_offense_probation_violation_admissions","new_offense_parole_violation_admissions",
           "total_technical_violation_population",  "technical_probation_violation_population",  "technical_parole_violation_population",
           "total_new_offense_violation_population","new_offense_probation_violation_population","new_offense_parole_violation_population"
            )
         ) %>%
  mutate(flag_error.ta = ifelse(is.na(technical_probation_violation_admissions) & 
                                is.na(technical_parole_violation_admissions) &
                                !is.na(total_technical_violation_admissions), 1, 0
                                ),
         flag_error.na = ifelse(is.na(new_offense_probation_violation_admissions) & 
                                is.na(new_offense_parole_violation_admissions) &
                                !is.na(total_new_offense_violation_admissions), 1, 0
                                ),
         flag_error.tp = ifelse(is.na(technical_probation_violation_population) & 
                                is.na(technical_parole_violation_population) &
                                !is.na(total_technical_violation_population), 1, 0
                                ),
         flag_error.np = ifelse(is.na(new_offense_probation_violation_population) & 
                                is.na(new_offense_parole_violation_population) &
                                !is.na(total_new_offense_violation_population), 1, 0
                                )
         )

#check for 0's in the data, which should exist only for Maine for Parole
check.zeroes <- which(adm_pop_analysis_with_bjs1 == 0, arr.ind=TRUE)
state.zeroes <- adm_pop_analysis_with_bjs1[check.zeroes[,1],1] #states with 0s
years.zeroes <- adm_pop_analysis_with_bjs1[check.zeroes[,1],2] #years with 0s
var.zeroes   <- names(adm_pop_analysis_with_bjs1[,check.zeroes[,2]]) #variables with 0s

#create dataframe with special missing values for analysis
#There are three types of special missings:
# -99 = REFUSED TO ANSWER SURVEY
##      A value of -99 is assigned for states that left data blank or submitted NAs (did not submit any data)
# -88 = MISSING DATA INTENTIONALLY
##      A value of -88 is assigned for states that submitted 0's, NA's or left data blank *intentionally*
# -77 = DATA DOESN'T EXIST
##      A value of -77 is assigned for states were parole was abolished

#create special missings and store as dataframe
specialmissings <- adm_pop_analysis_with_bjs1 %>%
  mutate(
    #make NAs into special missings
    across(!states & !year, replaceit, missit = 'NA', whichstate = states99, missval = -99, var = states, pickyear = year),
    across(!states & !year, replaceit, missit = 'NA', whichstate = states88, missval = -88, var = states, pickyear = year),
    
    #REQUIRES UPDATING EACH YEAR
    #special cases for Maine and Delaware - change parole 0s to -77
    across(c(parole_violation_admissions,
             technical_parole_violation_admissions,
             new_offense_parole_violation_admissions,
             parole_violation_population,
             technical_parole_violation_population,
             new_offense_parole_violation_population
             ),
           replaceit, missit = 0, whichstate = states77, missval = -77, var = states, pickyear = year
           )
    )