#New National Variables/columns
m.imp <- adm_pop_analysis_with_bjs %>% mutate(
  overall_admissions = total_prison_admissions,
  overall_population = total_prison_population
) %>%
  select(states, year,
         overall_admissions,
         probation_violation_admissions,
         parole_violation_admissions,
         total_technical_violation_admissions,
         technical_probation_violation_admissions, 
         technical_parole_violation_admissions,
         total_new_offense_violation_admissions,
         new_offense_probation_violation_admissions,
         new_offense_parole_violation_admissions,
         
         overall_population,
         probation_violation_population,
         parole_violation_population,
         total_technical_violation_population,
         technical_probation_violation_population,
         technical_parole_violation_population,
         total_new_offense_violation_population,
         new_offense_probation_violation_population,
         new_offense_parole_violation_population
         )

# missing data for a certain feature or sample is more than 5%
pMiss <- function(x){
  sum(is.na(x))/length(x)*100
}
apply(m.imp,2,pMiss)

# m=5 refers to the number of imputed datasets
# meth='pmm' refers to the imputation method, predictive mean matching
temp_data <- mice(m.imp,
                  m     = 5,
                  maxit = 50,
                  meth  = 'pmm',
                  seed  = 500)
summary(temp_data)

# check imputed data
# each observation (first column left) within each imputed data set (first row at the top)
temp_data$imp$overall_admissions

#each individual imputed data frame (m=5)
#use this for modeling code at the state level
for (i in 1:5) {
  micedata <- complete(temp_data,i)
  assign(paste0("mice_imputed_data",i),
         micedata,
         envir = .GlobalEnv)
}

#get response columns from survey
#number of survey variables
numvar    <- 1:length(tablevals.pretty)
tablevals <- colnames(mice_imputed_data1[numvar[3]:(length(numvar)+2)])