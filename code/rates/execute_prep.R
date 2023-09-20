#############################################################
## Revocation Counts and Race and Ethnicity - DATA

box::use(./ROOT
         , ./rri_tables)
source(file.path(ROOT$sp,"/code/rates/PUMS_pull.R"))
rri_tables$prep_DATA()
