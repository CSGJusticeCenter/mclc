#OUTPUT_FORMAT
#change output format to either be a word docx or html

#PARAMS
#set_title    - title of report
#yearofsurvey - latest data year collected from the MCLC survey (numeric), i.e., 2021
#filepath     - top-level file path of github clone to run this program, e.g., "C:/Users/username/mclc"

rmarkdown::render('05_multiple_imputation.Rmd',
                  #output_format = Gmisc::docx_document(),
                  output_format = "html_document",
                  output_file   = paste0("MCLCestimates_",Sys.Date(),".html"),
                  params = list(
                    filepath     = "C:/Users"
                  )
)
