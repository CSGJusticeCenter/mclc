# Supervsion Violations and Their Impact on Incarceration

## Project overview  
In 2018, The Council of State Governments (CSG) Justice Center sent out an annual survey to corrections departments in all 50 states to collect data on the impact of supervision violations on prison admissions and populations. The study found that in 2017, probation and parole violations made up as much as 45 percent of state prison admissions nationwide, with wide variation across states. With continued partnership with the Correctional Leaders Association and support from Arnold Ventures, the CSG Justice Center will be collecting and reporting these numbers annually through 2025. 

In September 2022, the CSG Justice Center sent the [Supervision Violations and Their Impact on Incarceration](https://projects.csgjusticecenter.org/supervision-violations-impact-on-incarceration/methodology/) survey (created in Google Sheets) to the departments of corrections of all 50 states. 

Respondents were asked to provide aggregate counts of the number of people admitted to or in prison on a given day across numerous categories for calendar years 2018, 2019, 2020, and 2021. Respondents were given the option to provide the data according to fiscal year if that is the standard reporting method for the state and indicate if so.  

The categories requested include: 

* *Total admissions/population*: The number of people admitted to or in prison.  

* *Total violation admissions/population*: The number of people admitted to or in prison because of a violation of supervision conditions.  

* *Total probation violation admissions/population*: The number of people admitted to or in prison because of a violation of probation. This includes people admitted to or in prison because of new offense violations and technical violations of probation.  

* *New offense probation violation admissions/population*: The number of people admitted to or in prison because of a new offense violation of probation.  

* *Technical probation violation admissions/population*: The number of people admitted to or in prison because of a technical violation of probation.  

* *Total parole violation admissions/population*: The number of people admitted to or in prison because of a violation of probation. This includes people admitted to or in prison because of new offense violations and technical violations of parole.  

* *New offense parole violation admissions/population*: The number of people admitted to or in prison because of a new offense violation of parole.  

* *Technical parole violation admissions/population*: The number of people admitted to or in prison because of a technical violation of parole.  

* *Cost*: Average operating cost per person per day across all prison facilities.   

Where available, data provided in the survey was supplemented with publicly available data or data that we routinely collect for other purposes. We note on each [state’s page](https://projects.csgjusticecenter.org/supervision-violations-impact-on-incarceration/report/#statedashboard) when we used publicly available data.  

Additionally, Racial and Ethnic disparities in prison admissions for parole revocations and in populations incarcerated for parole revocations are reported as relative rate indices (white reference group) for all states where data is publicly available. Disparties in race and ethnicity are reported as total disparities and the portion of disparities attributable to parole revocations. We note on each [state’s page](https://projects.csgjusticecenter.org/supervision-violations-impact-on-incarceration/report/#statedashboard) how the relative rate indices are calculated, and when we are unable to calculate racial and ethnic disparities.  

### Caution on Comparing States: 

States define admissions and populations due to supervision violations differently:   

* Some states reported that they include quick dips and supervision sanctions in their admissions numbers, while others do not.  
* Some states reported that they classify new felony offenses as technical violations, while others do not.  
* States do not necessarily have the infrastructure or institutional memory to pull data in a consistent way over time.  
* When provided the opportunity, states revised data that they previously submitted in past years.  
* States may continue to revise their survey responses during future validation processes.  

For more information about [Supervision Violations and Their Impact on Incarceration](https://projects.csgjusticecenter.org/supervision-violations-impact-on-incarceration/), visit our website or [OSF project registration](https://osf.io/f74d6/).   

## Repository  
This repository contains code that conducts three operations:

1) Formats data for imputation.
2) Produces national estimates and state-by-state costs report.
3) Produces rates and Relative Rate Indices RRIs for racial and ethnic disparities.

### Folder structure  

```
    |-- code 
      |-- survey
          |-- 03_clean.R               # Format data like 2021 survey data, replace data with BJS numbers
          |-- execute_report.R         # create final results for national report - executes 05_multiple_imputation.R
          |-- 05_multiple_imputation.R 
              |-- sources the following programs in this order: 03_clean.R, special_missings.R, MImp1.R, MImp3.R, MImp3.R, Costs.R
      |-- rates
          |-- ROOT.R                   # set root folder path
          |-- admin.R                  # prep for calculating rates
          |-- assignflags.R            # finalize tables of RRIs/Rates/Counts and calculation language
          |-- calc.R                   # runs clean_*.R programs to calculate rates and RRIs
          |-- clean_APS.R              # Annual Probation Survey data cleaning
          |-- clean_NCRP.R             # National Corrections Reporting Program data cleaning
          |-- clean_PUMS.R             # Census Public Use Microdata Sample data cleaning
          |-- clean_SC.R               # Census Annual Resident Population Estimates for 6 Race Groups data cleaning
          |-- import.R                 # import public data  
          |-- PUMS_pull.R              # pull Census PUMS 2015-2019 data   
    |-- data
      |-- analysis
      |-- raw
        |--BJS
        |--NCRP
        |--notes
        |--PUMS
        |--SC-EST
```

## Data  
Various data sources were used in reporting in addition to the survey data collected. The survey data spans from 2018 to 2021. Public data sources include:

* Bureau of Justice Statistics Annual Probation Survey (APS)
* Bureau of Justice Statistics National Corrections Reporting Program (NCRP)
* Census Public Use Microdata Sample (PUMS)
* Census Annual Resident Population Estimates for 6 Race Groups (SC-EST)

```
    |-- data
      |-- Comparison_MCLC_and_BJS_data_v1.xlsx # file comparing MCLC reporting and BJS data reporting, used for producing national estimates/state-by-state costs
      |-- mclc_pre_data_2022.xlsx              # pre-cleaned MCLC survey data reported by DOCs, used for producing national estimates/state-by-state costs
      |-- analysis
      |-- raw   
        |--BJS
          |-- AParS
            |-- apars_idyear.csv               # file used to run rates programming including file paths and file names
            |-- README.txt                     # see this README for descriptions of AParS folder contents
            |-- ICPSR_34382-V1                 # DS0001 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/34382
            |-- ICPSR_34718-V1                 # DS0001 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/34718
            |-- ICPSR_35257-V1                 # DS0001 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/35257
            |-- ICPSR_35629-V1                 # DS0001 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/35629
            |-- ICPSR_36320-V1                 # DS0001 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/36320
            |-- ICPSR_36619-V1                 # DS0001 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/36619
            |-- ICPSR_37441-V1                 # DS0001 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/37441
            |-- ICPSR_37471-V1                 # DS0001 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/37471
            |-- ICPSR_38058-V1                 # DS0001 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/38058
        |--NCRP
          |-- README.txt                       # see this README for descriptions of NCRP folder contents
          |-- ICPSR_38492-V1                   # DS0002/DS0003/DS0004 R data must be downloaded manually: https://www.icpsr.umich.edu/web/NACJD/studies/38492/datadocumentation#
        |--notes
          |-- revcnt_notes.csv                 # state notes on NCRP data reporting
        |--PUMS                                # Location for Census PUMS 2015-2019 data, pulled via PUMS_pull.R program
        |--SC-EST                              # CSV data must be downloaded manually: https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/asrh/SC-EST2020-ALLDATA6.csv
          |-- README.txt                       # see this README for descriptions of SC-EST folder contents
```  

## Guide to run survey results
In order to successfully produce national estimates and state-by-state costs, survey programs (in the `/code/survey/` folder) are run by executing the `execute_report.R` program. `execute_report.R` accepts parameters (set to a default for the current survey year) as well as for setting output file name and format.

The `filepath` parameter must be changed to your top-level, personal file path of this GitHub clone to successfully run. Additional guidance may be found in the comments of `execute_report.R`.

## Guide to produce RRIs/Rates
In order to successfully produce the RRIs and Rates (in addition to other supplemental information including counts to calculate these rates), all relevant data must be downloaded first, otherwise these programs will not execute successfully:

* Download the [ICPSR_34382 DS0001 R data](https://www.icpsr.umich.edu/web/NACJD/studies/34382/datadocumentation) manually into the `/data/raw/BJS/AParS/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.
* Download the [ICPSR_34718 DS0001 R data](https://www.icpsr.umich.edu/web/NACJD/studies/34718/datadocumentation) manually into the `/data/raw/BJS/AParS/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.
* Download the [ICPSR_35257 DS0001 R data](https://www.icpsr.umich.edu/web/NACJD/studies/35257/datadocumentation) manually into the `/data/raw/BJS/AParS/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.
* Download the [ICPSR_35629 DS0001 R data](https://www.icpsr.umich.edu/web/NACJD/studies/35629/datadocumentation) manually into the `/data/raw/BJS/AParS/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.
* Download the [ICPSR_36320 DS0001 R data](https://www.icpsr.umich.edu/web/NACJD/studies/36320/datadocumentation) manually into the `/data/raw/BJS/AParS/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.
* Download the [ICPSR_36619 DS0001 R data](https://www.icpsr.umich.edu/web/NACJD/studies/36619/datadocumentation) manually into the `/data/raw/BJS/AParS/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.
* Download the [ICPSR_37441 DS0001 R data](https://www.icpsr.umich.edu/web/NACJD/studies/37441/datadocumentation) manually into the `/data/raw/BJS/AParS/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.
* Download the [ICPSR_37471 DS0001 R data](https://www.icpsr.umich.edu/web/NACJD/studies/37471/datadocumentation) manually into the `/data/raw/BJS/AParS/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.
* Download the [ICPSR_38058 DS0001 R data](https://www.icpsr.umich.edu/web/NACJD/studies/38058/datadocumentation) manually into the `/data/raw/BJS/AParS/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.

* Download the [ICPSR 38492 DS0002/DS0003/DS0004 R data](https://www.icpsr.umich.edu/web/NACJD/studies/38492/datadocumentation#) manually into the `/data/raw/NCRP/` folder. The data must be downloaded as an R file type `.rda`. You must agree to the terms of use and register for a MyData account to download these data.

* Download the [SC-EST2020-ALLDATA6 CSV data](https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/asrh/SC-EST2020-ALLDATA6.csv) manually into the `/data/raw/SC-EST/` folder. The data must be downloaded as a CSV file type `.csv`.

Next, the `sp` value in `ROOT.R` must be changed to your top-level, personal file path of this GitHub clone to successfully run.

Finally, all rates programs (in the `/code/rates/` folder) are run by executing the `execute_prep.R` program.