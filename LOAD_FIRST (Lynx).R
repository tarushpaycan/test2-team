################# ~~ READ ME ~~ #########################
# This script loads libraries and everything you will need for the MA sections
# Notes to run file: 
# 1. Change 'wd' to location you will want to save MA output
# 2. Change endDate, endDateBQ to last date of observational year (Eg: for 2022, dates are "2022-12-31")
# 3. To run on a Mac, comment out windowsFonts and uncomment the last two commands for the font
#########################################################

# new branch may 29 test change
# Initialize
gc()
rm(list=ls())


# Auto-load all libraries required
## Specify list of packages used in code
usedPackagesList <- c("bigrquery", "DBI", "stringr", "ggplot2", "dplyr", "gghighlight",
                      "scales", "extrafont", "zoo", "tidyverse", "gridExtra", "grid",
                      "lubridate", "ggrepel", "tidyr", "readr", "quantmod", "Quandl",
                      "data.table", "bizdays", "bdscale", "anytime", "statcanR",
                      "valet", "sqldf", "httpuv");

## check to see if required packages are installed.
## if not install and load them them
## else load them
CheckPagagesInstalled <- lapply(
  usedPackagesList,
  FUN = function(x){
    if(!require(x,character.only = TRUE)){
      install.packages(x, dependencies = TRUE);
      require(x,character.only = TRUE)
    }
  }
)

##List all loaded packages
search()

# Packages required - uncomment for first run
#install.packages("anytime")                        ## Handles some unrecognized dates 
#install.packages("devtools")                       ## Download DevTools
#devtools::install_github("warint/statcanR")        ## Download StatCan data tables
#devtools::install_github("runkelcorey/valet")      ## Download Bank of Canada data tables

### UPDATE THIS
# high-level wd for system-levels plots etc.
wd = "C:/Users/tgupta/Desktop/Projects/Member Analytics/MA_2022_Q4/"
setwd(wd)

### UPDATE THIS (next year i guess)
# literally everything hinges on these dates 
endDateBQ <- "'2022-12-31'"
endDate <- "2022-12-31"

# custom colors for plotting
drk_red <- '#f9423a'
mid_red <- '#f9423a'
lineGrey <- '#4b4f54'
drk_blue <- '#69b3e7'
twoColour <- c('#f9423a', '#69b3e7')
threeColour <- c('#f9423a', '#69b3e7', '#85b09a')
fourColour <- c('#f9423a', '#69b3e7', '#85b09a','#b7b09c')
better_four <- c('#f9423a', '#69b3e7', '#85b09a','#87189d')

# custom ggplot theme elements
ma_theme <- function() {
  
  font <- "Roboto-Light"
  
  fontSize <- 11
  
  ggplot2::theme(plot.title = element_text(family=font,
                                           size=fontSize+2,
                                           face='bold',
                                           color='#4b4f54'),
                 
                 plot.subtitle = element_text(family=font,
                                              size=fontSize,
                                              color='#4b4f54'),
                 
                 legend.position ='top',
                 
                 legend.justification = 'left',
                 
                 legend.title=element_blank(),
                 
                 legend.text = element_text(family=font,
                                            size=fontSize,
                                            color='#4b4f54'),
                 
                 axis.title = element_text(family=font,
                                           size=fontSize,
                                           color='#4b4f54'),
                 
                 axis.text = element_text(family=font,
                                          size=fontSize,
                                          color='#4b4f54'),
                 
                 plot.caption = element_text(family=font,
                                             size=10,
                                             color='#4b4f54',
                                             hjust=1),
                 
                 panel.background = element_rect(fill='transparent'),
                 
                 strip.background = element_blank(),
                 strip.placement = 'outside',
                 strip.text = element_text(size=11, family='Roboto-Light', color='#4b4f54', face='bold', hjust = 0)
  )
  
}

# create BigQuery connection
options(scipen=20) #for bigquery
bQconnection <- dbConnect(
  bigrquery::bigquery(),
  project = "prod-data-storage-prj",
  dataset = "lvts_boc",
  billing = "acs-research-prj"
)

bQconnection

# list of banks for test rollout
# Uses FI lists below for Lynx and ACSS separately
## deprecated # testBanks <- c('Laurentian Bank','National Bank','BMO','Desjardins','CIBC','Central 1','HSBC','Scotiabank','RBC','TD Bank','ATB Financial','Bank of Canada', 'Payments Canada', 'Peoples Trust Company')
testBanks_hvps <- c('ATB Financial','Laurentian Bank','National Bank','BNP Paribas','Bank of America','BMO','Desjardins','CIBC','Citibank','Central 1','HSBC','Scotiabank','RBC','State Street','TD Bank','ICICI Bank Canada','ING','Manulife')
testBanks_acss <- c('Laurentian Bank','National Bank','BMO','Desjardins','CIBC','Central 1','HSBC','Scotiabank','RBC','TD Bank','ATB Financial','Bank of Canada', 'Payments Canada', 'Peoples Trust Company')


# renaming all fis for plots

renameFI <- data.frame(
  irn = c('ATBR','BLCM','BNDC','BNPA','BOFA','BOFM','FCDQ','CIBC', 'CITI','CUCX', 'HSBC','NOSC','ROYC','SBOS','TDOM','ICIC','INGB','MCBT'),
  fi = c('ATB Financial','Laurentian Bank','National Bank','BNP Paribas','Bank of America','BMO','Desjardins','CIBC','Citibank','Central 1','HSBC','Scotiabank','RBC','State Street','TD Bank','ICICI Bank Canada','ING','Manulife')
)

renameFI_ACSS <- data.frame(
  irn = c('Banque Laurentienne du Canada','Banque Nationale du Canada','Bank of Montreal','Fede caisses Desjardins','Canadian Imperial Bank of Commerce','Central 1 Credit Union','HSBC Bank Canada','Bank of Nova Scotia','Royal Bank of Canada','Toronto-Dominion Bank','ATB Financial','Bank of Canada', 'Payments Canada', 'Peoples Trust Company'),
  fi = c('Laurentian Bank','National Bank','BMO','Desjardins','CIBC','Central 1','HSBC','Scotiabank','RBC','TD Bank','ATB Financial','Bank of Canada', 'Payments Canada', 'Peoples Trust Company')
)

# you will need to create local folders for each bank with the exact name in renameFI, ie a folder named 'Bank of America','TD Bank', etc, in wherever you have set your working directory
# TG, 2023: Added in code to check and create local folders specified in note above, for both Lynx and ACSS
for (bank in testBanks_hvps)
  if (file.exists(bank)) {
  } else {
    dir.create(file.path(wd, bank))
  }

for (bank in testBanks_acss)
  if (file.exists(bank)) {
  } else {
    dir.create(file.path(wd, bank))
  }


windowsFonts("Roboto-Light" = windowsFont("Roboto-Light"))
# windowsFonts does not work on Mac, suggested change below
## TG, 2023:  Mac import of Roboto
#font_add_google("Roboto", "roboto")
#showtext_auto()