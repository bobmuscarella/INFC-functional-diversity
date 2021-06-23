##%######################################################%##
#                                                          #
####                  Load R packages                   ####
#                                                          #
##%######################################################%##
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('raster','sp','readxl', 'ncdf4', 'readr', 'writexl','dplyr', 'data,table')
ipak(packages)

##%######################################################%##
#                                                          #
### Load INFC plot data and all the attached variables  ####
#                                                          #
##%######################################################%##

#So in the first R-code that I uploaded, there is a big part of the code that can be left out
#Everything before the trait data, is to match environmental and climate classification to plot coordinates
#This will always stay the same, so I did this and extracted this as PlotData
#In this way we don´t need to go back to Dropbox and we can do everything throug GitHub
#Path to find the right dataset: INFC-functional-diversity/DATA/PlotData.xlsx


##%######################################################%##
#                                                          #
####                  Load Trait data                   ####
#                                                          #
##%######################################################%##
# path to find the right dataset: INFC-functional-diversity/DATA/TraitDataFrame.xlsx

##%######################################################%##
#                                                          #
####              Load Alive trees data                 ####
#                                                          #
##%######################################################%##
# path to find the right dataset: INFC-functional-diversity/DATA/RAW/infc05_apv/t2_05_apv.csv

### Add forest codes
AliveTrees1$Forestcode <- quantiF3$codcfor[match(AliveTrees1$idpunto, quantiF3$idpunto)]
AliveTrees2$Forestcode <- quantiF3$codcfor[match(AliveTrees2$idpunto, quantiF3$idpunto)]

#we can replace: quantiF3$codcfor[match(AliveTrees1$idpunto, quantiF3$idpunto)]
# with the Plotdataset that I created, so same pathway as I mentioned before
# It conains Codcfor as well


### After this you should be able to perform the could without uploading something
### Sorry for not providing the correct links, but it was not working out for me




