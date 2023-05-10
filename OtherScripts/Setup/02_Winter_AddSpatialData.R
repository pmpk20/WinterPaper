#### RELATE WP5: Winter Paper Setting up spatial parts of the data  ###############
# Script author: Dr Peter King (p.m.king@kent.ac.uk)
# Last Edited: 03/02/2023
## NOTES: This is just importing packages.
# Function of this script is to convert Winter to GB_Winter
# Note: To save time in SECTION 0-5, just import this and skip it: # Winter <-  data.frame(read.csv("Winter_dataframe_Step2.csv", encoding = "latin1"))
# I am so sorry for this script and how messy it is


# *********************************************************************************************************
#### Section 0: Setting up ####
## NOTES: This is just importing packages.
# *********************************************************************************************************


## sessionInfo()-----------------------------------------------------------------
# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
#
# Matrix products: default
#
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8    LC_MONETARY=English_United Kingdom.utf8
# [4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.utf8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] udunits2_0.13.2.1  PostcodesioR_0.3.1 geosphere_1.5-18   psych_2.2.9        dplyr_1.0.10       magrittr_2.0.3     readxl_1.4.1
# [8] here_1.0.1
#
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.9       cellranger_1.1.0 pillar_1.8.1     compiler_4.2.0   tools_4.2.0      digest_0.6.31    evaluate_0.20    lifecycle_1.0.3
# [9] tibble_3.1.8     nlme_3.1-157     lattice_0.20-45  pkgconfig_2.0.3  rlang_1.0.6      cli_3.6.0        DBI_1.1.3        rstudioapi_0.14
# [17] yaml_2.3.6       parallel_4.2.0   xfun_0.36        fastmap_1.1.0    httr_1.4.4       knitr_1.41       generics_0.1.3   vctrs_0.5.1
# [25] rprojroot_2.0.3  grid_4.2.0       tidyselect_1.2.0 glue_1.6.2       R6_2.5.1         fansi_1.0.3      rmarkdown_2.20   sp_1.6-0
# [33] htmltools_0.5.4  assertthat_0.2.1 mnormt_2.1.1     utf8_1.2.2


## Libraries here: -----------------------------------------------------------------
## Setting up libraries in order of use in the script
rm(list=ls())
library(here)
library(readxl)
library(magrittr)
library(psych)
library(dplyr)
library(geosphere)
library(PostcodesioR)
library(udunits2)
library(sf)
library(stringi)
library(stringr)
library(lubridate)
library(data.table)


# *********************************************************************************************************
#### Step One: Read in data frame with all respondents ####
# *********************************************************************************************************


Winter <-  here("OtherData","Winter_dataframe_Step1.csv") %>% fread() %>% data.frame() ## Otherwise just import these two


# *********************************************************************************************************
#### Step Two: Extract Woodland Coordinates ####
### Note: This rigmarole is necessary to clean and validate the self-entered postcodes
# *********************************************************************************************************


## Manipulating lat-lon data to be postcodes
PCs <-
  toupper(noquote(
    str_split(
      Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...,
      " ",
      simplify = TRUE
    )
  ))
Lefts <- PCs[, 1] ## Lefthand side of postcodes
Rights <- PCs[, 2] ## Right-hand side of postcodes



## Trim values down to lose leading digits
# Calculate a value for each respondent who replied with their data
# nchar counts number of characters, this is important as postareas can be both 3 or 4 digits.
# substr extracts parts of a string such as `G2` from `RG22`
# noquote removes quotation marks from data
for (i in 1:nrow(PCs)) {
  Lefts[i] <-
    (ifelse(nchar(PCs[i, ])[1] > 4, ifelse(
      nchar(PCs[i, ])[1] > 6, substr(PCs[i, ], 1, 4), substr(PCs[i, ], 1, 3)
    ), PCs[i, ]))
  Rights[i] <-
    (ifelse(
      nchar(PCs[i, ])[1] > 4,
      ifelse(nchar(PCs[i, ])[1] > 6, noquote(str_sub(PCs[i, ], 4, -1)[1]), noquote(str_sub(PCs[i, ], 3, -1)[1])),
      noquote(str_sub(PCs[i, ], 1, -1)[2])
    ))
}


## Correct for missing data
Lefts[is.na(Lefts)] <- 0
Rights[is.na(Rights)] <- 0


## Here I create a dataframe to assign values too, I use an already existing column from data for ease
Postcodes <- Winter$PC


# *********************************************************************************************************
#### Step Three: Assign correct postcode value to respondents ####
## The problem here is incomplete or erroneous postcodes which require some effort to correct
# *********************************************************************************************************


## Here I extract the county per persons' postcode
# postcode_* commands require library(postcodesio)
for (i in 1:nrow(PCs)){
  Postcodes[i] <-
    ifelse(
      postcode_validation(paste0(Lefts[i], Rights[i])) == TRUE,
      postcode_lookup(paste0(Lefts[i], Rights[i]))$postcode,
      ifelse(
        is.null(postcode_query(paste0(Lefts[i], Rights[i]))[[1]]$postcode) == TRUE,
        0,
        postcode_lookup(postcode_query(paste0(Lefts[i], Rights[i]))[[1]]$postcode)$postcode))
}


## Here I extract the county per persons' postcode
for (i in 1:nrow(PCs)) {
  Postcodes[i] <-
    ifelse(
      Postcodes[i] == 0,
      ifelse(postcode_validation(PCs[i])==FALSE,0,
             ifelse(
               is.na(postcode_lookup(PCs[i])$postcode)==TRUE,
               ifelse(
                 is.na(postcode_lookup(PCs[i])$postcode)==TRUE,
                 0,
                 postcode_lookup(PCs[i])$postcode),
               postcode_lookup(PCs[i])$postcode)),
      Postcodes[i])
}


## This corrects a few NULL issues
for (i in 1:nrow(PCs)) {
  Postcodes[i] <-
    ifelse(
      Postcodes[i] != 0,Postcodes[i],
      ifelse(
        postcode_validation(PCs[i]) == FALSE,
        0,
        ifelse(
          is.na(postcode_lookup(postcode_autocomplete(PCs[i])[[1, 1]])$postcode) !=
            TRUE,
          postcode_lookup(postcode_autocomplete(PCs[i])[[1, 1]])$postcode,

          ifelse(is.na(postcode_lookup(postcode_autocomplete(PCs[i])[[1, 1]])$postcode) ==
                   TRUE,
                 0,
                 postcode_lookup(postcode_autocomplete(PCs[i])[[1, 1]])$postcode
          ))))

}


## So many corrections it's starting to look like my thesis
for (i in 1:nrow(PCs)) {
  Postcodes[i] <-
    ifelse(Postcodes[i] != 0,
           Postcodes[i],
           ifelse(
             is.na(PCs[i]) == TRUE,
             0,
             ifelse(
               tryCatch({
                 postcode_autocomplete(PCs[i])
                 1
               }, error=function(e) 0)==0,0,
               ifelse(is.na(postcode_lookup(postcode_autocomplete(PCs[i])[[1, 1]])$postcode) ==
                        TRUE,
                      0,
                      postcode_lookup(postcode_autocomplete(PCs[i])[[1, 1]])$postcode
               ))))

}


## Final attempt to get everyones correct postcodes
for (i in 1:nrow(PCs)) {
  Postcodes[i] <-
    ifelse(Postcodes[i]!=0,Postcodes[i],ifelse(
      tryCatch({
        postcode_autocomplete(PCs[i])
        1
      }, error=function(e) 0)==0,0,ifelse(Postcodes[i]!=0,Postcodes[i],postcode_lookup(postcode_autocomplete(Lefts[i])[[1, 1]])$postcode)))}


## Update dataframe to include the correct postcodes
Winter$Postcode <- Postcodes


# *********************************************************************************************************
#### Step Four: Start working out different variables using postcodes ####
## Now we can use the complete and correct postcodes to collect location data that can help us merge data later
# *********************************************************************************************************


## PC coordinates for use in spatial clustering:
## Getting coordinates for each postcodes also helps calculate distances
for (i in 1:nrow(PCs)) {
  Winter$LonPC[i] <-
    ifelse(Winter$Postcode[i] != 0,
           postcode_lookup(Winter$Postcode[i])$longitude ,
           NA)
  Winter$LatPC[i] <-
    ifelse(Winter$Postcode[i] != 0,
           postcode_lookup(Winter$Postcode[i])$latitude ,
           NA)
}



## Distance between woodlands and house using self-reported postcodes:
for (i in 1:nrow(PCs)) {
  Winter$PostcodeDistance[i] <-
    ifelse(is.na(Winter$LonPC[i]) == TRUE,
           NA,
           ifelse(is.na(Winter$LonH[i]) == TRUE, NA, distm(
             x = c(Winter$LonPC[i], Winter$LatPC[i]),
             y = c(Winter$LonW[i], Winter$LatW[i])
           ) / 1000))
}




## Postcode county:
### Note: I am aggregating at county level here as too few observations at postcode level
for (i in 1:nrow(Winter)) {
  Winter$County2[i] <- ifelse(
    tryCatch({
      postcode_lookup(Winter$Postcode[i])$admin_district
      1
    }, error = function(e)
      0) != 0,
    postcode_lookup(Winter$Postcode[i])$admin_district,
    ifelse(
      tryCatch({
        postcode_lookup(Winter$Postcode[i])$admin_county
        1
      }, error = function(e)
        0) != 0,
      postcode_lookup(Winter$Postcode[i])$admin_county,
      0
    )
  )
}


## Postcode county:
### Note: County and County2 are the same but the function is wrong for some reason so fix later
for (i in 1:nrow(Winter)) {
  Winter$County[i] <- ifelse(
    tryCatch({
      postcode_lookup(Winter$Postcode[i])$admin_county
      1
    }, error = function(e)
      0) != 0,
    postcode_lookup(Winter$Postcode[i])$admin_county,
    ifelse(
      tryCatch({
        postcode_lookup(Winter$Postcode[i])$admin_district
        1
      }, error = function(e)
        0) != 0,
      postcode_lookup(Winter$Postcode[i])$admin_district,
      0
    )
  )
}


## Short term fix for my county code mistake. This works:
Winter$County <- (coalesce(Winter$County, Winter$County2))



## While we're here, let's get the NUTS code per region to make later merging easier.
for (i in 1:nrow(Winter)) {
  Winter$NUTS[i] <- ifelse(
    tryCatch({
      postcode_lookup(Winter$Postcode[i])$nuts_code
      1
    }, error = function(e)
      0) != 0,
    ifelse(
      is.null(postcode_lookup(Winter$Postcode[i])$nuts_code) == TRUE,
      NA,
      postcode_lookup(Winter$Postcode[i])$nuts_code
    ),
    ifelse(
      Winter$Postcode[i] == 0,
      NA,
      postcode_lookup(Winter$Postcode[i])$nuts_code
    )
  )
}



## And the County code in case it's helpful
for (i in 1:nrow(Winter)) {
  Winter$CCCode[i] <- ifelse(
    tryCatch({
      postcode_lookup(Winter$Postcode[i])$admin_county_code
      1
    }, error = function(e)
      0) != 0,
    ifelse(
      is.null(postcode_lookup(Winter$Postcode[i])$admin_county_code) == TRUE,
      NA,
      postcode_lookup(Winter$Postcode[i])$admin_county_code
    ),
    ifelse(
      Winter$Postcode[i] == 0,
      NA,
      postcode_lookup(Winter$Postcode[i])$admin_county_code
    )
  )
}




## And the specific region if I want to aggregate to that scale instead
for (i in 1:nrow(Winter)) {
  Winter$Region[i] <- ifelse(
    tryCatch({
      postcode_lookup(Winter$Postcode[i])$region
      1
    }, error = function(e)
      0) != 0,
    ifelse(
      is.null(postcode_lookup(Winter$Postcode[i])$region) == TRUE,
      NA,
      postcode_lookup(Winter$Postcode[i])$region
    ),
    ifelse(
      Winter$Postcode[i] == 0,
      NA,
      postcode_lookup(Winter$Postcode[i])$region
    )
  )
}


## Now we've done all that, export the data to save future effort
## Make sure to update data in that ISO format.
fwrite(Winter,here("OtherData","Winter_dataframe_Step2.csv"))



# *********************************************************************************************************
#### Step Five: Append to GB_Winter shapefile ####
# *********************************************************************************************************


## If you CBA for the above, just import this and skip it:
Winter <-  data.frame(fread(here("OtherData","Winter_dataframe_Step2.csv"))) ## Otherwise just import these two


## Start by reading in the new shapefile
### Available here: https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-december-2019-boundaries-uk-buc/about
GB <- st_read(here("OtherData","Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp"))
GB <- st_transform(GB,crs=4326) ## Comes in BNG so convert to LatLon



## To check if counties are in the approved GB list:
# Winter[is.na(Winter$County[GB$ctyua19nm %in% Winter$County]) != TRUE, ]
## Trim data to leave out missing or NA responses
### Investigate these later
# WinterTest <- Winter[which(!duplicated(Winter$LatW)),]
# WinterTest <- WinterTest[which(!duplicated(WinterTest$LonW)),]
# WinterTest <- WinterTest[which(!duplicated(WinterTest$LatH)),]
# WinterTest <- WinterTest[which(!duplicated(WinterTest$LonH)),]
# WinterTest2 <- WinterTest[!(is.na(WinterTest$County)) ,]
# WinterTest2 <- WinterTest2[WinterTest$County!=0 ,]
# GB_Winter <- st_as_sf(GB_Winter,coords=c("LonH","LatH"),crs=4326)
# GB_Winter <- GB_Winter[!is.na(GB_Winter$SmellWTP),]



## NOTE:
# unique(WinterTest2$County)[54] = "West Northamptonshire"
## Enter Large Missing County:
Winter$County <- ifelse(
  Winter$County == unique(Winter$County)[58],
  "Northamptonshire",
  Winter$County)


## Join county name to county name
GB_Winter <- left_join(x = GB,Winter,by=c("ctyua19nm"="County"))


# *********************************************************************************************************
#### Step Five B: Assorted additions that may be helpful ####
# *********************************************************************************************************


## Add some missing data that may be helpful for Spatio-Temporal Data:
Winter$Month <- as.numeric(substring(Winter$Recorded.Date,6,7))
Winter$Week <- week(ymd(substring(Winter$Recorded.Date,1,10)))
Winter$Day <- as.numeric(substring(Winter$Recorded.Date,9,10))


GB_Winter$Month <- as.numeric(substring(GB_Winter$Recorded.Date,6,7))
GB_Winter$Week <- week(ymd(substring(GB_Winter$Recorded.Date,1,10)))
GB_Winter$Day <- as.numeric(substring(GB_Winter$Recorded.Date,9,10))


# *********************************************************************************************************
#### Step Six: Export GB_Winter shapefile which has data per polygon ####
# *********************************************************************************************************



fwrite(Winter,here("OtherData","Winter_dataframe_Step3.csv"))
st_write(GB_Winter,"GB_Winter_Step3.gpkg",append=FALSE)


# *********************************************************************************************************
#### Step Seven: Merging With External Data ####
# *********************************************************************************************************


PopDensity <- read_excel(here("OtherData","AdditionalData.xlsx"),
                         sheet = "Pop.Density")

GDHI <- read_excel(here("OtherData","AdditionalData.xlsx"),
                   sheet = "Income")




GB_Winter_Joined <- left_join(x = GB_Winter,PopDensity,by=c("NUTS"="NUTS"))
GB_Winter_Joined <- left_join(x = GB_Winter_Joined,GDHI,by=c("NUTS"="NUTS"))
colnames(GB_Winter_Joined)[which(names(GB_Winter_Joined)==colnames(GB_Winter_Joined)[291])] <- "Density"



## Now export joined data:
## If there's more data that would be interesting to merge let me know :)
st_write(GB_Winter_Joined,"GB_Winter_Step4.gpkg",append=FALSE) ## NOTE: UPDATE DATE ON WHICH NEW VERSION CREATED



# # *********************************************************************************************************
# #### Step Eight: Adding Trees Data ####
# NO LONGER NECESSARY GIVEN WINTER_MERGESURVEYANDWTP.R
# # *********************************************************************************************************
#
#
# ## This is the gpkg of survey and external data we just made
# GB_Winter <- st_read(here("OtherData","GB_Winter_Step4.gpkg"))
#
#
# ## This is a GPKG of data I worked with in QGIS
# ## but was too stupid to save properly so it's in this format
# AdditionalData <- st_read(here("OtherData","AdditionalInfo.gpkg"))
#
#
# ## Use the same respondents
# GB_Winter_Trim <- GB_Winter[GB_Winter$ID %in% AdditionalData$ID,]
#
# ## Just combine the two data
# GB_Winter_Step5 <- cbind(GB_Winter_Trim, AdditionalData)
#
#
# ## Export and be done here
# GB_Winter_Step5 %>%
#   st_write(here("OtherData","GB_Winter_Step5.gpkg"),append=FALSE)


# *********************************************************************************************************
#### END OF SCRIPT ####
## Next Step: Run Mixed Logit Models To Estimate WTP
# *********************************************************************************************************
