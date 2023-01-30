#### RELATE WP5: Winter Paper Setting up data for analysis  ###############
# Script author: Dr Peter King (p.m.king@kent.ac.uk)
# Last Edited: 27/01/2023
# You only need to run this if converting from Winter_All_BothBlocks_PK1806.xlsx to Winter_dataframe.csv
# Originally written in 2021 and no time to significantly update so code works but is inelegant.
# Caution: 1000 lines and very repetitive


#----------------------------------------------------------------------------------------------------------
#### Section 0: Setting up ####
## NOTES: This is just importing packages.
#----------------------------------------------------------------------------------------------------------



## Setting up libraries in order of use in the script
library(here)
library(readxl)
library(magrittr)
library(psych)
library(dplyr)
library(geosphere)
library(PostcodesioR)
library(udunits2)


## Setting working directory
# here()
## Yes, I'm using here() for all imports and exports



#----------------------------------------------------------------------------------------------------------
#### Section 1: Data Importing ####
## NOTES: This imports  "Winter_SurveyData.xlsx" which is the survey data from qualtrics
#----------------------------------------------------------------------------------------------------------



## As the questions are in the top two rows, I use this method to import both as column names
(headers <- sapply( read_xlsx(here("OtherData","Winter_SurveyData.xlsx"),n_max=1),paste)) ## Here I import and concatenate the top two header rows from the data.
Winter <- data.frame(read_xlsx(here("OtherData","Winter_SurveyData.xlsx"),sheet = 1,skip = 2,col_names = headers)) ## Import all the data and add the top two rows as headers.
attach(Winter) ## just makes it easier to refer to variables.


## Trim to only completed responses
Winter <- Winter[Winter$Finished=="True",] # Get only complete responses/
Winter$Season <-0 ## Set to zero for later seasons



#----------------------------------------------------------------------------------------------------------
#### Section 2: Coding Variables ####
## Note: This should dummy/categorical code all explanatory variables from the survey.
## Just want to say I really hate hard-coding variables BUT in many cases in this script it is
### easier to read with hard-coding
#----------------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------------
## Start with BIOWELL responses first:
#----------------------------------------------------------------------------------------------------------


BioWell <- data.frame(Winter[c(52:136)]) ## Subset the winter data to only have biowell responses
Factors2 <- (fa(r=BioWell,11,rotate="none",fm="pa")) ## Re-estimate with fewer factors
Scores <- data.frame(Factors2$scores) ## The scores are respondent-level values of each of the 11 factors
Winter <- cbind(Winter,Scores) ## These are the factors you can cbind() to the data and include in models
# Winter <- Winter[!is.na(Winter$PA1),] ## Drop incomplete or missing data


## This code calculates each respondents mean for each `stem` question (17 in total).
## This is helpful to have one value per person per question which can then be used to calculate one `score` per person.
BioWellScores <- cbind("Encountering.the.living.things"=BioWell %>% select(starts_with("Encountering.the.living.things")) %>% rowMeans(),
                       "The.number.of.living.things"=BioWell %>% select(starts_with("The.number.of.living.things")) %>% rowMeans(),
                       "The.variety.of.living.things"=BioWell %>% select(starts_with("The.variety.of.living.things")) %>% rowMeans(),
                       "The.interactions.between.plants"=BioWell %>% select(starts_with("The.interactions.between.plants")) %>% rowMeans(),
                       "The.living.processes"=BioWell %>% select(starts_with("The.living.processes")) %>% rowMeans(),
                       "The.variety.of.sounds"=BioWell %>% select(starts_with("The.variety.of.sounds")) %>% rowMeans(),
                       "The.distinctive.sounds"=BioWell %>% select(starts_with("The.distinctive.sounds")) %>% rowMeans(),
                       "The.vivid.colours"=BioWell %>% select(starts_with("The.vivid.colours")) %>% rowMeans(),
                       "The.variety.of.colours"=BioWell %>% select(starts_with("The.variety.of.colours")) %>% rowMeans(),
                       "The.maturity.of.living.things"=BioWell %>% select(starts_with("The.maturity.of.living.things")) %>% rowMeans(),
                       "The.variety.of.shapes"=BioWell %>% select(starts_with("The.variety.of.shapes")) %>% rowMeans(),
                       "The.sponginess.of.living.things"=BioWell %>% select(starts_with("The.sponginess.of.living.things")) %>% rowMeans(),
                       "The.variety.of.textures"=BioWell %>% select(starts_with("The.variety.of.textures")) %>% rowMeans(),
                       "The.woody.smells"=BioWell %>% select(starts_with("The.woody.smells")) %>% rowMeans(),
                       "The.variety.of.smells"=BioWell %>% select(starts_with("The.variety.of.smells")) %>% rowMeans(),
                       "Changes.in.this.season"=BioWell %>% select(starts_with("Changes.in.this.season")) %>% rowMeans(),
                       "The.presence.of.animals"=BioWell %>% select(starts_with("The.presence.of.animals")) %>% rowMeans())



# BioWell_Everything <-cbind(BioWell,BioWellScores,"Overall"=rowSums(BioWellScores)/17)
Winter <-cbind(Winter,BioWellScores,"Overall"=rowSums(BioWellScores)/17) ## Add a column to the Winter data with each persons mean biowell score.


#----------------------------------------------------------------------------------------------------------
## Now socioeconomic variables:
#----------------------------------------------------------------------------------------------------------



# ***********************
# Gender:
# ***********************

### Coding: Female=0,Male=1,Other=2
Winter$How.would.you.describe.your.gender. <- as.numeric(ifelse(Winter$How.would.you.describe.your.gender.==unique(Winter$How.would.you.describe.your.gender.)[1],1,
                                                                ifelse(Winter$How.would.you.describe.your.gender.==unique(Winter$How.would.you.describe.your.gender.)[2],0,2)))
colnames(Winter)[which(names(Winter)=="How.would.you.describe.your.gender.")] <- "Gender" ## RENAME COLUMNS
Winter <- Winter[Winter$Gender!=2,] ## Drop "In Another Way" respondents for simplicity


# ***********************
# Age:
# ***********************
# Calculate age as of survey date.
Winter$ExactAge <- as.numeric(ifelse(is.na(Winter$In.what.year.were.you.born....Please.select.from.the.dropdown.box)==TRUE,ifelse(Winter$How.old.are.you.==unique(Winter$How.old.are.you.)[1],23,ifelse(Winter$How.old.are.you.==unique(Winter$How.old.are.you.)[2],65,ifelse(Winter$How.old.are.you.==unique(Winter$How.old.are.you.)[3],45,ifelse(Winter$How.old.are.you.==unique(Winter$How.old.are.you.)[4],35,ifelse(Winter$How.old.are.you.==unique(Winter$How.old.are.you.)[5],55,70))))),as.numeric(2021 - Winter$In.what.year.were.you.born....Please.select.from.the.dropdown.box)))
Winter$DummyAge <- as.numeric(ifelse(Winter$ExactAge>=median(Winter$ExactAge,na.rm=TRUE),1,0))


# ***********************
# Protest Votes:
# ***********************

## Check by category before excluding
Winter$Protestor <- as.numeric(ifelse(is.na(Winter$You.selected.to.stick.with.the.planned.management.plan.for.the.new.woodland.in.all.of.your.choices.......Please.indicate.which..if.any..of..the.statements.listed.below.most.closely.match.your.reasons.for.this.choice...please.select.one.option..)==FALSE,1,0))

## List of possible answers to `Why choose the SQ?`:
#
# [2] "Due to COVID-19 related circumstances, I do not wish to commit to any tax increases"
# [3] "I could not afford any of the proposed changes to the new woodland"
# [4] "I already pay enough taxes and my government and local council should pay for any changes to the new woodland"
# [5] "I do not think it is important to finance any changes to the new woodland"
# [6] "Changes to this new woodland plan should not be funded by taxation"
# [7] "The options were too expensive compared to what I would get out of any changes to the new woodland"
# [8] "I would prefer to keep the new woodland management the same as the current plan"
# [9] "I prefer to spend my money on other things"
# [10] "This new woodland does not mean anything to me"
# [11] "Other"
# [12] "I do not think any management changes will have an effect on the new woodland"
# [13] "The trade-off between the different woodland elements made the current new woodland plan the best alternative in all choices"
# [14] "I did not understand the information I was given"


## Here, I make a vector of `Valid` reasons to consistently vote for the SQ.
ValidProtests <- c(c(unique(Winter$You.selected.to.stick.with.the.planned.management.plan.for.the.new.woodland.in.all.of.your.choices.......Please.indicate.which..if.any..of..the.statements.listed.below.most.closely.match.your.reasons.for.this.choice...please.select.one.option..)[3]),
                   c(unique(Winter$You.selected.to.stick.with.the.planned.management.plan.for.the.new.woodland.in.all.of.your.choices.......Please.indicate.which..if.any..of..the.statements.listed.below.most.closely.match.your.reasons.for.this.choice...please.select.one.option..)[7]),
                   c(unique(Winter$You.selected.to.stick.with.the.planned.management.plan.for.the.new.woodland.in.all.of.your.choices.......Please.indicate.which..if.any..of..the.statements.listed.below.most.closely.match.your.reasons.for.this.choice...please.select.one.option..)[8]),
                   c(unique(Winter$You.selected.to.stick.with.the.planned.management.plan.for.the.new.woodland.in.all.of.your.choices.......Please.indicate.which..if.any..of..the.statements.listed.below.most.closely.match.your.reasons.for.this.choice...please.select.one.option..)[9]),
                   c(unique(Winter$You.selected.to.stick.with.the.planned.management.plan.for.the.new.woodland.in.all.of.your.choices.......Please.indicate.which..if.any..of..the.statements.listed.below.most.closely.match.your.reasons.for.this.choice...please.select.one.option..)[10]),
                   c(unique(Winter$You.selected.to.stick.with.the.planned.management.plan.for.the.new.woodland.in.all.of.your.choices.......Please.indicate.which..if.any..of..the.statements.listed.below.most.closely.match.your.reasons.for.this.choice...please.select.one.option..)[13]))


## The vector is then used to indicate protest voters:
## Firstly, a value of zero is given for those who did not always vote for the SQ: those where the value is NA
## Secondly, a value of zero if the reason was more legitimate
Winter$Protestor <- (ifelse(is.na(Winter$You.selected.to.stick.with.the.planned.management.plan.for.the.new.woodland.in.all.of.your.choices.......Please.indicate.which..if.any..of..the.statements.listed.below.most.closely.match.your.reasons.for.this.choice...please.select.one.option..)==TRUE,0,
                            ifelse(Winter$You.selected.to.stick.with.the.planned.management.plan.for.the.new.woodland.in.all.of.your.choices.......Please.indicate.which..if.any..of..the.statements.listed.below.most.closely.match.your.reasons.for.this.choice...please.select.one.option.. %in% ValidProtests,0,1)))


# ***********************
# Participants completing too fast:
## There are two variables interested in:
# Winter$Timing...Page.Submit...23
# Winter$Timing...Page.Submit...28
# NO LONGER USED BUT ADDED FOR COMPLETENESS
# ***********************

## I concentrate on ..23 as there is a statistically significant difference between protestors time using 24 but not 29, see this Wilcox test:
wilcox.test(Winter$Timing...Page.Submit...23[Winter$Protestor == 0],
            Winter$Timing...Page.Submit...23[Winter$Protestor == 1]) ## Test differences between speed of (Q24) protest/non-protest
wilcox.test(Winter$Timing...Page.Submit...28[Winter$Protestor == 0],
            Winter$Timing...Page.Submit...28[Winter$Protestor == 1]) ## Test differences between speed of (Q29) protest/non-protest


## I then truncate the sample using the bottom 10% of the sample:
# The fastest 10%: 0.2024167 minutes
## This method excludes 86 respondents
length(Winter$ID[Winter$Timing...Page.Submit...23/60<quantile(Winter$Timing...Page.Submit...23/60,0.05)])

## The median timing for those faster then 50\% of the median: 0.2554417 minutes
## This method excludes 384 respondents
length(Winter$ID[Winter$Timing...Page.Submit...23/60<median(Winter$Timing...Page.Submit...23)/60*0.5])



## Here remove protest voters:
Winter_Truncated <- Winter[(Winter$Timing...Page.Submit...23/60 > quantile(Winter$Timing...Page.Submit...23/60,0.05)),]
Winter_Truncated <- Winter_Truncated[Winter_Truncated$Protestor!=1,]



# ***********************
# Country of respondent:
# Categorizing countries: England = 0, Wales = 1, Scotland = 2
# ***********************
Winter$Country <- ifelse(Winter$Which.country.are.you.currently.living.in. %in% c("England"),0,
                         ifelse(Winter$Which.country.are.you.currently.living.in. %in% c("Wales"),1,2)) %>% as.numeric()


# ***********************
# Urban or rural:
# ***********************
## Urbanicity: Rural = 0, Urban = 1
Winter$Urbanicity <- ifelse(Winter$Would.you.say.the.area.you.live.in.is.generally.rural.or.urban. %in% c("Urban"),1,0 ) %>% as.numeric()


# ***********************
# Respondent ethnicity:
# Note: Four variables defined.
# ***********************

## If ethnicity equals any of the following three they are coded `1` while all others are `0`
# "White - British/English/Welsh/Scottish/Northern Irish" [3]
# "White - any other white background" [13]
# "White - Irish" [16]
# "White - Gypsy or Irish Traveller"
Winter$EthnicityDummyWhite <-
  ifelse(
    Winter$Which.of.these.ethnic.groups.do.you.belong.to. %in% c(
      "White - British/English/Welsh/Scottish/Northern Irish",
      "White - any other white background",
      "White - Gypsy or Irish Traveller",
      "White - Irish"
    ),
    1, #White passing
    0 # All other
  ) %>% as.numeric()


## If ethnicity equals any of the following three they are coded `1` while all others are `0`
# "Asian or Asian British - Bangladeshi"
# "Asian or Asian British - Indian"
# "Asian or Asian British - Chinese"
# "Asian or Asian British - any other Asian background"
# "Asian or Asian British - Pakistani"
Winter$EthnicityDummyAsian <-
  ifelse(Winter$Which.of.these.ethnic.groups.do.you.belong.to.  %in%  c("Asian or Asian British - Bangladeshi",
                                                                        "Asian or Asian British - Indian",
                                                                        "Asian or Asian British - Chinese",
                                                                        "Asian or Asian British - any other Asian background",
                                                                        "Asian or Asian British - Pakistani"),
         1,
         0
  ) %>% as.numeric()


## If ethnicity equals any of the following three they are coded `1` while all others are `0`
# "Black or Black British - Caribbean"
# "Black or Black British - African"
# "Black or Black British - any other Black background"
Winter$EthnicityDummyBlack <-
  ifelse(
    Winter$Which.of.these.ethnic.groups.do.you.belong.to.  %in%  c("Black or Black British - Caribbean",
                                                                   "Black or Black British - African",
                                                                   "Black or Black British - any other Black background"),
    1,
    0
  ) %>% as.numeric()

## If ethnicity equals any of the following three they are coded `1` while all others are `0`
# Mixed - any other mixed background" 33
# "Mixed - White and Black African"   9
# "Any other ethnic group" 15
# "Mixed - White and Asian" 36
# "Other ethnic group - Arab" 8
# "Mixed - White and Black Caribbean" 13
Winter$EthnicityDummyMixed <-
  ifelse(
    Winter$Which.of.these.ethnic.groups.do.you.belong.to. %in% c("Mixed - any other mixed background",
                                                                 "Any other ethnic group",
                                                                 "Mixed - White and Asian" ,
                                                                 "Other ethnic group - Arab",
                                                                 "Mixed - White and Black Caribbean",
                                                                 "Mixed - White and Black African"),1,0) %>% as.numeric()



# ***********************
# Profession of respondent:
# ***********************
## Profession-based class dummy: ABC1 = 1,C2DE = 0
Winter$ClassDummy <-
  ifelse(
    Winter$Now..thinking.of.the.chief..income.earner.in.your.household..which.might.be.you.or.somebody.else.in.the..household..which.of.these.best.describes.the.current.status.of.the.chief.income..earner. %in% c(
      "Senior management",
      "Office/clerical/administration",
      "Crafts/tradesperson/skilled worker",
      "Middle management",
      "Professional"
    ),
    1, ## ABC1
    0 ## C2DE
  )



# ***********************
# Seasonal visit frequency:
# Coding: Categorical increasing with frequency
## There is a way to loop through and apply this all in one but idk right now
# ***********************
## Woodland Visit Frequency Dummy Summer2020
Winter$Summer2020WoodlandVisitDummy <- recode(Winter$Over.the.past.year..how.often.did.you..visit.a.woodland.or.forest.during.each.season..Please.tick.one.option.for.each..season....Summer.2020..Jun.to.Aug.,
                                              "I did not visit"=0,
                                              "Once or twice a season"=1,
                                              "Once or twice a month"=2,
                                              "Once a week"=3,
                                              "Several times a week"=4,
                                              "Every day"=5)


## Woodland Visit Frequency Dummy Autumn2020
Winter$Autumn2020WoodlandVisitDummy <- recode(Winter$Over.the.past.year..how.often.did.you..visit.a.woodland.or.forest.during.each.season..Please.tick.one.option.for.each..season....Autumn.2020..Sep.to.Nov.,
                                              "I did not visit"=0,
                                              "Once or twice a season"=1,
                                              "Once or twice a month"=2,
                                              "Once a week"=3,
                                              "Several times a week"=4,
                                              "Every day"=5)


## Woodland Visit Frequency Dummy Winter2020
Winter$Winter2020WoodlandVisitDummy <- recode(Winter$Over.the.past.year..how.often.did.you..visit.a.woodland.or.forest.during.each.season..Please.tick.one.option.for.each..season....Winter.2020.21..Dec.to.Feb.,
                                              "I did not visit"=0,
                                              "Once or twice a season"=1,
                                              "Once or twice a month"=2,
                                              "Once a week"=3,
                                              "Several times a week"=4,
                                              "Every day"=5)


## Woodland Visit Frequency Dummy Spring2021
Winter$Spring20210WoodlandVisitDummy <- recode(Winter$Over.the.past.year..how.often.did.you..visit.a.woodland.or.forest.during.each.season..Please.tick.one.option.for.each..season....Spring.2021..Mar.to.now.,
                                               "I did not visit"=0,
                                               "Once or twice a season"=1,
                                               "Once or twice a month"=2,
                                               "Once a week"=3,
                                               "Several times a week"=4,
                                               "Every day"=5)
## THIS IS HOW YOU RENAME COLUMNS
colnames(Winter)[which(names(Winter)=="Spring20210WoodlandVisitDummy")] <- "MostRecentVisit"


# ***********************
# Age visit frequency:
# Coding: Categorical increasing with frequency
# ***********************
## Child Visit Frequency
Winter$ChildVisitWoodlands <- recode(Winter$As.a.child..up.to.12..years.old..I.spent.a.lot.of.time.in.woodlands.or.forests,
                                     "Strongly disagree"=0,
                                     "Disagree"=1,
                                     "Neither agree nor disagree"=2,
                                     "Agree"=3,
                                     "Strongly agree"=4)

## Teen Visit Frequency
Winter$TeenVisitWoodlands <- recode(Winter$As..a.teenager..13.to.18.years.old..I.spent.a.lot.of.time.in.woodlands.or.forests,
                                    "Strongly disagree"=0,
                                    "Disagree"=1,
                                    "Neither agree nor disagree"=2,
                                    "Agree"=3,
                                    "Strongly agree"=4)


## Adult Visit Importance
Winter$AdultVisitWoodlands <- recode(Winter$Spending.time.out.of.doors.is.an.important.part.of.my.life..By..out.of.doors..we.mean.open.spaces.in.and.around.towns.and.cities..including.parks..canals.and.nature.areas..the.coast.and.beaches..and.the.countryside.including.farmland..woodland..hills.and.rivers.,
                                     "Strongly disagree"=0,
                                     "Disagree"=1,
                                     "Neither agree nor disagree"=2,
                                     "Agree"=3,
                                     "Strongly agree"=4)



## Rename the local woodlands perception variables:
colnames(Winter)[which(names(Winter)=="What.is.your.nearby.woodland.like..........Thinking.about.that.nearby.woodland.you.just.selected..please.indicate.the.level.of.each.element.you.believe.that.woodland.contains.at.this.time.of.year..i.e..Spring............For.example..if.you.believe.your.nearby.woodland.has.very.low.amount.of.an.element..e.g..variety.of.sounds...move.the.blue.circle.far.to.the.right..If.you.believe.it.has.a.high.amount.of.an.element..move.the.blue.circle.to.the.left..If.you.think.your.woodland.has.a.medium.amount.of.an.element..place.the.circle.in.the.middle..........Remember..there.are.no.right.or.wrong.answers....The.variety.of.colours")] <- "WoodlandsColours"
colnames(Winter)[which(names(Winter)=="What.is.your.nearby.woodland.like..........Thinking.about.that.nearby.woodland.you.just.selected..please.indicate.the.level.of.each.element.you.believe.that.woodland.contains.at.this.time.of.year..i.e..Spring............For.example..if.you.believe.your.nearby.woodland.has.very.low.amount.of.an.element..e.g..variety.of.sounds...move.the.blue.circle.far.to.the.right..If.you.believe.it.has.a.high.amount.of.an.element..move.the.blue.circle.to.the.left..If.you.think.your.woodland.has.a.medium.amount.of.an.element..place.the.circle.in.the.middle..........Remember..there.are.no.right.or.wrong.answers....The.variety.of.sounds")] <- "WoodlandsSounds"
colnames(Winter)[which(names(Winter)=="What.is.your.nearby.woodland.like..........Thinking.about.that.nearby.woodland.you.just.selected..please.indicate.the.level.of.each.element.you.believe.that.woodland.contains.at.this.time.of.year..i.e..Spring............For.example..if.you.believe.your.nearby.woodland.has.very.low.amount.of.an.element..e.g..variety.of.sounds...move.the.blue.circle.far.to.the.right..If.you.believe.it.has.a.high.amount.of.an.element..move.the.blue.circle.to.the.left..If.you.think.your.woodland.has.a.medium.amount.of.an.element..place.the.circle.in.the.middle..........Remember..there.are.no.right.or.wrong.answers....The.variety.of.smells")] <- "WoodlandsSmells"
colnames(Winter)[which(names(Winter)=="What.is.your.nearby.woodland.like..........Thinking.about.that.nearby.woodland.you.just.selected..please.indicate.the.level.of.each.element.you.believe.that.woodland.contains.at.this.time.of.year..i.e..Spring............For.example..if.you.believe.your.nearby.woodland.has.very.low.amount.of.an.element..e.g..variety.of.sounds...move.the.blue.circle.far.to.the.right..If.you.believe.it.has.a.high.amount.of.an.element..move.the.blue.circle.to.the.left..If.you.think.your.woodland.has.a.medium.amount.of.an.element..place.the.circle.in.the.middle..........Remember..there.are.no.right.or.wrong.answers....The.number.of.trees.for.natural.decomposition")] <- "WoodlandsTree"


# ***********************
# Charity membership:
# Coding: No = 0, Yes = 1
# ***********************
Winter$Charity <- recode(Winter$In.the.last.five.years.have.you.been.a.member.of.any.wildlife.conservation.or.natural.heritage.organisations..e.g...RSPB..Yorkshire.Wildlife.Trust..National.Trust..a.local.angling.society..,
                         "No"=0,
                         "Yes"=1)

# ***********************
# Income:
# Note: Convert income bracket to levels
# Note: Then convert levels to dummy.
# ***********************
Winter$IncomeLevels <-recode(Winter$Please.select.the.income.band.that.represents.your.total..before.tax..yearly.household.income..pick.one.option.,
                             "Prefer not to say"=0,
                             "Up to £5199"=5000,
                             "£5200 and up to £10,399"=7799.50,
                             "£10,400 up to £15,599" =12999.50,
                             "£15,600 and up to £20,799"=18199.50,
                             "£20,800 up to £25,999" =23399.50,
                             "£26,000 and up to £31,199"=28599.50,
                             "£31,200 up to £36,399"=33799.50,
                             "£36,400 and up to £51,999"=44199.50,
                             "£52,000 and above" =52000)

## Income Dummy. Below median (28599.5) - 0, Above = 1
Winter$IncomeDummy <- ifelse(Winter$IncomeLevels>=median(Winter$IncomeLevels),1,0)


# ***********************
# Education:
# ***********************
Winter$EducationLevels <- recode(Winter$Please.select.your.highest.level.of.education.you.have.completed..pick.one.option.,
                                 "No qualifications"=0,
                                 "1 - 4 Levels / CSEs / GCSEs, NVQ Level 1" =1,
                                 "Other qualifications (vocational/work related, foreign qualifications or level unknown)"=2,
                                 "5 + O Levels / CSEs / GCSEs, NVQ Level 2, AS Levels, Higher Diploma, Diploma Apprenticeship"=3,
                                 "2 + A Levels, NVQ Level 3, BTEC National Diploma"=4,
                                 "Degree, Higher Degree, NVQ level 4-5, BTEC Higher Level, professional qualifications (e.g. teaching, nursing, accountancy)"=5)


## Dummy for educated higher than A levels
Winter$EducationDummy <- ifelse(Winter$EducationLevels>3,1,0)


# ***********************
# Sensory Impairment:
# Coding: No = 0, Yes = 1
# ***********************
Winter$SmellIssues <- ifelse(Winter$Do..you.have.any.difficulties.being.able.to.smell.at.the.moment..For.example..due..to.no.sense.of.smell..having.a.cold.or.hay.fever.=="Yes",1,0)
Winter$HearingIssues <- ifelse(Winter$Do..you.have.difficulties.with.your.hearing.at.the.moment..Please.answer.assuming..you.are.wearing.hearing.aids.if.you.need.them.=="Yes",1,0)
Winter$SightIssues <- ifelse(Winter$Do..you.have.difficulties.with.your.sight.at.the.moment..Please.answer.assuming.you..are.wearing.glasses.or.contact.lenses.if.you.need.them.=="Yes",1,0)

Winter$Impairment <- ifelse((Winter$SightIssues==1)|
                                (Winter$SmellIssues==1)|
                                (Winter$HearingIssues==1),1,0)

#----------------------------------------------------------------------------------------------------------
## Calculating distances:
# Two Stages:
# Stage One: Convert lon-lat to numerical
# Stage Two: Calculate distances between numerical points.
#----------------------------------------------------------------------------------------------------------


# ***********************
# Stage One: Converting WOODLANDS lon-lat to useable coordinates
# ***********************
## Puts Woodland Lat (Column 1) and Lon (Column 2) in a dataframe
Woodland1<-(data.frame(strsplit(as.character(noquote(gsub("[:lat lng ,{}\"]","",Winter$Your.nearby.woodland......We.would.now.like.you.to.think.about.a.woodland.near.to.where.you.live..Using.the.map.below..please.find.that.woodland..You.can.either.search.using.the.search.bar.below.the.map..or.click.and.drag.the.marker.......Try.to.be.as.specific.as.possible..If.there.is.a.particular.area.of.the.woodland.you.visit..try.to.move.the.marker.as.close.to.that.as.possible..This.will.help.us.understand.what.the.woodland.is.like.,ignore.case = TRUE))),split = "-")))
Woodland1[1,] <- as.numeric(Woodland1[1,])
Woodland1[2,] <- -1*as.numeric(Woodland1[2,])
Woodland1 <- cbind(t(Woodland1[2,]),t(Woodland1[1,]))
colnames(Woodland1) <- c("LonW","LatW")
rownames(Woodland1) <- Winter$ID
# Woodland1 <- Woodland1[complete.cases(Woodland1),]


# ***********************
# Stage One: Converting RESPONDENTS lon-lat to useable coordinates
# ***********************
## Puts Housing Lat (Column 1) and Lon (Column 2) in a dataframe
Housing1<-(data.frame(strsplit(as.character(noquote(gsub("[:lat lng ,{}\"]","",Winter$Using.the.map.below..please.indicate.where.you.live..You.can.either.search.using.the.search.bar.below.the.map..or.click.and.drag.the.marker..........We.will.only.use.this.information.for.the.purposes.of.calculating.the.distance.between.the.woodland.you.have.selected.and.where.you.live..This.information.will.stored.securely.following.GDPR.and.will.not.be.shared.any.further.than.the.immediate.research.team..We.will.not.use.this.information.for.any.marketing.purposed.or.share.it.with.third.parties.,ignore.case = TRUE))),split = "-")))
Housing1[1,] <- as.numeric(Housing1[1,])
Housing1[2,] <- -1*as.numeric(Housing1[2,])
Housing1 <- cbind(t(Housing1[2,]),t(Housing1[1,]))
colnames(Housing1) <- c("LonH","LatH")
rownames(Housing1) <- Winter$ID


## Putting all the distance measures in one for now.
DistancesReported <- (cbind(Woodland1,Housing1))
DistancesReported <- data.frame(DistancesReported)
## Calculate distance between points in miles (formula uses (Lon,Lat))
# distm(c(as.numeric(DistancesReported[,2]),as.numeric(DistancesReported[,1])), c(as.numeric(DistancesReported[,4]),as.numeric(DistancesReported[,3])), fun = distHaversine)[,1]/1609


# ***********************
# Stage Two:
# Calculates distance in three ways: Map (coordinates), Miles (reported), Postcode (between postcodes)
# ***********************

# Calculate a value for each respondent who replied with their data
for (i in 1:length(Winter$Your.nearby.woodland......We.would.now.like.you.to.think.about.a.woodland.near.to.where.you.live..Using.the.map.below..please.find.that.woodland..You.can.either.search.using.the.search.bar.below.the.map..or.click.and.drag.the.marker.......Try.to.be.as.specific.as.possible..If.there.is.a.particular.area.of.the.woodland.you.visit..try.to.move.the.marker.as.close.to.that.as.possible..This.will.help.us.understand.what.the.woodland.is.like.)){
  Winter$MapDistance[i] <-ifelse(is.na(distm(x = as.numeric(c(DistancesReported[i,3:4])),y=as.numeric(c(DistancesReported[i,1:2])))/1609)==TRUE,NA,distm(x = as.numeric(c(DistancesReported[i,3:4])),y=as.numeric(c(DistancesReported[i,1:2])))/1609)
}



## Doing "PostcodeDistance" in two parts:
## Initialise vector here
Winter$PostcodeDistance <- rep(0,each=nrow(Winter))

### a) Validate what we have
### b) Look up and calculate distance only for what we have
for (j in 1:length(
  Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...)) {
  Winter$PostcodeDistance[j] <-  ifelse(
    postcode_validation(
        Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...[j]
      ) == TRUE,
      1, ## SO postcode is validated
      0 ## postcode is not validated
    )
}


## This is step B where we calculate distance between points if the postcode is validated
## value = 0 otherwise
for (j in 1:length(
  Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...)) {
  Winter$PostcodeDistance[j] <- ifelse(Winter$PostcodeDistance[j]==1,
                                       distm(x = as.numeric(
                                         c(
                                           postcode_lookup(
                                             Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...[j]
                                           )$longitude,
                                           postcode_lookup(
                                             Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...[j]
                                           )$latitude
                                         )
                                       ),
                                       y = as.numeric(c(Woodland1[j, 1:2]))) /
                                         1609, ## Dividing to calculate in terms of miles
                                       0) ## Not validated so don't bother
}

## Original code:
# # Calculate a value for each respondent who replied with their postcode
# for (j in 1:length(Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...)){
#   Winter$PostcodeDistance[j] <-
#     ifelse(
#       postcode_validation(
#         Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...[j]
#       ) == TRUE,
#       distm(x = as.numeric(
#         c(
#           postcode_lookup(
#             Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...[j]
#           )$longitude,
#           postcode_lookup(
#             Winter$To.help.us.understand.more.about.the.woodland.you.are.thinking.of..we.want.to.ask.you.a.few.questions.about.where.the.nearby.woodland.is.in.relation.to.your.home...........Please.enter.your.full.postcode..e.g..SW1.4RK.or.TG16.3SJ...[j]
#           )$latitude
#         )
#       ),
#       y = as.numeric(c(Woodland1[j, 1:2]))) /
#         1609,
#       0
#     )
# }



## Now calculating a self-reported miles from woodland measure
Winter$MilesDistance <- recode(Winter$Please..indicate.the.approximate.distance.as.the.crow.flies..one.way..between.your.home..and.your.nearby.woodland.,
                               "Less than 1 mile"=1,
                               "1-4 miles"=2.5,
                               "5-9 miles"=7,
                               "10-19 miles"=14.5,
                               "Over 20 miles" =20)

## Converting all distances from miles to kilometers:
Winter$MilesDistance <- ud.convert(Winter$MilesDistance,"mi","km")
Winter$MapDistance <- ud.convert(Winter$MapDistance,"mi","km")
Winter$PostcodeDistance <- ud.convert(Winter$PostcodeDistance,"mi","km")



#----------------------------------------------------------------------------------------------------------
#### Section 3: Coding CE Attributes ####
## The next bit is very repetitive.
## I input the dummy levels for the winter choice cards using info from here: https://www.dropbox.com/home/WP5/Choice%20modelling/Final%20choice%20design%20(post%20pilot)/Winter
#----------------------------------------------------------------------------------------------------------



Scenario1CE <- data.frame("Sound1"=0,"Sound2"=2,"Sound3"=0,
                          "Colour1"=2,"Colour2"=0,"Colour3"=0,
                          "Smell1"=0,"Smell2"=2,"Smell3"=0,
                          "Deadwood1"=15,"Deadwood2"=0,"Deadwood3"=0,
                          "Tax1"=10,"Tax2"=20,"Tax3"=0)

Scenario2CE <-data.frame("Sound1"=0,"Sound2"=2,"Sound3"=0,
                         "Colour1"=2,"Colour2"=0,"Colour3"=0,
                         "Smell1"=0,"Smell2"=2,"Smell3"=0,
                         "Deadwood1"=0,"Deadwood2"=15,"Deadwood3"=0,
                         "Tax1"=4,"Tax2"=60,"Tax3"=0)

Scenario3CE <-data.frame("Sound1"=0,"Sound2"=2,"Sound3"=0,
                         "Colour1"=1,"Colour2"=2,"Colour3"=0,
                         "Smell1"=0,"Smell2"=2,"Smell3"=0,
                         "Deadwood1"=0,"Deadwood2"=15,"Deadwood3"=0,
                         "Tax1"=4,"Tax2"=35,"Tax3"=0)


Scenario4CE <- data.frame("Sound1"=0,"Sound2"=2,"Sound3"=0,
                          "Colour1"=1,"Colour2"=1,"Colour3"=0,
                          "Smell1"=1,"Smell2"=1,"Smell3"=0,
                          "Deadwood1"=0,"Deadwood2"=15,"Deadwood3"=0,
                          "Tax1"=4,"Tax2"=20,"Tax3"=0)

Scenario5CE <- data.frame("Sound1"=0,"Sound2"=1,"Sound3"=0,
                          "Colour1"=0,"Colour2"=1,"Colour3"=0,
                          "Smell1"=1,"Smell2"=1,"Smell3"=0,
                          "Deadwood1"=7,"Deadwood2"=0,"Deadwood3"=0,
                          "Tax1"=35,"Tax2"=60,"Tax3"=0)

Scenario6CE <- data.frame("Sound1"=2,"Sound2"=1,"Sound3"=0,
                          "Colour1"=0,"Colour2"=2,"Colour3"=0,
                          "Smell1"=2,"Smell2"=0,"Smell3"=0,
                          "Deadwood1"=0,"Deadwood2"=15,"Deadwood3"=0,
                          "Tax1"=2,"Tax2"=35,"Tax3"=0)


Scenario7CE <- data.frame("Sound1"=2,"Sound2"=0,"Sound3"=0,
                          "Colour1"=2,"Colour2"=0,"Colour3"=0,
                          "Smell1"=1,"Smell2"=1,"Smell3"=0,
                          "Deadwood1"=0,"Deadwood2"=15,"Deadwood3"=0,
                          "Tax1"=4,"Tax2"=35,"Tax3"=0)

Scenario8CE <- data.frame("Sound1"=1,"Sound2"=1,"Sound3"=0,
                          "Colour1"=0,"Colour2"=1,"Colour3"=0,
                          "Smell1"=2,"Smell2"=1,"Smell3"=0,
                          "Deadwood1"=0,"Deadwood2"=7,"Deadwood3"=0,
                          "Tax1"=2,"Tax2"=60,"Tax3"=0)

Scenario9CE <- data.frame("Sound1"=0,"Sound2"=2,"Sound3"=0,
                          "Colour1"=0,"Colour2"=2,"Colour3"=0,
                          "Smell1"=2,"Smell2"=0,"Smell3"=0,
                          "Deadwood1"=15,"Deadwood2"=0,"Deadwood3"=0,
                          "Tax1"=10,"Tax2"=20,"Tax3"=0)

Scenario10CE <- data.frame("Sound1"=1,"Sound2"=1,"Sound3"=0,
                           "Colour1"=1,"Colour2"=1,"Colour3"=0,
                           "Smell1"=1,"Smell2"=0,"Smell3"=0,
                           "Deadwood1"=7,"Deadwood2"=7,"Deadwood3"=0,
                           "Tax1"=60,"Tax2"=2,"Tax3"=0)

Scenario11CE <- data.frame("Sound1"=1,"Sound2"=1,"Sound3"=0,
                           "Colour1"=1,"Colour2"=1,"Colour3"=0,
                           "Smell1"=2,"Smell2"=1,"Smell3"=0,
                           "Deadwood1"=15,"Deadwood2"=0,"Deadwood3"=0,
                           "Tax1"=35,"Tax2"=20,"Tax3"=0)

Scenario12CE <- data.frame("Sound1"=2,"Sound2"=0,"Sound3"=0,
                           "Colour1"=0,"Colour2"=2,"Colour3"=0,
                           "Smell1"=0,"Smell2"=2,"Smell3"=0,
                           "Deadwood1"=15,"Deadwood2"=0,"Deadwood3"=0,
                           "Tax1"=20,"Tax2"=4,"Tax3"=0)

Scenario13CE <- data.frame("Sound1"=1,"Sound2"=0,"Sound3"=0,
                           "Colour1"=1,"Colour2"=1,"Colour3"=0,
                           "Smell1"=1,"Smell2"=1,"Smell3"=0,
                           "Deadwood1"=15,"Deadwood2"=0,"Deadwood3"=0,
                           "Tax1"=60,"Tax2"=10,"Tax3"=0)

Scenario14CE <-data.frame("Sound1"=1,"Sound2"=2,"Sound3"=0,
                          "Colour1"=2,"Colour2"=0,"Colour3"=0,
                          "Smell1"=2,"Smell2"=0,"Smell3"=0,
                          "Deadwood1"=7,"Deadwood2"=7,"Deadwood3"=0,
                          "Tax1"=35,"Tax2"=2,"Tax3"=0)

Scenario15CE <-data.frame("Sound1"=2,"Sound2"=0,"Sound3"=0,
                          "Colour1"=2,"Colour2"=0,"Colour3"=0,
                          "Smell1"=2,"Smell2"=0,"Smell3"=0,
                          "Deadwood1"=7,"Deadwood2"=7,"Deadwood3"=0,
                          "Tax1"=10,"Tax2"=4,"Tax3"=0)

Scenario16CE <- data.frame("Sound1"=2,"Sound2"=0,"Sound3"=0,
                           "Colour1"=2,"Colour2"=0,"Colour3"=0,
                           "Smell1"=0,"Smell2"=2,"Smell3"=0,
                           "Deadwood1"=15,"Deadwood2"=7,"Deadwood3"=0,
                           "Tax1"=20,"Tax2"=20,"Tax3"=0)

Scenario17CE <- data.frame("Sound1"=1,"Sound2"=1,"Sound3"=0,
                           "Colour1"=1,"Colour2"=2,"Colour3"=0,
                           "Smell1"=1,"Smell2"=1,"Smell3"=0,
                           "Deadwood1"=7,"Deadwood2"=7,"Deadwood3"=0,
                           "Tax1"=2,"Tax2"=60,"Tax3"=0)

Scenario18CE <- data.frame("Sound1"=2,"Sound2"=0,"Sound3"=0,
                           "Colour1"=0,"Colour2"=2,"Colour3"=0,
                           "Smell1"=0,"Smell2"=2,"Smell3"=0,
                           "Deadwood1"=7,"Deadwood2"=15,"Deadwood3"=0,
                           "Tax1"=10,"Tax2"=10,"Tax3"=0)


## This is all the attribute levels for BLOCK ONE WINTER
LevelsB1 <- data.frame(rbind(Scenario1CE,Scenario3CE,Scenario6CE,Scenario7CE,
                             Scenario8CE,Scenario9CE,Scenario10CE,Scenario13CE,
                             Scenario16CE))
rownames(LevelsB1) <- c("Scenario1","Scenario3","Scenario6","Scenario7",
                        "Scenario8","Scenario9","Scenario10",
                        "Scenario13","Scenario16")


## This is all the attribute levels for BLOCK TWO WINTER
LevelsB2 <- data.frame(rbind(Scenario2CE,Scenario4CE,Scenario5CE,Scenario11CE,Scenario12CE,
                             Scenario14CE,Scenario15CE,Scenario17CE,Scenario18CE))
rownames(LevelsB2) <- c("Scenario2","Scenario4","Scenario5","Scenario11",
                        "Scenario12","Scenario14",
                        "Scenario15","Scenario17","Scenario18")


## Here I just repeat the CE attributes for each of the 9 choices
LevelsB1A <- LevelsB1[rep(rownames(LevelsB1),nrow(Winter)),]
LevelsB2A <- LevelsB2[rep(rownames(LevelsB2),nrow(Winter)),]
colnames(LevelsB2A) <- paste(colnames(LevelsB2A),"B") # The columns wouldn't merge unless the new headers were different so I change the BLOCKTWO column names


## Take all the respondent choices and change into a single vector
Choices <- data.frame(Choice = c(t(
  data.frame(rep(Winter[,33:41] , times=1)))[,]))


Winter_NoCE <- Winter[,-c(33:41)] ## Necessary to reshape survey data.


#----------------------------------------------------------------------------------------------------------
#### Section 3B: Reshaping data ####
## Notes: Goes from one row per respondent to one row per choice (thus 9 per respondent).
#----------------------------------------------------------------------------------------------------------


## Combine the choices above with a `Task` variable and all the remaining data reshaped from long to wide
Reshaped <- (data.frame(rep((1:9),times=nrow(Winter)),
                        Choices,
                        data.frame(apply(X = Winter_NoCE,2,function(x) rep(x,each=9)))))
colnames(Reshaped)[1:2] <- c("Task","Choice") ## Note: The `Task` variable refers to which number of choice tasks the respondent did.


# ***********************
# Now adding in attribute levels per respondent per choice:
## Essentially this inputs the CE attribute levels depending on respondent's block.
# ***********************
Reshaped$Sound1 <- ifelse(Reshaped$Block==1,LevelsB1A$Sound1,LevelsB2A$`Sound1 B`)
Reshaped$Sound2 <- ifelse(Reshaped$Block==1,LevelsB1A$Sound2,LevelsB2A$`Sound2 B`)
Reshaped$Sound3 <- ifelse(Reshaped$Block==1,LevelsB1A$Sound3,LevelsB2A$`Sound3 B`)
Reshaped$Colour1 <- ifelse(Reshaped$Block==1,LevelsB1A$Colour1,LevelsB2A$`Colour1 B`)
Reshaped$Colour2 <- ifelse(Reshaped$Block==1,LevelsB1A$Colour2,LevelsB2A$`Colour2 B`)
Reshaped$Colour3 <- ifelse(Reshaped$Block==1,LevelsB1A$Colour3,LevelsB2A$`Colour3 B`)

Reshaped$Smell1 <- ifelse(Reshaped$Block==1,LevelsB1A$Smell1,LevelsB2A$`Smell1 B`)
Reshaped$Smell2 <- ifelse(Reshaped$Block==1,LevelsB1A$Smell2,LevelsB2A$`Smell2 B`)
Reshaped$Smell3 <- ifelse(Reshaped$Block==1,LevelsB1A$Smell3,LevelsB2A$`Smell3 B`)

Reshaped$Deadwood1 <- ifelse(Reshaped$Block==1,LevelsB1A$Deadwood1,LevelsB2A$`Deadwood1 B`)
Reshaped$Deadwood2 <- ifelse(Reshaped$Block==1,LevelsB1A$Deadwood2,LevelsB2A$`Deadwood2 B`)
Reshaped$Deadwood3 <- ifelse(Reshaped$Block==1,LevelsB1A$Deadwood3,LevelsB2A$`Deadwood3 B`)

Reshaped$Tax1 <- ifelse(Reshaped$Block==1,LevelsB1A$Tax1,LevelsB2A$`Tax1 B`)
Reshaped$Tax2 <- ifelse(Reshaped$Block==1,LevelsB1A$Tax2,LevelsB2A$`Tax2 B`)
Reshaped$Tax3 <- ifelse(Reshaped$Block==1,LevelsB1A$Tax3,LevelsB2A$`Tax3 B`)


# ***********************
# Coding other necessary features of the CE including choice, task, ID, respondent.
# ***********************
# The text has to be numerical but need to consider the correct coding.
## NOTE: Alt1 (A) ==1, Alt2 (B) ==2, Alt3 (Status Quo) ==3
Reshaped$Choice <- recode(Reshaped$Choice,"OPTION A"=1,
                          "OPTION B"=2, "NO CHANGE"=3)


## APOLLO always needs these availability indicators in case any alternative is not available.
Reshaped$av_A <- rep(1,nrow(Reshaped)) ## Alternative A: Alt1
Reshaped$av_B <- rep(1,nrow(Reshaped)) ## Alternative B: Alt2
Reshaped$av_C <- rep(1,nrow(Reshaped)) ## Alternative C: Status Quo


# Unique identifier for each respondent and choice:
Reshaped$ID <- seq.int(nrow(Reshaped))
# unique identifier for each respondent:
Reshaped$Respondent <-rep(1:nrow(Winter),each=length(unique(Reshaped$Task)))


## I coerce all these variables to numeric here to enable estimation later
Reshaped <- Reshaped %>% mutate_at(c("EducationDummy","Choice","Gender","ExactAge","MilesDistance",
                                     "Charity","EducationLevels","ClassDummy","IncomeDummy",
                                     "SmellIssues","SightIssues","SightIssues","HearingIssues","Impairment",
                                     "Country","EthnicityDummyWhite","Block","Overall",
                                     "Encountering.the.living.things","The.number.of.living.things",
                                     "The.variety.of.living.things","The.interactions.between.plants",
                                     "The.living.processes","The.variety.of.sounds",
                                     "The.distinctive.sounds","The.vivid.colours",
                                     "The.variety.of.colours","The.maturity.of.living.things",
                                     "The.variety.of.shapes","The.sponginess.of.living.things",
                                     "The.variety.of.textures","The.woody.smells",
                                     "The.variety.of.smells","Changes.in.this.season",
                                     "The.presence.of.animals",
                                     "WoodlandsSounds","WoodlandsSmells","WoodlandsColours","WoodlandsTree"), as.numeric)


## APOLLO requires the data to be called `database` so I do that here
database<- data.frame(Reshaped)
database<- database[!is.na(database$MilesDistance),] ## Drop missing distances
database<- database[!is.na(database$Overall),] ## Drop respondents not completing BIOWELL




#----------------------------------------------------------------------------------------------------------
#### Section 4: Finishing and exporting data ####
#----------------------------------------------------------------------------------------------------------


# ***********************
## Assorted dummy coding fixes
# ***********************
# database$EthnicityDummyWhite <- ifelse(database$EthnicityDummyWhite==0,1,0) ## Was here for a reason once but idk why now
database$WoodlandsScore <- rowMeans(cbind(database$WoodlandsSounds,database$WoodlandsSmells,database$WoodlandsColours,database$WoodlandsTree))
Winter$WoodlandsScore <- rowMeans(cbind(Winter$WoodlandsSounds,Winter$WoodlandsSmells,Winter$WoodlandsColours,Winter$WoodlandsTree))
colnames(Winter)[which(names(Winter)=="Spring20210WoodlandVisitDummy")] <- "MostRecentVisit"
colnames(database)[which(names(database)=="Spring20210WoodlandVisitDummy")] <- "MostRecentVisit"


## How to truncate the data for use in Apollo
## NOTE: Not used anymore
# database_Truncated <- database[database$Timing...Page.Submit...24/60 > quantile(database$Timing...Page.Submit...24/60,0.05),]
# database_Truncated <- database_Truncated[database_Truncated$Protestor!=1,]
# database_Truncated$Respondent <- rep(1:(nrow(database_Truncated)/9),each=(length(unique(database_Truncated$Task))))


# ***********************
## Exporting all dataframes to the directory to enable reference to later
# Note: Necessary to specify file encoding when working with TESLA/ICARUS
# ***********************


## These are the only ones we actually end up using
write.csv(database, file = "database_Winter_Step1.csv", fileEncoding = "latin1")
write.csv(Winter,  file = "Winter_dataframe_Step1.csv", fileEncoding = "latin1")

# write.csv(database_Truncated, file = "database_Truncated_Winter.csv", fileEncoding = "latin1")

# write.csv(Winter_Truncated,  file = "Winter_Truncated_dataframe.csv", fileEncoding = "latin1")
# write.csv(Reshaped, file = "Reshaped_Winter_dataframe.csv", fileEncoding = "latin1")


#----------------------------------------------------------------------------------------------------------
#### END OF SCRIPT ####
## Notes: Data now converted to usable dataframes: Winter_dataframe and database_Winter
## Next step: Winter_SpatialSetup.R
#----------------------------------------------------------------------------------------------------------
