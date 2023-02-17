

# Load Packages -----------------------------------------------------------
library("haven")
library("here")


# Load dataset ------------------------------------------------------------

df <- read_dta(here("AMUSEFARMERAW.dta"))



# Broad plan for analysis -------------------------------------------------

#' main ideas:
#' Look at determinants of disease incidence (AMU type, AMU expenditure, vaccination, biosecurity, access to animal health services)
#' Subquestions:
#' 1) how necessary is AMU in preventing disease?
#' 2) is it only therapeutic AMU which prevents disease?
#' 3) can vaccination, biosecurity, non-antimicrobial drugs and animal health services provide a similar benefit to antimicrobials?
#' 
#' Can do regression analyses, and also just divide farms into categories and see if disease incidence
#' differs significantly between them


#' Potential dependent variables
#' main animal disease problem: q34
#' animal disease in the last 2 weeks: q36


#' Potential independent variables
#' manure management: q32
#' drugs used in last 2 months: setofg15 - 18
#' protection of animals from disease: q41-42
#' animal health services: q43, 49
#' use of (antimcrobial) vet drugs: q51 + cattle1-10 + smallrmnt1-4 + poultry1-5 + pig1-7
#' purpose of vaccination and AMU: q64 - q65
#' expenditure on vaccination, vitamins, antibiotics: q75

#'Potetial control variables
#' Type and number of animals, production system: q24-26, 28, 30





# Add vars ----------------------------------------------------------------

#checking number of farms with each kind of livestock
df <- as.data.frame(df)
dfcattle <- df[df$q22_1 == 1 & !is.na(df$q22_1)|df$q22_2 == 1 & !is.na(df$q22_2)|df$q22_3 == 1 & !is.na(df$q22_3)|df$q22_4 == 1 & !is.na(df$q22_4),]
dfruminant <- df[df$q22_1 == 2 & !is.na(df$q22_1)|df$q22_2 == 2 & !is.na(df$q22_2)|df$q22_3 == 2 & !is.na(df$q22_3)|df$q22_4 == 2 & !is.na(df$q22_4),]
dfpoultry <- df[df$q22_1 == 3 & !is.na(df$q22_1)|df$q22_2 == 3 & !is.na(df$q22_2)|df$q22_3 == 3 & !is.na(df$q22_3)|df$q22_4 == 3 & !is.na(df$q22_4),]
dfpigs <- df[df$q22_1 == 4 & !is.na(df$q22_1)|df$q22_2 == 4 & !is.na(df$q22_2)|df$q22_3 == 4 & !is.na(df$q22_3)|df$q22_4 == 4 & !is.na(df$q22_4),]

#217 had cattle, 248 had ruminants, 326 had poultry, 469 had pigs

#create a var for having each type of animal
df$has_cattle <- 0
df$has_ruminant <- 0
df$has_poultry <- 0
df$has_pigs <- 0

df$has_cattle[df$q22_1 == 1 & !is.na(df$q22_1)|df$q22_2 == 1 & !is.na(df$q22_2)|df$q22_3 == 1 & !is.na(df$q22_3)|df$q22_4 == 1 & !is.na(df$q22_4)] <- 1
df$has_ruminant
df$has_poultry
df$has_pigs