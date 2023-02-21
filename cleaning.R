

# Load Packages -----------------------------------------------------------
library("haven")
library("here")
library("plyr")
library("labelled")
library("xlsx")


# Load dataset ------------------------------------------------------------

df <- read_dta(here::here("AMUSEFARMERAW.dta"))



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
# df <- as.data.frame(df)
# dfcattle <- df[df$q22_1 == 1 & !is.na(df$q22_1)|df$q22_2 == 1 & !is.na(df$q22_2)|df$q22_3 == 1 & !is.na(df$q22_3)|df$q22_4 == 1 & !is.na(df$q22_4),]
# dfruminant <- df[df$q22_1 == 2 & !is.na(df$q22_1)|df$q22_2 == 2 & !is.na(df$q22_2)|df$q22_3 == 2 & !is.na(df$q22_3)|df$q22_4 == 2 & !is.na(df$q22_4),]
# dfpoultry <- df[df$q22_1 == 3 & !is.na(df$q22_1)|df$q22_2 == 3 & !is.na(df$q22_2)|df$q22_3 == 3 & !is.na(df$q22_3)|df$q22_4 == 3 & !is.na(df$q22_4),]
# dfpigs <- df[df$q22_1 == 4 & !is.na(df$q22_1)|df$q22_2 == 4 & !is.na(df$q22_2)|df$q22_3 == 4 & !is.na(df$q22_3)|df$q22_4 == 4 & !is.na(df$q22_4),]

#217 had cattle, 248 had ruminants, 326 had poultry, 469 had pigs

# df$pigstest <- 0
# df$pigstest[df$q22_1 == 4 & !is.na(df$q22_1)|df$q22_2 == 4 & !is.na(df$q22_2)|df$q22_3 == 4 & !is.na(df$q22_3)|df$q22_4 == 4 & !is.na(df$q22_4)] <- 1
# df$num_pigs_2 <- df$q24_boars + df$q24_piglets + df$q24_sows + df$q24_growers
# df$has_pigs_2 <- 0
# df$has_pigs_2[df$num_pigs_2 > 0 & !is.na(df$num_pigs_2)] <- 1

#create a var for flock / herd size
df$num_pigs <- 0
df$num_chickens <- 0
df$num_ruminants <- 0
df$num_cattle <- 0

df$num_pigs <- df$q24_boars + df$q24_piglets + df$q24_sows + df$q24_growers
df$num_chickens <- df$q24_poultry
df$num_ruminants <- df$q24_rumnt_female + df$q24_rumnt_male + df$q24_rumnt_young
df$num_cattle <- df$q24_cattle_adltfemale + df$q24_cattle_adltmale + df$q24_cattle_heifer

#create a var for having each type of animal
df$has_cattle <- 0
df$has_ruminant <- 0
df$has_poultry <- 0
df$has_pigs <- 0

df$has_cattle[df$num_cattle > 0 & !is.na(df$num_cattle)] <- 1
df$has_ruminant[df$num_ruminants > 0 & !is.na(df$num_ruminants)] <- 1
df$has_poultry[df$num_chickens > 0 & !is.na(df$num_chickens)] <- 1
df$has_pigs[df$num_pigs > 0 & !is.na(df$num_pigs)] <- 1

#'216 had cattle, 347 had pigs, 326 had chickens, and 247 had ruminants

# df$has_cattle[df$q22_1 == 1 & !is.na(df$q22_1)|df$q22_2 == 1 & !is.na(df$q22_2)|df$q22_3 == 1 & !is.na(df$q22_3)|df$q22_4 == 1 & !is.na(df$q22_4)] <- 1
# df$has_ruminant[df$q22_1 == 2 & !is.na(df$q22_1)|df$q22_2 == 2 & !is.na(df$q22_2)|df$q22_3 == 2 & !is.na(df$q22_3)|df$q22_4 == 2 & !is.na(df$q22_4)] <- 1
# df$has_poultry[df$q22_1 == 3 & !is.na(df$q22_1)|df$q22_2 == 3 & !is.na(df$q22_2)|df$q22_3 == 3 & !is.na(df$q22_3)|df$q22_4 == 3 & !is.na(df$q22_4)] <- 1
# df$has_pigs[df$q22_1 == 4 & !is.na(df$q22_1)|df$q22_2 == 4 & !is.na(df$q22_2)|df$q22_3 == 4 & !is.na(df$q22_3)|df$q22_4 == 4 & !is.na(df$q22_4)] <- 1

#create a var for having had illness in the last 2 weeks (for each animal species)
df$cattle_disease <- 0
df$ruminant_disease <- 0
df$poultry_disease <- 0
df$pig_disease <- 0

df$q36_cattle_other[df$q36_cattle_other == ""] <- NA
df$q36_cattle_other[df$q36_cattle_other == "."] <- NA
df$cattle_disease[!is.na(df$q36_cattle_1) | !is.na(df$q36_cattle_2) | !is.na(df$q36_cattle_other)] <- 1

df$q36_pig_other[df$q36_pig_other == ""] <- NA
df$q36_pig_other[df$q36_pig_other == "."] <- NA
df$pig_disease[!is.na(df$q36_pig_1) | !is.na(df$q36_pig_2) | !is.na(df$q36_pig_3) | !is.na(df$q36_pig_other)] <- 1

df$q36_poultry_other[df$q36_poultry_other == ""] <- NA
df$q36_poultry_other[df$q36_poultry_other == "."] <- NA
df$poultry_disease[!is.na(df$q36_poultry_1) | !is.na(df$q36_poultry_2) | !is.na(df$q36_poultry_3) | !is.na(df$q36_poultry_other)] <- 1

df$q36_smallrumnt_other[df$q36_smallrumnt_other == ""] <- NA
df$q36_smallrumnt_other[df$q36_smallrumnt_other == "."] <- NA
df$ruminant_disease[!is.na(df$q36_smallrumnt_1) | !is.na(df$q36_smallrumnt_2) | !is.na(df$q36_smallrumnt_3) | !is.na(df$q36_smallrumnt_other)] <- 1

#create vars for prophylactic AMU
#' every farm used AMU for at least one purpose, and most used it therapeutically. Of the ones who used it 
#' preventatively, most also used it therapeutically
#' probably best to just look at whether or not it was used preventatively at all

df$proph_amu <- 0
df$proph_amu[df$q65_1 == 2 | df$q65_1 == 3 | df$q65_2 == 2 | df$q65_2 == 3] <- 1

#create var for prophylactic vaccination
#' the overwhelming majority of farms used vaccination, and the overwhelming majority of those used it
#' preventatively, with most using it exclusively preventatively
#' if we look at any farmers who used vaccinations prevenatively, that will be almost everyone, so
#' we will look at those who used it exclusively preventatively

df$proph_vacc <- 0
df$proph_vacc[df$q64_1 == 2 | df$q64_2 == 2 | df$q64_2 == 3 ] <- 1

#create vars for expenditure on vaccination, vitamins, abx
df$cattle_expense_dewormer <- df$q75_cattle_dewormer
df$cattle_expense_dewormer[df$cattle_expense_dewormer == -98] <- NA

df$cattle_expense_acaracide <- df$q75_cattle_acaracides
df$cattle_expense_acaracide[df$cattle_expense_acaracide == -98] <- NA

df$q75_cattle_antibiotics[df$q75_cattle_antibiotics == -98] <- NA
df$cattle_expense_antibiotics <- df$q75_cattle_antibiotics

df$q75_cattle_vaccination[df$q75_cattle_vaccination == -98] <- NA
df$cattle_expense_vaccination <- df$q75_cattle_vaccination

df$q75_cattle_vitamins[df$q75_cattle_vitamins == -98] <- NA
df$cattle_expense_vitamins <- df$q75_cattle_vitamins

df$cattle_expense_othermed <- df$cattle_expense_acaracide + df$cattle_expense_dewormer + df$cattle_expense_vitamins



df$pigs_expense_dewormer <- df$q75_pigs_dewormer
df$pigs_expense_dewormer[df$pigs_expense_dewormer == -98] <- NA

df$pigs_expense_acaracide <- df$q75_pigs_acaracides
df$pigs_expense_acaracide[df$pigs_expense_acaracide == -98] <- NA

df$q75_pigs_antibiotics[df$q75_pigs_antibiotics == -98] <- NA
df$pigs_expense_antibiotics <- df$q75_pigs_antibiotics

df$q75_pigs_vaccination[df$q75_pigs_vaccination == -98] <- NA
df$pigs_expense_vaccination <- df$q75_pigs_vaccination

df$q75_pigs_vitamins[df$q75_pigs_vitamins == -98] <- NA
df$pigs_expense_vitamins <- df$q75_pigs_vitamins

df$pigs_expense_othermed <- df$pigs_expense_acaracide + df$pigs_expense_dewormer + df$pigs_expense_vitamins



df$poultry_expense_dewormer <- df$q75_poultry_dewormer
df$poultry_expense_dewormer[df$poultry_expense_dewormer == -98] <- NA

df$poultry_expense_acaracide <- df$q75_poultry_acaracides
df$poultry_expense_acaracide[df$poultry_expense_acaracide == -98] <- NA

df$q75_poultry_antibiotics[df$q75_poultry_antibiotics == -98] <- NA
df$poultry_expense_antibiotics <- df$q75_poultry_antibiotics

df$q75_poultry_vaccination[df$q75_poultry_vaccination == -98] <- NA
df$poultry_expense_vaccination <- df$q75_poultry_vaccination

df$q75_poultry_vitamins[df$q75_poultry_vitamins == -98] <- NA
df$poultry_expense_vitamins <- df$q75_poultry_vitamins

df$poultry_expense_othermed <- df$poultry_expense_acaracide + df$poultry_expense_dewormer + df$poultry_expense_vitamins



df$ruminant_expense_dewormer <- df$q75_smallrumnt_dewormer
df$ruminant_expense_dewormer[df$ruminant_expense_dewormer == -98] <- NA

df$ruminant_expense_acaracide <- df$q75_smallrumnt_acaracides
df$ruminant_expense_acaracide[df$ruminant_expense_acaracide == -98] <- NA

df$q75_smallrumnt_antibiotics[df$q75_smallrumnt_antibiotics == -98] <- NA
df$ruminant_expense_antibiotics <- df$q75_smallrumnt_antibiotics

df$q75_smallrumnt_vaccination[df$q75_smallrumnt_vaccination == -98] <- NA
df$ruminant_expense_vaccination <- df$q75_smallrumnt_vaccination

df$q75_smallrumnt_vitamins[df$q75_smallrumnt_vitamins == -98] <- NA
df$ruminant_expense_vitamins <- df$q75_smallrumnt_vitamins

df$ruminant_expense_othermed <- df$ruminant_expense_acaracide + df$ruminant_expense_dewormer + df$ruminant_expense_vitamins

#create var for having biosecurity measures in q42 (1, 2, 6, 7, 9, 10, 11, 12, 13)
df$cattle_biosecurity <- 0
df$poultry_biosecurity <- 0
df$pig_biosecurity <- 0
df$ruminant_biosecurity <- 0

df$cattle_biosecurity[df$q42_cattle_1 == 1 | df$q42_cattle_1 == 2 | df$q42_cattle_1 == 6 |
                        df$q42_cattle_1 == 7 | df$q42_cattle_1 == 9 | df$q42_cattle_1 == 10 |
                        df$q42_cattle_1 == 11 | df$q42_cattle_1 == 12 | df$q42_cattle_1 == 13 |
                        
                        df$q42_cattle_2 == 1 | df$q42_cattle_2 == 2 | df$q42_cattle_2 == 6 |
                        df$q42_cattle_2 == 7 | df$q42_cattle_2 == 9 | df$q42_cattle_2 == 10 |
                        df$q42_cattle_2 == 11 | df$q42_cattle_2 == 12 | df$q42_cattle_2 == 13 |
                        
                        df$q42_cattle_3 == 1 | df$q42_cattle_3 == 2 | df$q42_cattle_3 == 6 |
                        df$q42_cattle_3 == 7 | df$q42_cattle_3 == 9 | df$q42_cattle_3 == 10 |
                        df$q42_cattle_3 == 11 | df$q42_cattle_3 == 12 | df$q42_cattle_3 == 13] <- 1

df$ruminant_biosecurity[df$q42_smallrumnt_1 == 1 | df$q42_smallrumnt_1 == 2 | df$q42_smallrumnt_1 == 6 |
                        df$q42_smallrumnt_1 == 7 | df$q42_smallrumnt_1 == 9 | df$q42_smallrumnt_1 == 10 |
                        df$q42_smallrumnt_1 == 11 | df$q42_smallrumnt_1 == 12 | df$q42_smallrumnt_1 == 13 |
                        
                        df$q42_smallrumnt_2 == 1 | df$q42_smallrumnt_2 == 2 | df$q42_smallrumnt_2 == 6 |
                        df$q42_smallrumnt_2 == 7 | df$q42_smallrumnt_2 == 9 | df$q42_smallrumnt_2 == 10 |
                        df$q42_smallrumnt_2 == 11 | df$q42_smallrumnt_2 == 12 | df$q42_smallrumnt_2 == 13 |
                        
                        df$q42_smallrumnt_3 == 1 | df$q42_smallrumnt_3 == 2 | df$q42_smallrumnt_3 == 6 |
                        df$q42_smallrumnt_3 == 7 | df$q42_smallrumnt_3 == 9 | df$q42_smallrumnt_3 == 10 |
                        df$q42_smallrumnt_3 == 11 | df$q42_smallrumnt_3 == 12 | df$q42_smallrumnt_3 == 13] <- 1

df$pig_biosecurity[df$q42_pig_1 == 1 | df$q42_pig_1 == 2 | df$q42_pig_1 == 6 |
                        df$q42_pig_1 == 7 | df$q42_pig_1 == 9 | df$q42_pig_1 == 10 |
                        df$q42_pig_1 == 11 | df$q42_pig_1 == 12 | df$q42_pig_1 == 13 |
                        
                        df$q42_pig_2 == 1 | df$q42_pig_2 == 2 | df$q42_pig_2 == 6 |
                        df$q42_pig_2 == 7 | df$q42_pig_2 == 9 | df$q42_pig_2 == 10 |
                        df$q42_pig_2 == 11 | df$q42_pig_2 == 12 | df$q42_pig_2 == 13 |
                        
                        df$q42_pig_3 == 1 | df$q42_pig_3 == 2 | df$q42_pig_3 == 6 |
                        df$q42_pig_3 == 7 | df$q42_pig_3 == 9 | df$q42_pig_3 == 10 |
                        df$q42_pig_3 == 11 | df$q42_pig_3 == 12 | df$q42_pig_3 == 13 |
                        
                        df$q42_pig_4 == 1 | df$q42_pig_4 == 2 | df$q42_pig_4 == 6 |
                        df$q42_pig_4 == 7 | df$q42_pig_4 == 9 | df$q42_pig_4 == 10 |
                        df$q42_pig_4 == 11 | df$q42_pig_4 == 12 | df$q42_pig_4 == 13] <- 1

df$poultry_biosecurity[df$q42_poultry_1 == 1 | df$q42_poultry_1 == 2 | df$q42_poultry_1 == 6 |
                     df$q42_poultry_1 == 7 | df$q42_poultry_1 == 9 | df$q42_poultry_1 == 10 |
                     df$q42_poultry_1 == 11 | df$q42_poultry_1 == 12 | df$q42_poultry_1 == 13 |
                     
                     df$q42_poultry_2 == 1 | df$q42_poultry_2 == 2 | df$q42_poultry_2 == 6 |
                     df$q42_poultry_2 == 7 | df$q42_poultry_2 == 9 | df$q42_poultry_2 == 10 |
                     df$q42_poultry_2 == 11 | df$q42_poultry_2 == 12 | df$q42_poultry_2 == 13 |
                     
                     df$q42_poultry_3 == 1 | df$q42_poultry_3 == 2 | df$q42_poultry_3 == 6 |
                     df$q42_poultry_3 == 7 | df$q42_poultry_3 == 9 | df$q42_poultry_3 == 10 |
                     df$q42_poultry_3 == 11 | df$q42_poultry_3 == 12 | df$q42_poultry_3 == 13 |
                     
                     df$q42_poultry_4 == 1 | df$q42_poultry_4 == 2 | df$q42_poultry_4 == 6 |
                     df$q42_poultry_4 == 7 | df$q42_poultry_4 == 9 | df$q42_poultry_4 == 10 |
                     df$q42_poultry_4 == 11 | df$q42_poultry_4 == 12 | df$q42_poultry_4 == 13] <- 1

#create var for using herbs or trad medicine (q42 = 5)

df$cattle_tradmed <- 0
df$pig_tradmed <- 0
df$poultry_tradmed <- 0
df$ruminant_tradmed <- 0

df$cattle_tradmed[df$q42_cattle_1 == 8 | df$q42_cattle_2 == 8 | df$q42_cattle_3 == 8] <- 1
df$ruminant_tradmed[df$q42_smallrumnt_1 == 8 | df$q42_smallrumnt_2 == 8 | df$q42_smallrumnt_3 == 8] <- 1
df$pig_tradmed[df$q42_pig_1 == 8 | df$q42_pig_2 == 8 | df$q42_pig_3 == 8 | df$q42_pig_4 == 8] <- 1
df$poultry_tradmed[df$q42_poultry_1 == 8 | df$q42_poultry_2 == 8 | df$q42_poultry_3 == 8 | df$q42_poultry_4 == 8] <- 1

#way too few farms used this for us to be able to investigate it

#create a var for using animal health services
df$animal_health_services <- 0
df$animal_health_services[df$q43 == 1] <- 1

# create separate dfs -----------------------------------------------------

# dfcattle <- df[df$q22_1 == 1 & !is.na(df$q22_1)|df$q22_2 == 1 & !is.na(df$q22_2)|df$q22_3 == 1 & !is.na(df$q22_3)|df$q22_4 == 1 & !is.na(df$q22_4),]
# dfruminant <- df[df$q22_1 == 2 & !is.na(df$q22_1)|df$q22_2 == 2 & !is.na(df$q22_2)|df$q22_3 == 2 & !is.na(df$q22_3)|df$q22_4 == 2 & !is.na(df$q22_4),]
# dfpoultry <- df[df$q22_1 == 3 & !is.na(df$q22_1)|df$q22_2 == 3 & !is.na(df$q22_2)|df$q22_3 == 3 & !is.na(df$q22_3)|df$q22_4 == 3 & !is.na(df$q22_4),]
# dfpigs <- df[df$q22_1 == 4 & !is.na(df$q22_1)|df$q22_2 == 4 & !is.na(df$q22_2)|df$q22_3 == 4 & !is.na(df$q22_3)|df$q22_4 == 4 & !is.na(df$q22_4),]

dfcattle <- df[df$has_cattle == 1,]
dfruminant <- df[df$has_ruminant == 1,]
dfpoultry <- df[df$has_poultry == 1,]
dfpigs <- df[df$has_pigs == 1,]

# Explore our main vars ---------------------------------------------------
table(df$animal_health_services)
table(df$proph_amu)
table(df$proph_vacc)

table(dfcattle$cattle_biosecurity)
table(dfpigs$pig_biosecurity)
table(dfruminant$ruminant_biosecurity)
table(dfpoultry$poultry_biosecurity)

table(dfcattle$cattle_disease)
table(dfpigs$pig_disease)
table(dfruminant$ruminant_disease)
table(dfpoultry$poultry_disease)

summary(dfcattle$cattle_expense_antibiotics / 3.31)
summary(dfpigs$pigs_expense_antibiotics / 4.92)
summary(dfruminant$ruminant_expense_antibiotics / 4.65)
summary(dfpoultry$poultry_expense_antibiotics / 25.75)

summary(dfpigs$pigs_expense_vaccination / 3.31)
summary(dfcattle$cattle_expense_vaccination / 4.92)
summary(dfruminant$ruminant_expense_vaccination / 4.65)
summary(dfpoultry$poultry_expense_vaccination / 25.75)

summary(dfcattle$cattle_expense_othermed / 3.31)
summary(dfpigs$pigs_expense_othermed / 4.92)
summary(dfruminant$ruminant_expense_othermed / 4.65)
summary(dfpoultry$poultry_expense_othermed / 25.75)

summary(dfcattle$num_cattle)
summary(dfpigs$num_pigs)
summary(dfruminant$num_ruminants)
summary(dfpoultry$num_chickens)


# Create compact dataset for analysis -------------------------------------
rm(dfcattle, dfpigs, dfpoultry, dfruminant)
which( colnames(df)=="num_pigs" )
df <- df[,c(309:355)]

dfcattle <- df[df$has_cattle == 1,]
dfpigs <- df[df$has_pigs == 1,]
dfruminants <- df[df$has_ruminant == 1,]
dfpoultry <- df[df$has_poultry == 1,]

#cattle
#' we want to keep: cattle_disease, proph_amu, proph_vacc, cattle_expense_antibiotics,
#' cattle_expense_vaccination, cattle_expense_othermed, cattle_biosecurity, num_cattle

dfcattle <- dfcattle[,c("cattle_disease", "proph_amu", "proph_vacc", "cattle_expense_antibiotics",
                        "cattle_expense_vaccination", "cattle_expense_othermed", "cattle_biosecurity",
                        "num_cattle", "animal_health_services")]

#add a var for species
dfcattle$species <- "cattle"

#rename vars
dfcattle <- plyr::rename(dfcattle,
                         c("cattle_disease" = "disease",
                           "cattle_expense_antibiotics" = "expense_antibiotics",
                           "cattle_expense_vaccination" = "expense_vaccination",
                           "cattle_biosecurity" = "biosecurity",
                           "num_cattle" = "num_animals",
                           "cattle_expense_othermed" = "expense_othermed"))

#express expenditures as expenditure per animal
dfcattle$expense_vaccination <- dfcattle$expense_vaccination / dfcattle$num_animals
dfcattle$expense_antibiotics <- dfcattle$expense_antibiotics / dfcattle$num_animals
dfcattle$expense_othermed <- dfcattle$expense_othermed / dfcattle$num_animals

#pigs
#'we want to keep: pig_disease, proph_amu, proph_vacc, pigs_expense_antibiotics,
#'pigs_expense_vaccination, pigs_expense_othermed, pig_biosecurity, num_pigs
  
dfpigs <- dfpigs[,c("pig_disease", "proph_amu", "proph_vacc", "pigs_expense_antibiotics",
                        "pigs_expense_vaccination", "pigs_expense_othermed", "pig_biosecurity",
                        "num_pigs", "animal_health_services")]

#add a var for species
dfpigs$species <- "pigs"  

#rename vars
dfpigs <- plyr::rename(dfpigs,
                         c("pig_disease" = "disease",
                           "pigs_expense_antibiotics" = "expense_antibiotics",
                           "pigs_expense_vaccination" = "expense_vaccination",
                           "pig_biosecurity" = "biosecurity",
                           "num_pigs" = "num_animals",
                           "pigs_expense_othermed" = "expense_othermed"))

#express expenditures as expenditure per animal
dfpigs$expense_vaccination <- dfpigs$expense_vaccination / dfpigs$num_animals
dfpigs$expense_antibiotics <- dfpigs$expense_antibiotics / dfpigs$num_animals
dfpigs$expense_othermed <- dfpigs$expense_othermed / dfpigs$num_animals


#poultry
#'we want to keep: poultry_disease, proph_amu, proph_vacc, poultry_expense_antibiotics,
#'poultry_expense_vaccination, poultry_expense_othermed, poultry_biosecurity, num_chickens

dfpoultry <- dfpoultry[,c("poultry_disease", "proph_amu", "proph_vacc", "poultry_expense_antibiotics",
                    "poultry_expense_vaccination", "poultry_expense_othermed", "poultry_biosecurity",
                    "num_chickens", "animal_health_services")]

#add a var for species
dfpoultry$species <- "chickens"

#rename vars
dfpoultry <- plyr::rename(dfpoultry,
                       c("poultry_disease" = "disease",
                         "poultry_expense_antibiotics" = "expense_antibiotics",
                         "poultry_expense_vaccination" = "expense_vaccination",
                         "poultry_biosecurity" = "biosecurity",
                         "num_chickens" = "num_animals",
                         "poultry_expense_othermed" = "expense_othermed"))

#express expenditures as expenditure per animal
dfpoultry$expense_vaccination <- dfpoultry$expense_vaccination / dfpoultry$num_animals
dfpoultry$expense_antibiotics <- dfpoultry$expense_antibiotics / dfpoultry$num_animals
dfpoultry$expense_othermed <- dfpoultry$expense_othermed / dfpoultry$num_animals


#ruminants
#'we want to keep: ruminant_disease, proph_amu, proph_vacc, ruminant_expense_antibiotics,
#'ruminant_expense_vaccination, ruminant_expense_othermed, ruminant_biosecurity, num_ruminants

dfruminants <- dfruminants[,c("ruminant_disease", "proph_amu", "proph_vacc", "ruminant_expense_antibiotics",
                          "ruminant_expense_vaccination", "ruminant_expense_othermed", "ruminant_biosecurity",
                          "num_ruminants", "animal_health_services")]

#add a var for species
dfruminants$species <- "ruminants"

dfruminants <- plyr::rename(dfruminants,
                          c("ruminant_disease" = "disease",
                            "ruminant_expense_antibiotics" = "expense_antibiotics",
                            "ruminant_expense_vaccination" = "expense_vaccination",
                            "ruminant_biosecurity" = "biosecurity",
                            "num_ruminants" = "num_animals",
                            "ruminant_expense_othermed" = "expense_othermed"))

#express expenditures as expenditure per animal
dfruminants$expense_vaccination <- dfruminants$expense_vaccination / dfruminants$num_animals
dfruminants$expense_antibiotics <- dfruminants$expense_antibiotics / dfruminants$num_animals
dfruminants$expense_othermed <- dfruminants$expense_othermed / dfruminants$num_animals

#remove labels from 4 datasets
dfcattle <- remove_labels(dfcattle)
dfpigs <- remove_labels(dfpigs)
dfpoultry <- remove_labels(dfpoultry)
dfruminants <- remove_labels(dfruminants)

#merge datasets
rm(df)
df <- rbind(dfcattle, dfpigs, dfpoultry, dfruminants)
rm(dfcattle, dfpigs, dfpoultry, dfruminants)

#where appropriate, set NAs and NaN to zero. Then, set silly values like infinity and negative numbers to NA
is.na(df$expense_antibiotics)
table(df$expense_antibiotics)
df$expense_antibiotics[is.na(df$expense_antibiotics)] <- 0
df$expense_antibiotics[df$expense_antibiotics < 0] <- NA

table(df$expense_vaccination)
is.na(df$expense_vaccination)
df$expense_vaccination[is.na(df$expense_vaccination)] <- 0
df$expense_vaccination[df$expense_vaccination < 0] <- NA

table(df$expense_othermed)
is.na(df$expense_othermed)
df$expense_othermed[is.na(df$expense_othermed)] <- 0
df$expense_othermed[df$expense_othermed < 0] <- NA

#remove outlier expenditures
dfcattle <- df[df$species == "cattle",]
dfchicken <- df[df$species == "chickens",]
dfpig <- df[df$species == "pigs",]
dfruminant <- df[df$species == "ruminants",]

hist(dfcattle$expense_antibiotics)
nrow(dfcattle[dfcattle$expense_antibiotics > 300000, ]) #1
nrow(dfcattle[dfcattle$expense_antibiotics > 200000, ]) #2
nrow(dfcattle[dfcattle$expense_antibiotics > 150000, ]) #2
nrow(dfcattle[dfcattle$expense_antibiotics > 100000, ]) #5
#remove above 200,000
df$expense_antibiotics[df$species == "cattle" & df$expense_antibiotics > 200000] <- NA

hist(dfchicken$expense_antibiotics)
nrow(dfchicken[dfchicken$expense_antibiotics > 5000, ]) #2
nrow(dfchicken[dfchicken$expense_antibiotics > 4000, ]) #2
nrow(dfchicken[dfchicken$expense_antibiotics > 3000, ]) #2
nrow(dfchicken[dfchicken$expense_antibiotics > 2000, ]) #6
nrow(dfchicken[dfchicken$expense_antibiotics > 1000, ]) #18
#remove above 3,000
df$expense_antibiotics[df$species == "chickens" & df$expense_antibiotics > 3000] <- NA

hist(dfruminant$expense_antibiotics)
nrow(dfruminant[dfruminant$expense_antibiotics > 150000, ]) #1
nrow(dfruminant[dfruminant$expense_antibiotics > 100000, ]) #3
nrow(dfruminant[dfruminant$expense_antibiotics > 50000, ]) #4
nrow(dfruminant[dfruminant$expense_antibiotics > 30000, ]) #7
#remove above 100,000
df$expense_antibiotics[df$species == "ruminant" & df$expense_antibiotics > 100000] <- NA

hist(dfpig$expense_antibiotics)
nrow(dfpig[dfpig$expense_antibiotics > 100000,]) #2
nrow(dfpig[dfpig$expense_antibiotics > 50000,]) #3
nrow(dfpig[dfpig$expense_antibiotics > 25000,]) #20
#remove above 100,000
df$expense_antibiotics[df$species == "pigs" & df$expense_antibiotics > 100000] <- NA



hist(dfcattle$expense_vaccination)
nrow(dfcattle[dfcattle$expense_vaccination > 125000,]) #1
nrow(dfcattle[dfcattle$expense_vaccination > 100000,]) #2
nrow(dfcattle[dfcattle$expense_vaccination > 50000,]) #4
nrow(dfcattle[dfcattle$expense_vaccination > 25000,]) #6
nrow(dfcattle[dfcattle$expense_vaccination > 10000,]) #11
nrow(dfcattle[dfcattle$expense_vaccination > 5000,]) #11
nrow(dfcattle[dfcattle$expense_vaccination > 3000,]) #17
nrow(dfcattle[dfcattle$expense_vaccination > 1000,]) #23
#remove above 100,000
df$expense_vaccination[df$species == "cattle" & df$expense_vaccination > 100000] <- NA

hist(dfpig$expense_vaccination)
nrow(dfpig[dfpig$expense_vaccination > 1000,]) #35
nrow(dfpig[dfpig$expense_vaccination > 5000,]) #24
nrow(dfpig[dfpig$expense_vaccination > 10000,]) #16
nrow(dfpig[dfpig$expense_vaccination > 50000,]) #2
nrow(dfpig[dfpig$expense_vaccination > 100000,]) #2
#remove above 100,000
df$expense_vaccination[df$species == "pigs" & df$expense_vaccination > 100000] <- NA

hist(dfchicken$expense_vaccination)
nrow(dfchicken[dfchicken$expense_vaccination > 100000,]) #1
nrow(dfchicken[dfchicken$expense_vaccination > 50000,]) #1
nrow(dfchicken[dfchicken$expense_vaccination > 10000,]) #2
nrow(dfchicken[dfchicken$expense_vaccination > 5000,]) #7
nrow(dfchicken[dfchicken$expense_vaccination > 1000,]) #38
#remove above 100,000
df$expense_vaccination[df$species == "chickens" & df$expense_vaccination > 100000] <- NA

hist(dfruminant$expense_vaccination)
nrow(dfruminant[dfruminant$expense_vaccination > 5000,]) #6
nrow(dfruminant[dfruminant$expense_vaccination > 10000,]) #4
nrow(dfruminant[dfruminant$expense_vaccination > 25000,]) #4
nrow(dfruminant[dfruminant$expense_vaccination > 50000,]) #1
#remove above 50,000
df$expense_vaccination[df$species == "ruminants" & df$expense_vaccination > 50000] <- NA



hist(dfcattle$expense_othermed)
nrow(dfcattle[dfcattle$expense_othermed > 10000,]) #70
nrow(dfcattle[dfcattle$expense_othermed > 100000,]) #7
nrow(dfcattle[dfcattle$expense_othermed > 200000,]) #5
nrow(dfcattle[dfcattle$expense_othermed > 300000,]) #5
nrow(dfcattle[dfcattle$expense_othermed > 400000,]) #3
nrow(dfcattle[dfcattle$expense_othermed > 450000,]) #2
nrow(dfcattle[dfcattle$expense_othermed > 500000,]) #0
#have decided to leave all responses in

hist(dfpig$expense_othermed)
nrow(dfpig[dfpig$expense_othermed > 100000,]) #6
nrow(dfpig[dfpig$expense_othermed > 150000,]) #2
nrow(dfpig[dfpig$expense_othermed > 200000,]) #1
#remove all over 200,000
df$expense_othermed[df$species == "pigs" & df$expense_othermed > 200000] <- NA

hist(dfchicken$expense_othermed)
nrow(dfchicken[dfchicken$expense_othermed > 10000,]) #1
nrow(dfchicken[dfchicken$expense_othermed > 5000,]) #2
nrow(dfchicken[dfchicken$expense_othermed > 2000,]) #4
nrow(dfchicken[dfchicken$expense_othermed > 1000,]) #10
nrow(dfchicken[dfchicken$expense_othermed > 500,]) #22
#remove above 10,000
df$expense_othermed[df$species == "chickens" & df$expense_othermed > 10000] <- NA

hist(dfruminant$expense_othermed)
nrow(dfruminant[dfruminant$expense_othermed > 10000,]) #25
nrow(dfruminant[dfruminant$expense_othermed > 25000,]) #13
nrow(dfruminant[dfruminant$expense_othermed > 50000,]) #7
nrow(dfruminant[dfruminant$expense_othermed > 100000,]) #2
nrow(dfruminant[dfruminant$expense_othermed > 150000,]) #2
#remove above 100,000
df$expense_othermed[df$species == "ruminants" & df$expense_othermed > 100000] <- NA

#divide expenditure by 1,000
rm(dfcattle, dfchicken, dfruminant, dfpig)
df$expense_antibiotics <- df$expense_antibiotics / 1000
df$expense_vaccination <- df$expense_vaccination / 1000
df$expense_othermed <- df$expense_othermed / 1000

#save dataset
write.xlsx(df, "cleaned dataset.xlsx", replace)
