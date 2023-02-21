
# load packages -----------------------------------------------------------

library("stargazer")
library("here")
library("xlsx")
library("readxl")
library("ggplot2")
library("patchwork")
library("sjPlot")


# create outputs folder ---------------------------------------------------

main_dir <- here::here()
sub_dir <- "Outputs"
ifelse(!dir.exists(file.path(main_dir, sub_dir)), dir.create(file.path(main_dir, sub_dir)), F)
rm("main_dir", "sub_dir")

# load dataset ------------------------------------------------------------

df <- read.xlsx(here::here("cleaned dataset.xlsx"), 1,
                as.data.frame = T, header = T)

df <- subset(df, select = -c(1))

# impact of practices -----------------------------------------------------

reg1 <- glm(disease ~ proph_amu + proph_vacc + animal_health_services + biosecurity + num_animals,
            family = "binomial", data = subset(df, species == "cattle"))
reg2 <- glm(disease ~ proph_amu + proph_vacc + animal_health_services + biosecurity + num_animals,
            family = "binomial", data = subset(df, species == "pigs"))
reg3 <- glm(disease ~ proph_amu + proph_vacc + animal_health_services + biosecurity + num_animals,
            family = "binomial", data = subset(df, species == "ruminants"))
reg4 <- glm(disease ~ proph_amu + proph_vacc + animal_health_services + biosecurity + num_animals,
            family = "binomial", data = subset(df, species == "chickens"))
reg5 <- glm(disease ~ proph_amu + proph_vacc + animal_health_services + biosecurity + num_animals,
            family = "binomial", data = df)

stargazer(reg1, reg2, reg3, reg4, reg5, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = "text", out = "Outputs/practices.csv",
          title = "Table 1: Effect of Practices on Disease Likelihood (Odds Ratio)",
          column.labels = c("Cattle", "Pigs", "Ruminants", "Chickens", "Whole Sample"),
          covariate.labels = c("Prophylactic AMU", "Prophylactic Vaccination",
                               "Access to Animal Health Services",
                               "On-Farm Biosecurity Measures",
                               "Number of Animals")
          )

stargazer(reg1, reg2, reg3, reg4, reg5, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = "html", out = "Outputs/practices.html",
          title = "Table 1: Effect of Practices on Disease Likelihood (Odds Ratio)",
          column.labels = c("Cattle", "Pigs", "Ruminants", "Chickens", "Whole Sample"),
          covariate.labels = c("Prophylactic AMU", "Prophylactic Vaccination",
                               "Access to Animal Health Services",
                               "On-Farm Biosecurity Measures",
                               "Number of Animals")
          )


# impact of expenditures --------------------------------------------------

reg6 <- glm(disease ~ expense_antibiotics + expense_vaccination + expense_othermed + num_animals,
            family = "binomial", data = subset(df, species == "cattle"))
reg7 <- glm(disease ~ expense_antibiotics + expense_vaccination + expense_othermed + num_animals,
            family = "binomial", data = subset(df, species == "pigs"))
reg8 <- glm(disease ~ expense_antibiotics + expense_vaccination + expense_othermed + num_animals,
            family = "binomial", data = subset(df, species == "ruminants"))
reg9 <- glm(disease ~ expense_antibiotics + expense_vaccination + expense_othermed + num_animals,
            family = "binomial", data = subset(df, species == "chickens"))
reg10 <- glm(disease ~ expense_antibiotics + expense_vaccination + expense_othermed + num_animals,
            family = "binomial", data = df)
 
stargazer(reg6, reg7, reg8, reg9, reg10, type = "text", out = "Outputs/expenditures.csv",
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          title = "Table 2: Effect of Expenditures on Disease Likelihood (odds ratio for additional 1000 UGX per animal per year)",
          column.labels = c("Cattle", "Pigs", "Ruminants", "Chickens", "Whole Sample"),
          covariate.labels = c("Annual Expenditure on Antibiotics per Animal",
                               "Annual Expenditure on Vaccination per Animal",
                               "Annual Expenditure on Other Medicines per Animal",
                               "Number of Animals")
          )

stargazer(reg6, reg7, reg8, reg9, reg10, type = "html", out = "Outputs/expenditures.html",
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          title = "Table 2: Effect of Expenditures on Disease Likelihood (odds ratio for additional 1000 UGX per animal per year)",
          column.labels = c("Cattle", "Pigs", "Ruminants", "Chickens", "Whole Sample"),
          covariate.labels = c("Annual Expenditure on Antibiotics per Animal",
                               "Annual Expenditure on Vaccination per Animal",
                               "Annual Expenditure on Other Medicines per Animal",
                               "Number of Animals")
          )


# simple correlations -----------------------------------------------------

dfchickens <- df[df$species == "chickens",]
dfchickens <- subset(dfchickens, select = -c(10))

dfcattle <- df[df$species == "cattle",]
dfcattle <- subset(dfcattle, select = -c(10))

dfpigs <- df[df$species == "pigs",]
dfpigs <- subset(dfpigs, select = -c(10))

dfruminants <- df[df$species == "ruminants",]
dfruminants <- subset(dfruminants, select = -c(10))

tab_corr(dfchickens, triangle = "lower",
         title = "Chickens",
         file = "Outputs/corrchickens.doc")

tab_corr(dfcattle, triangle = "lower",
         title = "Cattle",
         file = "Outputs/corrcattle.doc")

tab_corr(dfpigs, triangle = "lower",
         title = "Pigs",
         file = "Outputs/corrpigs.doc")

tab_corr(dfruminants, triangle = "lower",
         title = "Ruminants",
         file = "Outputs/corrruminants.doc")


