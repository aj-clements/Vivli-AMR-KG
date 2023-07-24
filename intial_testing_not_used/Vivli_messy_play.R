# script for playing around with ATLAS
library(data.table)
library(ggplot2)
library(scales)
library(tidyverse)
library(here)

# read in the data
#atlas <- read.csv("~/Desktop/atlas_input.csv")
atlas <- read.csv("data/Atlas_Reuse_Data.csv")
atlas <- data.table(atlas)

target_drug <- "Moxifloxacin"
cat_drug <- paste0(target_drug, "_I")
atlas_sa <- atlas[Species =="Staphylococcus aureus"]
table(atlas_sa[,..cat_drug]) 
names_check <- names(table(atlas_sa[,..cat_drug]) )
atlas_sa_sub <- atlas_sa[,c("Isolate.Id", "Study", "Country", "State", "Gender", "Age.Group", "Speciality", "Source", "Year", 
                            ..target_drug, ..cat_drug)]

f <- paste0("Source + Gender + Age.Group ~ ", cat_drug)
sa_atlas <- dcast(atlas_sa_sub, formula =  f, fun.aggregate = length)

if("Intermediate" %in% names_check){
  sa_atlas[, prop_R := (Intermediate+Resistant) / (Intermediate+Resistant+Susceptible)]
} else {sa_atlas[, prop_R := (Resistant) / (Resistant+Susceptible)]}

sa_atlas<- sa_atlas[!Gender=="", ]
sa_atlas$Age.Group <- factor(sa_atlas$Age.Group, 
                             levels = c(
                               "0 to 2 Years" ,"3 to 12 Years",
                               "13 to 18 Years", "19 to 64 Years",
                               "65 to 84 Years",
                               "85 and Over",    "Unknown"
                             )) 

source_over_1k <- names(table(atlas_sa_sub$Source)[which(table(atlas_sa_sub$Source)>1000)]) # include only those with more than 100 samples
sa_atlas <- sa_atlas[Source %in% source_over_1k]

SOURCE <- ggplot(sa_atlas, aes(x = Age.Group, y = prop_R, colour = Source, group = Source)) +
 geom_line() + 
 facet_grid(Gender~.) + 
  labs(title = paste0("Proportion S. Aureus resistant to", target_drug, " for major Source (over 1000 samples)")) + 
  theme_linedraw()

f <- paste0("Country + Gender + Age.Group ~ ", cat_drug)
sa_atlas <- dcast(atlas_sa_sub,  formula =  f, fun.aggregate = length)

if("Intermediate" %in% names_check){
  sa_atlas[, prop_R := (Intermediate+Resistant) / (Intermediate+Resistant+Susceptible)]
} else {sa_atlas[, prop_R := (Resistant) / (Resistant+Susceptible)]}

sa_atlas<- sa_atlas[!Gender=="", ]
sa_atlas$Age.Group <- factor(sa_atlas$Age.Group, 
                             levels = c(
                               "0 to 2 Years" ,"3 to 12 Years",
                               "13 to 18 Years", "19 to 64 Years",
                               "65 to 84 Years",
                               "85 and Over",    "Unknown"
                             )) 
if("Intermediate" %in% names_check){
  sa_atlas[,total_samples := Intermediate + Susceptible+Resistant]
  } else {sa_atlas[,total_samples := Susceptible+Resistant]}

COUNTRY <- ggplot(sa_atlas[total_samples>50], aes(x = Age.Group, y = prop_R, colour = Country, group = Country)) +
  geom_line() + 
  facet_grid(Gender~.) + 
  labs(title = paste0("Proportion S. Aureus resistant to ",target_drug, " by country (incl.only over 50 samples)")) + 
  theme_linedraw()

f <- paste0("Year + Gender + Age.Group ~ ", cat_drug)
sa_atlas <- dcast(atlas_sa_sub,  formula =  f, fun.aggregate = length)
if("Intermediate" %in% names_check){
  sa_atlas[, prop_R := (Intermediate+Resistant) / (Intermediate+Resistant+Susceptible)]
} else {sa_atlas[, prop_R := (Resistant) / (Resistant+Susceptible)]}
sa_atlas<- sa_atlas[!Gender=="", ]
sa_atlas$Age.Group <- factor(sa_atlas$Age.Group, 
                             levels = c(
                               "0 to 2 Years" ,"3 to 12 Years",
                               "13 to 18 Years", "19 to 64 Years",
                               "65 to 84 Years",
                               "85 and Over",    "Unknown"
                             )) 

if("Intermediate" %in% names_check){
  sa_atlas[,total_samples := Intermediate + Susceptible+Resistant]
} else {sa_atlas[,total_samples := Susceptible+Resistant]}

YEAR <- ggplot(sa_atlas[total_samples>50], aes(x = Age.Group, y = prop_R, colour = Year, group = Year)) +
  geom_line() + 
  facet_grid(Gender~.) + 
  labs(title = paste0("Proportion S. Aureus resistant to ", target_drug ," by year")) + 
  theme_linedraw()


atlas_sa_sub[Moxifloxacin == ">2", Moxifloxacin := 2]
atlas_sa_sub[Moxifloxacin == "<=0.03", Moxifloxacin := 0.03]
atlas_sa_sub[Moxifloxacin == ">4", Moxifloxacin := 4]
atlas_sa_sub[Moxifloxacin == "<=0.008", Moxifloxacin := 0.008]
atlas_sa_sub[Moxifloxacin == "", Moxifloxacin := NA]
atlas_sa_sub$Moxifloxacin <- as.numeric(atlas_sa_sub$Moxifloxacin)
atlas_sa_sub <- na.omit(atlas_sa_sub)

test <- dcast(atlas_sa_sub, Gender + Age.Group + ~Moxifloxacin)
test[, total :=  `0.008` +  `0.015` + `0.03` + `0.06` + `0.12` + 
       `0.25` + `0.5` + `1` + `2` + `4`]
test[, "0.008_prop" := (`0.008` )/ total]
test[, "0.015_prop" := (`0.008` + `0.015` )/ total]
test[, "0.03_prop" := (`0.008` + `0.015` + `0.03`)/ total]
test[, "0.06_prop" := ( `0.008` + `0.015` + `0.03`+`0.06` )/ total]
test[, "0.12_prop" := ( `0.008` + `0.015` + `0.03`+`0.06` + `0.12`)/ total]
test[, "0.25_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25`)/ total]
test[, "0.5_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` )/ total]
test[, "1_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` + `1`)/ total]
test[, "2_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` + `1`+`2` )/ total]
test[, "4_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` + `1`+`2` + `4`)/ total]


test2 <- melt.data.table(test, id.vars = c("Gender", "Age.Group"), 
                         measure.vars = c("0.008_prop" ,
                                          "0.015_prop" ,
                                          "0.03_prop" ,
                                            "0.06_prop" ,
                                            "0.12_prop" ,
                                            "0.25_prop" ,
                                            "0.5_prop",
                                            "1_prop" ,
                                              "2_prop" ,
                                                "4_prop"))


test2[variable == "0.008_prop", MIC_max := 0.008]
test2[variable == "0.015_prop", MIC_max := 0.015]
test2[variable == "0.03_prop", MIC_max := 0.03]
test2[variable == "0.06_prop", MIC_max := 0.06]
test2[variable == "0.12_prop", MIC_max := 0.12]
test2[variable == "0.25_prop", MIC_max := 0.25]
test2[variable == "0.5_prop", MIC_max := 0.5]
test2[variable == "1_prop", MIC_max := 1]
test2[variable == "2_prop", MIC_max := 2]
test2[variable == "4_prop", MIC_max := 4]

test2$Age.Group <- factor(test2$Age.Group, 
                                 levels = c(
                                   "0 to 2 Years" ,"3 to 12 Years",
                                   "13 to 18 Years", "19 to 64 Years",
                                   "65 to 84 Years",
                                   "85 and Over",    "Unknown"
                                 )) 


ggplot(test2, aes(x = MIC_max, y = value, colour = Age.Group)) +
  geom_line()+
  labs(title = "MIC by age - Moxifloxacin", x = "MIC value", 
       y = "cumulative proportion of samples by age group", 
       colour = "Age group") + 
  scale_x_log10() + 
  facet_grid(.~Gender) + 
  theme_linedraw()

# need to contrast that with if there are differences by year!



test <- dcast(atlas_sa_sub, Gender + Year  ~ Moxifloxacin)
test[, total :=  `0.008` +  `0.015` + `0.03` + `0.06` + `0.12` + 
       `0.25` + `0.5` + `1` + `2` + `4`]
test[, "0.008_prop" := (`0.008` )/ total]
test[, "0.015_prop" := (`0.008` + `0.015` )/ total]
test[, "0.03_prop" := (`0.008` + `0.015` + `0.03`)/ total]
test[, "0.06_prop" := ( `0.008` + `0.015` + `0.03`+`0.06` )/ total]
test[, "0.12_prop" := ( `0.008` + `0.015` + `0.03`+`0.06` + `0.12`)/ total]
test[, "0.25_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25`)/ total]
test[, "0.5_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` )/ total]
test[, "1_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` + `1`)/ total]
test[, "2_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` + `1`+`2` )/ total]
test[, "4_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` + `1`+`2` + `4`)/ total]


test2 <- melt.data.table(test, id.vars = c("Gender", "Year"), 
                         measure.vars = c("0.008_prop" ,
                                          "0.015_prop" ,
                                          "0.03_prop" ,
                                          "0.06_prop" ,
                                          "0.12_prop" ,
                                          "0.25_prop" ,
                                          "0.5_prop",
                                          "1_prop" ,
                                          "2_prop" ,
                                          "4_prop"))


test2[variable == "0.008_prop", MIC_max := 0.008]
test2[variable == "0.015_prop", MIC_max := 0.015]
test2[variable == "0.03_prop", MIC_max := 0.03]
test2[variable == "0.06_prop", MIC_max := 0.06]
test2[variable == "0.12_prop", MIC_max := 0.12]
test2[variable == "0.25_prop", MIC_max := 0.25]
test2[variable == "0.5_prop", MIC_max := 0.5]
test2[variable == "1_prop", MIC_max := 1]
test2[variable == "2_prop", MIC_max := 2]
test2[variable == "4_prop", MIC_max := 4]

test2$Year <- factor(test2$Year) 


ggplot(test2, aes(x = MIC_max, y = value, colour = Year)) +
  geom_line()+
  labs(title = "MIC by year - Moxifloxacin", x = "MIC value", 
       y = "cumulative proportion of samples by age group", 
       colour = "Age group") + 
  scale_x_log10() + 
  facet_grid(.~Gender) + 
  theme_linedraw()


# Can do by infection type too???



test <- dcast(atlas_sa_sub, Gender + Source  ~ Moxifloxacin)
test[, total :=  `0.008` +  `0.015` + `0.03` + `0.06` + `0.12` + 
       `0.25` + `0.5` + `1` + `2` + `4`]
test[, "0.008_prop" := (`0.008` )/ total]
test[, "0.015_prop" := (`0.008` + `0.015` )/ total]
test[, "0.03_prop" := (`0.008` + `0.015` + `0.03`)/ total]
test[, "0.06_prop" := ( `0.008` + `0.015` + `0.03`+`0.06` )/ total]
test[, "0.12_prop" := ( `0.008` + `0.015` + `0.03`+`0.06` + `0.12`)/ total]
test[, "0.25_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25`)/ total]
test[, "0.5_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` )/ total]
test[, "1_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` + `1`)/ total]
test[, "2_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` + `1`+`2` )/ total]
test[, "4_prop" := (  `0.008` + `0.015` + `0.03`+`0.06` + `0.12`+`0.25` + `0.5` + `1`+`2` + `4`)/ total]

test <- test[total>=1000]


test2 <- melt.data.table(test, id.vars = c("Gender", "Source"), 
                         measure.vars = c("0.008_prop" ,
                                          "0.015_prop" ,
                                          "0.03_prop" ,
                                          "0.06_prop" ,
                                          "0.12_prop" ,
                                          "0.25_prop" ,
                                          "0.5_prop",
                                          "1_prop" ,
                                          "2_prop" ,
                                          "4_prop"))


test2[variable == "0.008_prop", MIC_max := 0.008]
test2[variable == "0.015_prop", MIC_max := 0.015]
test2[variable == "0.03_prop", MIC_max := 0.03]
test2[variable == "0.06_prop", MIC_max := 0.06]
test2[variable == "0.12_prop", MIC_max := 0.12]
test2[variable == "0.25_prop", MIC_max := 0.25]
test2[variable == "0.5_prop", MIC_max := 0.5]
test2[variable == "1_prop", MIC_max := 1]
test2[variable == "2_prop", MIC_max := 2]
test2[variable == "4_prop", MIC_max := 4]


ggplot(test2, aes(x = MIC_max, y = value, colour = Source)) +
  geom_line()+
  labs(title = "MIC by Source - Moxifloxacin (only where n >1000)", x = "MIC value", 
       y = "cumulative proportion of samples by age group", 
       colour = "Age group") + 
  scale_x_log10() + 
  facet_grid(.~Gender) + 
  theme_linedraw()



atlas_sa_sub$Age.Group <- factor(atlas_sa_sub$Age.Group, 
                                 levels = c(
                                   "0 to 2 Years" ,"3 to 12 Years",
                                   "13 to 18 Years", "19 to 64 Years",
                                   "65 to 84 Years",
                                   "85 and Over",    "Unknown"
                                 )) 


ggplot(atlas_sa_sub, aes(x = Moxifloxacin, colour = Age.Group)) + 
  geom_density() + facet_grid(.~Gender) + 
  scale_x_log10() 

SOURCE
COUNTRY
YEAR
