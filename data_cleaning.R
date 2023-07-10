##### DATA explore and clean 

## To do
## - screen which datasets have MIC
## - develop method to extract all MIC values uniformly 
## - clean data to only those with sufficient MIC values 

## Libraries
library(tidyverse)
library(readxl)
library(data.table)

## Read in data 
list.files("data") # should be 6 files

#####******************* Look at datasets - decide which can use ******************#################
###### (1) ATLAS
atlas <- read_csv("data/2023_06_15 atlas_antibiotics.csv")
colnames(atlas) # metadata and antibiotic MIC eg age gender, source, country, in/out patient
unique(atlas$Year) # latest data from 2021! 

table(atlas$Speciality)
table(atlas$Study)

### Explore antibiotic data 
# NRW: did you do this for all and just keep some? Or why these ones?
unique(atlas$Amikacin)
# Some data are logicals: no MIC, remove
unique(atlas$Gatifloxacin)
unique(atlas$Tetracycline)
# Some are all NA: remove
unique(atlas$Gatifloxacin_I)
# Some are doubles: make characters for now
unique(atlas$`Quinupristin dalfopristin`)
atlas$`Quinupristin dalfopristin` <- as.character(atlas$`Quinupristin dalfopristin`)

# Pivot longer to explore ranges in MIC
# Ignores the specified drugs, melts the rest, removes NAs and adds a colum for data source
# NOTE! This melts in both the mic value, but also the categorisation (S/I/R etc). So many records in twice. Drops out later. 
atlas_clean <- atlas %>% select(-c("Gatifloxacin", "Gatifloxacin_I","Tetracycline")) %>% 
  pivot_longer(cols = `Amikacin`:`GIM`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic)) %>% mutate(data = "atls")

unique(atlas_clean$mic)
colnames(atlas_clean) <- tolower(colnames(atlas_clean))
atlas_clean <- rename(atlas_clean, "organism" = "species")
atlas_clean <- rename(atlas_clean, "age" = "age group")

###### (2) DREAM: Mtb 
dream <- readxl::read_excel("data/BEDAQUILINE DREAM DATASET FOR VIVLI - 06-06-2022.xlsx")
colnames(dream) # has MIC data and country, specimen metadata but not age / gender
dim(dream) # 5928
table(dream$Specimen) # vast majority (89%) sputum so would be hard to do sub analysis 
table(dream$SubType) # Resistance classification
## Due to lack of sub groupings EXCLUDE

##### (3) GSK
gsk <- read_csv("data/gsk_201818_published.csv")
colnames(gsk) # has age and gender, body location 
dim(gsk) # small: only 2413? 
table(gsk$COUNTRY) # Eastern Europe focus? 
table(gsk$ORGANISMNAME) # only Haemophilus influenzae and Strep pneumo but does have MIC 
table(gsk$BODYLOCATION) # lots sputum 
table(gsk$AGE)
table(gsk$GENDER)

# Pivot longer to explore ranges in MIC
gsk_clean <- gsk %>% #select(-c("Gatifloxacin", "Gatifloxacin_I","Tetracycline")) %>% 
  pivot_longer(cols = `AMOXICILLIN`:`TRIMETHOPRIM_SULFA`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic)) %>% mutate(data = "gsk8")

unique(gsk_clean$mic)
colnames(gsk_clean) <- tolower(colnames(gsk_clean))
gsk_clean <- rename(gsk_clean, "source" = "bodylocation")
gsk_clean <- rename(gsk_clean, "year" = "yearcollected")
gsk_clean <- rename(gsk_clean, "organism" = "organismname")


###### (4) Omadacycline
oma <- readxl::read_excel("data/Omadacycline_2014_to_2022_Surveillance_data.xlsx")
colnames(oma) # Age and Gender in there, alongside 
head(oma)
dim(oma) # big: 83209
table(oma$Gender)
table(oma$Age) # good range! ### THHERE ARE AGES ABOVE 248!!! 
table(oma$`CF Patient`) # info on ~4000: 3738 CF patients
table(oma$Country) # Global
table(oma$Organism) # lots

### Explore antibiotic data 
unique(oma$Amikacin)
# Some data are logicals: no MIC, remove: 17! 
unique(oma$Oxacillin)
unique(oma$Ceftaroline)
unique(oma$Ceftriaxone)
unique(oma[,12])
unique(oma$Ampicillin)
unique(oma$Penicillin)

# Pivot longer to explore ranges in MIC
oma_clean <- oma %>% select(-c("Oxacillin","Ceftaroline","Ceftriaxone","Amoxicillin-\r\nclavulanic acid","Erythromycin","Clindamycin","Linezolid","Daptomycin",
                               "Vancomycin","Teicoplanin","Ampicillin","Azithromycin","Aztreonam","Ceftazidime","Colistin","Moxifloxacin","Penicillin")) %>% pivot_longer(cols = `Omadacycline`:`Trimethoprim-sulfamethoxazole`, values_to = "mic", names_to = "antibiotic") %>%
  filter(!is.na(mic)) %>% mutate(data = "omad") %>%
  filter(Age < 120) # some odd year entries = birth date? 

unique(oma_clean$mic)
colnames(oma_clean) <- tolower(colnames(oma_clean))
oma_clean <- rename(oma_clean, "source" = "infection source")
oma_clean <- rename(oma_clean, "year" = "study year")



###### (5) SIDERO
sidero <- readxl::read_excel("data/Updated_Shionogi Five year SIDERO-WT Surveillance data(without strain number)_Vivli_220409.xlsx")
colnames(sidero) # No age and gender. Country, body location. 
head(sidero)
dim(sidero) # big: 47615 
table(sidero$Country) # Global
table(sidero$`Organism Name`) # lots

### Explore antibiotic data 
unique(sidero$Cefiderocol)
sidero$Cefiderocol <- as.character(sidero$Cefiderocol) # make characters to harmonise for now
sidero$Meropenem <- as.character(sidero$Meropenem) # make characters to harmonise for now
sidero$Ciprofloxacin <- as.character(sidero$Ciprofloxacin) # make characters to harmonise for now
sidero$Colistin <- as.character(sidero$Colistin) # make characters to harmonise for now
sidero$`Ceftazidime/ Avibactam` <- as.character(sidero$`Ceftazidime/ Avibactam`) # make characters to harmonise for now
sidero$`Ceftolozane/ Tazobactam`<- as.character(sidero$`Ceftolozane/ Tazobactam`) # make characters to harmonise for now
sidero$Cefepime<- as.character(sidero$Cefepime) # make characters to harmonise for now

# Pivot longer to explore ranges in MIC
sidero_clean <- sidero %>% pivot_longer(cols = `Cefiderocol`:`Imipenem/ Relebactam`, values_to = "mic", names_to = "antibiotic") %>%
  filter(!is.na(mic), !mic == "NULL") %>% mutate(data = "sdro")

unique(sidero_clean$mic)
colnames(sidero_clean) <- tolower(colnames(sidero_clean))
sidero_clean <- rename(sidero_clean, "source" = "body location")
sidero_clean <- rename(sidero_clean, "year" = "year collected")
sidero_clean <- rename(sidero_clean, "organism" = "organism name")

###### (6) Venatorx
vena <- readxl::read_excel("data/Venatorx surveillance data for Vivli 27Feb2023.xlsx")
colnames(vena) # Age and gender. Country, bodysite, facility
head(vena)
table(vena$BodySite)
table(vena$Facility)
dim(vena) # big: 24782
table(vena$Country) # Global
table(vena$Organism) # lots

### Explore antibiotic data 
unique(vena$CAZ_MIC)
unique(vena$CIP_MIC)
vena$CAZ_MIC <- as.character(vena$CAZ_MIC) # make characters to harmonise for now
vena$FEP_MIC <- as.character(vena$FEP_MIC) # make characters to harmonise for now
vena$GM_MIC <- as.character(vena$GM_MIC) # make characters to harmonise for now
vena$MEM_MIC <- as.character(vena$MEM_MIC) # make characters to harmonise for now
vena$TZP_MIC <- as.character(vena$TZP_MIC) # make characters to harmonise for now

# Pivot longer to explore ranges in MIC
vena_clean <- vena %>% pivot_longer(cols = `CAZ_MIC`:`TZP_MIC`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic), !mic == "-") %>% mutate(data = "vena")

unique(vena_clean$mic)
colnames(vena_clean) <- tolower(colnames(vena_clean))
vena_clean <- rename(vena_clean, "source" = "bodysite")


#### Combine data: only explore age / gender / country / body location 
col_use <- c("age","gender","source","year", "organism","antibiotic","mic","data")

# combine the datasets
full_data <- rbind(atlas_clean[,col_use],gsk_clean[,col_use], 
      vena_clean[,col_use],oma_clean[,col_use]) %>% 
  filter(!is.na(mic), !is.na(age), !is.na(gender), !gender == "N") %>% 
  mutate(organism_clean = "")

dim(full_data)
unique(full_data$age)
unique(full_data$gender)
full_data$gender <- tolower(full_data$gender)
full_data$gender <- substr(full_data$gender, 1, 1)     
unique(full_data$gender)
unique(full_data$year)
unique(full_data$mic)
full_data$mic <- gsub('<', '', full_data$mic)
full_data$mic <- gsub('>', '', full_data$mic)
full_data$mic <- gsub('=', '', full_data$mic)
full_data$mic <- gsub('≤', '', full_data$mic)
unique(full_data$mic)
full_data$mic <- as.numeric(full_data$mic)

#### Clean organism for 3 top bugs for now
u <- unique(full_data$organism)
# S. aureus
u[str_which(u, "aureus")] # yes
u[str_which(u, "Staph")] # too many: think above captures it 
full_data[which(full_data$organism == u[str_which(u, "aureus")]),"organism_clean"] <- "Staphylococcus aureus"

# E coli 
u[str_which(u, "coli")] # no too many others 
u[str_which(u, "E coli")] # none
u[str_which(u, "E. coli")] # none
u[str_which(u, "Escherichia")] # too many
full_data[which(full_data$organism == u[str_which(u, "Escherichia coli")]),"organism_clean"] <- "Escherichia coli"

# Klebsiella 
u[str_which(u, "Kleb")] # no too many others 
u[str_which(u, "kleb")] # none
u[str_which(u, "Klebsiella")] # lots
u[str_which(u, "pneumoniae")] # lots
full_data[which(full_data$organism == u[str_which(u,  "Klebsiella pneumoniae")]),"organism_clean"] <- "Klebsiella pneumoniae"

dim(full_data)
head(full_data)

# remove those with NA for MIC value
full_data <- full_data %>% filter(!is.na(mic))


### Clean antibiotics... 
full_data$antibiotic <- tolower(full_data$antibiotic)
abx <- unique(full_data$antibiotic) # remove those wtih "_mic"? which database do they come from? 

### compare across datasets
full_data <- data.table(full_data)

full_data[, age_group := age]
full_data[, age := as.numeric(age)]
full_data[ !is.na(age), age_group := "0 to 2 Years"]
full_data[ age > 2, age_group := "3 to 12 Years"]
full_data[ age > 12, age_group := "13 to 18 Years"]
full_data[ age > 18, age_group := "19 to 64 Years"]
full_data[ age > 64, age_group := "65 to 84 Years"]
full_data[ age > 84, age_group := "85 and Over"]
full_data[age_group == "Unknown", age_group := NA]
full_data <- full_data[!is.na(age_group)]


#### output
write.csv(full_data, "data/full_data.csv")


########## NAOMI WORKING - start ##########


levo_staph <-  full_data[organism_clean %in% c("Staphylococcus aureus") & antibiotic %in% c("levofloxacin")]

test <- levo_staph[, .N, by = .(age_group, mic,data )]
test2 <- levo_staph[, .N, by = .(age_group, data)]
test[test2, on = c("age_group", "data"), total := i.N]
test[,prop := N/total]
test <- test[order(mic, data, age_group)]
for_plot <-test[, cumulative_sum := cumsum(prop), by = c("data", "age_group")]
# does MIC vary across data sets?
ggplot(for_plot, aes(x= mic, y =cumulative_sum, colour = data)) + 
  geom_line()+
  scale_x_log10() + 
  facet_grid(.~age_group) + 
  theme_linedraw() 


########## NAOMI WORKING - end ##########

