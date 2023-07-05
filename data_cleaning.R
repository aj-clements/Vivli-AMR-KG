##### DATA explore and clean 

## To do
## - screen which datasets have MIC
## - develop method to extract all MIC values uniformly 
## - clean data to only those with sufficient MIC values 

## Libraries
library(tidyverse)
library(readxl)

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
atlas_clean <- atlas %>% select(-c("Gatifloxacin", "Gatifloxacin_I","Tetracycline")) %>% pivot_longer(cols = `Amikacin`:`GIM`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic))

unique(atlas_clean$mic)


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
  filter(!is.na(mic))

unique(gsk_clean$mic)

###### Omadacycline
oma <- readxl::read_excel("data/Omadacycline_2014_to_2022_Surveillance_data.xlsx")
colnames(oma) # Age and Gender in there, alongside 
head(oma)
dim(oma) # big: 83209
table(oma$Gender)
table(oma$Age) # good range
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
# Some are all NA: remove
unique(oma$Gatifloxacin_I)
# Some are doubles: make characters for now
unique(oma$`Quinupristin dalfopristin`)
oma$`Quinupristin dalfopristin` <- as.character(oma$`Quinupristin dalfopristin`)

# Pivot longer to explore ranges in MIC
oma_clean <- oma %>% select(-c("Oxacillin","Ceftaroline","Ceftriaxone","Amoxicillin-\r\nclavulanic acid","Erythromycin","Clindamycin","Linezolid","Daptomycin",
                               "Vancomycin","Teicoplanin","Ampicillin","Azithromycin","Aztreonam","Ceftazidime","Colistin","Moxifloxacin","Penicillin")) %>% pivot_longer(cols = `Omadacycline`:`Trimethoprim-sulfamethoxazole`, values_to = "mic", names_to = "antibiotic")

unique(oma_clean$mic)

###### SIDERO
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
sidero_clean <- sidero %>% pivot_longer(cols = `Cefiderocol`:`Imipenem/ Relebactam`, values_to = "mic", names_to = "antibiotic")

unique(sidero_clean$mic)

###### Venatorx
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
vena_clean <- vena %>% pivot_longer(cols = `CAZ_MIC`:`TZP_MIC`, values_to = "mic", names_to = "antibiotic")

unique(vena_clean$mic)


