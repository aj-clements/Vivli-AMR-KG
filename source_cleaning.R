#### Source cleaning 

library(tidyverse)
full_data <- read.csv("data/full_data.csv")
full_data <- full_data %>% mutate(key_source = "")

### What is in there?  
u <- unique(full_data$source)
tt <- table(full_data$source) %>% as.data.frame() %>% 
  arrange(desc(Freq))  # easier to manipulate shorter dataframe for exploration of terms
colnames(tt) <- c("source","freq")


### Key sources: 
## Urine / blood / respiratory / wound / gastro
# Could add reproduction / head (ear / eys) / heart 
tt <- tt %>% mutate("key_source" = "") # add a column that is for the key classification of sources
tt$source <- tolower(tt$source)
full_data$source <- tolower(full_data$source)

# urine
tt$source[str_which(tt$source, "urine")]
tt$source[str_which(tt$source, "bladder")] # not gall bladder
tt$source[str_which(tt$source, "urinary")] # 
tt$source[str_which(tt$source, "urethra")] # 
tt$source[str_which(tt$source, "ureter")] # 


full_data[str_which(full_data$source, "urine"),"key_source"] <- "urine"
full_data[str_which(full_data$source, "urinary"),"key_source"] <- "urine"
full_data[str_which(full_data$source, "urethra"),"key_source"] <- "urine"
full_data[which(full_data$source == "bladder"), "key_source"] <- "urine"
full_data[which(full_data$source == "ureter"), "key_source"] <- "urine"

# blood
tt$source[str_which(tt$source, "blood")]
tt$source[str_which(tt$source, "bld")]

full_data[str_which(full_data$source, "blood"),"key_source"] <- "blood"

# respiratory
tt$source[str_which(tt$source, "respiratory")]
tt$source[str_which(tt$source, "lung")]
tt$source[str_which(tt$source, "sputum")]
tt$source[str_which(tt$source, "aspirate")]
tt$source[str_which(tt$source, "sinus")]
tt$source[str_which(tt$source, "trache")]
tt$source[str_which(tt$source, "phagus")]
tt$source[str_which(tt$source, "lavage")]
tt$source[str_which(tt$source, "bronchiole")]


full_data[str_which(full_data$source, "respiratory"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "lung"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "sputum"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "aspirate"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "sinus"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "trache"),"key_source"] <- "respiratory"
full_data[str_which(full_data$source, "lavage"),"key_source"] <- "respiratory"
full_data[which(full_data$source == "bronchus"),"key_source"] <- "respiratory"
full_data[which(full_data$source == "pleural fluid"),"key_source"] <- "respiratory"
full_data[which(full_data$source == "bronchiole"),"key_source"] <- "respiratory"
# wound
tt$source[str_which(tt$source, "wound")]
tt$source[str_which(tt$source, "burn")]
tt$source[str_which(tt$source, "skin")]
tt$source[str_which(tt$source, "ulcer")]
tt$source[str_which(tt$source, "pus")]
tt$source[str_which(tt$source, "cellulitis")]


full_data[str_which(full_data$source, "wound"),"key_source"] <- "wound"
full_data[str_which(full_data$source, "burn"),"key_source"] <- "wound"
full_data[str_which(full_data$source, "skin"),"key_source"] <- "wound"
full_data[str_which(full_data$source, "pus"),"key_source"] <- "wound"
full_data[str_which(full_data$source, "cellulitis"),"key_source"] <- "wound"
full_data[which(full_data$source == "abscess"),"key_source"] <- "wound"

### Gastrointestinal track 
tt$source[str_which(tt$source, "gi:")]
tt$source[str_which(tt$source, "bowel")]
tt$source[str_which(tt$source, "intestinal")]
tt$source[str_which(tt$source, "ulcer")]
tt$source[str_which(tt$source, "pus")]
tt$source[str_which(tt$source, "cellulitis")]
tt$source[str_which(tt$source, "gastric abscess")]
tt$source[str_which(tt$source, "colon")]


full_data[str_which(full_data$source, "gi:"),"key_source"] <- "gastro"
full_data[str_which(full_data$source, "bowel"),"key_source"] <- "gastro"
full_data[str_which(full_data$source, "intestinal"),"key_source"] <- "gastro"
full_data[str_which(full_data$source, "gastric abscess"),"key_source"] <- "gastro"
full_data[str_which(full_data$source, "colon"),"key_source"] <- "gastro"

# what's left?
tt_not <- full_data %>% filter(key_source == "")
tt_not <- table(tt_not$source) %>% as.data.frame() %>% 
  arrange(desc(Freq)) 
tt_not
100 * sum(tt_not$Freq) / sum(tt$freq) # < 15%. Stop here

## Save
write.csv(full_data, "data/full_data.csv")
