# data cleaning for S/R instead of MIC
#### ONLY USE ATLAS AS OTHERS DONT HAVE

## Libraries
library(tidyverse)
library(readxl)
library(data.table)

## Read in data 
list.files("data") # should be 6 files

#####******************* Look at datasets - decide which can use ******************#################
###### (1) ATLAS
atlas <- read_csv("data/2023_06_15 atlas_antibiotics.csv")

atlas$`Quinupristin dalfopristin` <- as.character(atlas$`Quinupristin dalfopristin`)

atlas_clean <- atlas %>% select(-c("Gatifloxacin", "Gatifloxacin_I","Tetracycline")) %>% 
  pivot_longer(cols = `Amikacin`:`GIM`, values_to = "mic", names_to = "antibiotic") %>% 
  filter(!is.na(mic)) %>% mutate(data = "atls")
# keep only the ones with _I as they are S v R
atlas_clean <- atlas_clean[grepl("_I", atlas_clean$antibiotic),]
# remove the _I
atlas_clean$antibiotic <- gsub("_I", "", atlas_clean$antibiotic)

colnames(atlas_clean) <- tolower(colnames(atlas_clean))
atlas_clean$source <- tolower(atlas_clean$source)
# urine
atlas_clean[str_which(atlas_clean$source, "urine"),"key_source"] <- "urine"
atlas_clean[str_which(atlas_clean$source, "urinary"),"key_source"] <- "urine"
atlas_clean[str_which(atlas_clean$source, "urethra"),"key_source"] <- "urine"
atlas_clean[which(atlas_clean$source == "bladder"), "key_source"] <- "urine"
atlas_clean[which(atlas_clean$source == "ureter"), "key_source"] <- "urine"

# blood
atlas_clean[str_which(atlas_clean$source, "blood"),"key_source"] <- "blood"

# respiratory
atlas_clean[str_which(atlas_clean$source, "respiratory"),"key_source"] <- "respiratory"
atlas_clean[str_which(atlas_clean$source, "lung"),"key_source"] <- "respiratory"
atlas_clean[str_which(atlas_clean$source, "sputum"),"key_source"] <- "respiratory"
atlas_clean[str_which(atlas_clean$source, "aspirate"),"key_source"] <- "respiratory"
atlas_clean[str_which(atlas_clean$source, "sinus"),"key_source"] <- "respiratory"
atlas_clean[str_which(atlas_clean$source, "trache"),"key_source"] <- "respiratory"
atlas_clean[str_which(atlas_clean$source, "lavage"),"key_source"] <- "respiratory"
atlas_clean[which(atlas_clean$source == "bronchus"),"key_source"] <- "respiratory"
atlas_clean[which(atlas_clean$source == "pleural fluid"),"key_source"] <- "respiratory"
atlas_clean[which(atlas_clean$source == "bronchiole"),"key_source"] <- "respiratory"

# wound
atlas_clean[str_which(atlas_clean$source, "wound"),"key_source"] <- "wound"
atlas_clean[str_which(atlas_clean$source, "burn"),"key_source"] <- "wound"
atlas_clean[str_which(atlas_clean$source, "skin"),"key_source"] <- "wound"
atlas_clean[str_which(atlas_clean$source, "pus"),"key_source"] <- "wound"
atlas_clean[str_which(atlas_clean$source, "cellulitis"),"key_source"] <- "wound"
atlas_clean[which(atlas_clean$source == "abscess"),"key_source"] <- "wound"

### Gastrointestinal track 
atlas_clean[str_which(atlas_clean$source, "gi:"),"key_source"] <- "gastro"
atlas_clean[str_which(atlas_clean$source, "bowel"),"key_source"] <- "gastro"
atlas_clean[str_which(atlas_clean$source, "intestinal"),"key_source"] <- "gastro"
atlas_clean[str_which(atlas_clean$source, "gastric abscess"),"key_source"] <- "gastro"
atlas_clean[str_which(atlas_clean$source, "colon"),"key_source"] <- "gastro"


atlas_clean$age <- factor(atlas_clean$age, 
                              levels = c("0 to 2 Years","3 to 12 Years", "13 to 18 Years",
                                         "19 to 64 Years", "65 to 84 Years", "85 and Over"))


atlas_clean <- data.table(atlas_clean)
# treat Intermediate as resitant
atlas_clean[mic == "Intermediate", mic := "Resistant"]


# check by what?
characteristic <- "age" # age gender or key_source
atlas_clean <- atlas_clean[!is.na(get(characteristic))]



for(j in c("Staphylococcus aureus", "Escherichia coli", "Klebsiella pneumoniae")){
  data_sub <- atlas_clean[organism == j]
  
  # vector for storing relevant drugs and plots
  drugs <- unique(data_sub$antibiotic)
  plot_store <- list()
  
  #for each of the relevant drugs
  for(i in drugs){
    data_sub_drug <- data_sub[antibiotic == i]
    #count observations by subset
    test <- data_sub_drug[, .N, by = .(mic, get(characteristic) )]
    colnames(test) <- c("result", characteristic, "N")
    # also need out of total observations for the age_group/gender
    test2 <- data_sub_drug[, .N, by = .(get(characteristic))]
    colnames(test2) <- c(characteristic, "N")
    # note total number of MIC samples
    tot_samps <- sum(test2$N)
    # combine the two together so can work out proportion
    test[test2, on = c(characteristic), Total := i.N]
    #work out proportion
    test[, prop := N/Total]
    
    # store plot
    test <- test[N>100]
    if(nrow(test)>0){
      temp<- ggplot(test, aes(x= !!sym(characteristic), y =prop, colour = result, group = c(result))) + 
        geom_point()+ geom_line() +
        labs(title = paste0("S v (R+I) ", i, paste0(". Tot samples = ", tot_samps)), x = characteristic, 
             y = "Proportion", 
             colour = "Result") + 
        theme_linedraw()  + 
        lims(y = c(0,1))
    }
    
    plot_store[[i]] <- temp
  }
  
  tiff(paste0("plots/",j , "_", characteristic, "_cat.tiff"), width = 2500, height = 1500)
  print(cowplot::plot_grid(plotlist =  plot_store) )
  dev.off()  
}




