## Explore source 

library(tidyverse)
theme_set(theme_bw())
### data
data <- read_csv("data/full_data.csv")

#find all the drug names
bacteria <- unique(data$organism)

# Group over everything
data_just_source <- data %>% filter(!is.na(organism_clean), !is.na(key_source)) %>% 
  group_by(organism_clean, antibiotic, key_source, mic) %>% 
  summarise(n = n()) %>% 
  group_by(organism_clean, antibiotic, key_source) %>% 
  mutate(cum_n = cumsum(n))

ggplot(data_just_source, aes(x=mic, y= cum_n, group = key_source)) + 
  geom_line(aes(col = key_source)) + 
  facet_wrap(organism_clean ~ antibiotic, scales = "free")
ggsave("plots/source_all_group.pdf")

ggplot(data_just_source, aes(x=mic, y= cum_n, group = key_source)) + 
  geom_line(aes(col = key_source)) + 
  facet_grid(organism_clean ~ antibiotic, scales = "free")
ggsave("plots/source_all_group_grid.pdf", height = 10, width = 20)

#### For each bug over source age gender
bugs <- unique(data%>% filter(!organism_clean == "") %>% select(organism_clean))
for(i in bugs$organism_clean){
  print(i)
  data_sag <- data %>% filter(!is.na(organism_clean), !is.na(key_source)) %>% 
    filter(organism_clean == i) %>%
    group_by(organism_clean, antibiotic, key_source, mic, age_group, gender) %>% 
    summarise(n = n()) %>% 
    group_by(organism_clean, antibiotic, key_source, age_group, gender) %>% 
    mutate(cum_n = cumsum(n))
  
  ggplot(data_sag, aes(x=mic, y= cum_n, group = interaction(age_group, gender, key_source))) + 
    geom_line(aes(col = age_group, lty = key_source)) + 
    facet_wrap(gender ~ antibiotic, scales = "free") + 
    ggtitle(i)
  ggsave(paste0("plots/source_",substr(i,1,4),"_ag_age.pdf"))
  
  ggplot(data_sag, aes(x=mic, y= cum_n, group = interaction(age_group, gender, key_source))) + 
    geom_line(aes(col = key_source, lty = gender)) + 
    facet_grid(age_group ~ antibiotic, scales = "free") + 
    ggtitle(i)
  ggsave(paste0("plots/source_",substr(i,1,4),"_ag_age_gender.pdf"))
  
 
}

### Just some antibiotics: start bit random 
key_antibiotics <- c("amikacin","ampicillin","ciprofloxacin",
                     "oxacillin","clindamycin","erythromycin")

#### For each bug over source age gender
bugs <- unique(data %>% filter(!organism_clean == "") %>% select(organism_clean))
drugs <- key_antibiotics 
for(i in bugs$organism_clean){
  print(i)
  data_sag <- data %>% filter(!organism_clean == "", !key_source == "") %>% 
    filter(organism_clean == i, antibiotic %in% drugs) %>%
    group_by(organism_clean, antibiotic, key_source, mic, age_group, gender) %>% 
    summarise(n = n()) %>% 
    group_by(organism_clean, antibiotic, key_source, age_group, gender) %>% 
    mutate(cum_n = cumsum(n))
  
  ggplot(data_sag, aes(x=mic, y= cum_n, group = interaction(age_group, gender, key_source))) + 
    geom_line(aes(col = age_group, lty = key_source)) + 
    facet_wrap(gender ~ antibiotic, scales = "free") + 
    ggtitle(i)
  ggsave(paste0("plots/source_",substr(i,1,4),"_ag_age_focusabx.pdf"))
  
  ggplot(data_sag, aes(x=mic, y= cum_n, group = interaction(age_group, gender, key_source))) + 
    geom_line(aes(col = key_source, lty = gender)) + 
    facet_grid(age_group ~ antibiotic, scales = "free") + 
    ggtitle(i)
  ggsave(paste0("plots/source_",substr(i,1,4),"_ag_age_gender_focusabx.pdf"))
  
  
}



  # 
  # # look at just one bug for now (might loop this later!)
  # for(j in bacteria){
  #   data_sub <- data %>% filter(organism_clean == j)
  #   drugs <- unique(data_sub$antibiotic)
  #   
  #   # vector for storing relevant drugs and plots
  #   drug_holder <- c()
  #   plot_store <- list()
  #   
  #   #for each of the relevant drugs
  #   
  #     
  #     
  #     
  #     
  #     
  #     # subset cols of interest
  #     interested_cols <- c("gender", "source", "year", "In...Out.Patient", i)
  #     atlas_sub_drug <- atlas_sub[,..interested_cols]
  #     # remove NA values
  #     atlas_sub_drug <- atlas_sub_drug[!is.na(Age.Group) & !is.na(Gender) &
  #                                        !is.na(get(i))]
  #     #count observations by subset
  #     test <- atlas_sub_drug[, .N, by = .(Gender, get(i),get(characteristic) )]
  #     colnames(test) <- c("Gender", "MIC", characteristic, "N")
  #     test$MIC <- as.numeric(test$MIC)
  #     # also need out of total observations for the age_group/gender
  #     test2 <- atlas_sub_drug[, .N, by = .(Gender, get(characteristic))]
  #     colnames(test2) <- c("Gender", characteristic, "N")
  #     # note total number of MIC samples
  #     tot_samps <- sum(test2$N)
  #     # combine the two together so can work out proportion
  #     test[test2, on = c("Gender", characteristic), Total := i.N]
  #     #work out proportion
  #     test[, prop := N/Total]
  #     # cumulative sum of proportion (first order)
  #     test <- test[order(MIC, Gender, get(characteristic))]
  #     for_plot <-test[, cumulative_sum := cumsum(prop), by = c("Gender", characteristic)]
  #     # if it's the first one make and take legend first
  #     if(i == drug_holder[1]){
  #       temp <- ggplot(for_plot, aes(x= MIC, y =cumulative_sum, colour = !!sym(characteristic))) + 
  #         labs(colour = characteristic) + 
  #         facet_grid(.~Gender) 
  #       # extract legend
  #       shared_legend <- get_legend(temp)
  #     }
  #     # store plot
  #     for_plot <- for_plot[N>100]
  #     if(nrow(for_plot)>0){
  #       temp<- ggplot(for_plot[N>=100], aes(x= MIC, y =cumulative_sum, colour = !!sym(characteristic))) + 
  #         geom_line()+
  #         labs(title = paste0("MIC by age group - ", i, paste0(". Tot samples = ", tot_samps)), x = "MIC value", 
  #              y = "cumulative proportion of samples by age group and gender", 
  #              colour = characteristic) + 
  #         scale_x_log10() + 
  #         facet_grid(.~Gender) + 
  #         theme_linedraw() + 
  #         theme(legend.position = "NONE")
  #     }
  #     
  #     plot_store[[i]] <- temp
  #   }
  #   
  #   tiff(paste0(j , "_", characteristic, "_MICs.tiff"), width = 2500, height = 1500)
  #   print(cowplot::plot_grid(plotlist =  plot_store) )
  #   dev.off()  
  # }
  # 
  