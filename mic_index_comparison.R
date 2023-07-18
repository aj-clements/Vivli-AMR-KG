# Vivli initial cleaning and screening for interesting bugs
library(tidyverse);library(ggplot2);library(cowplot)
theme_set(theme_bw())
# read in the data
index_comparison <- rbind(read_csv("plots/gender_Escherichia coli_key_sourceindex_store.csv"),
                          read_csv("plots/gender_Klebsiella pneumoniae_key_sourceindex_store.csv"),
                          read_csv("plots/gender_Staphylococcus aureus_key_sourceindex_store.csv"))

sum_index <- index_comparison %>% group_by(antibiotic, organism, gender) %>% summarise(mx = max(dff)) 

ggplot(sum_index, aes(x=antibiotic, y = mx, group = interaction(organism, gender))) + geom_point(aes(col = organism, pch = gender)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_hline(yintercept = mean(sum_index$mx)) + 
  geom_hline(yintercept = quantile(sum_index$mx)[2], lty = "dashed") + geom_hline(yintercept = quantile(sum_index$mx)[4], lty = "dashed")

sum_index %>% filter(mx > quantile(sum_index$mx)[4])
