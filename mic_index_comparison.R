# Vivli initial cleaning and screening for interesting bugs
library(tidyverse);library(ggplot2);library(cowplot); library(patchwork)
theme_set(theme_bw())

characteristic <- "age_group"

# read in the data
index_comparison_gender <- rbind(read_csv(paste0("plots/gender_Escherichia coli_",characteristic,"index_store.csv")),
                          read_csv(paste0("plots/gender_Klebsiella pneumoniae_",characteristic,"index_store.csv")),
                          read_csv(paste0("plots/gender_Staphylococcus aureus_",characteristic,"index_store.csv")))

index_comparison <- rbind(read_csv(paste0("plots/Escherichia coli_",characteristic,"index_store.csv")),
                                 read_csv(paste0("plots/Klebsiella pneumoniae_",characteristic,"index_store.csv")),
                                 read_csv(paste0("plots/Staphylococcus aureus_",characteristic,"index_store.csv")))

sum_index_gender <- index_comparison_gender %>% group_by(antibiotic, organism, gender) %>% summarise(mx = max(dff)) 
sum_index <- index_comparison %>% group_by(antibiotic, organism) %>% summarise(mx = max(dff)) 

g1 <- ggplot(sum_index_gender, aes(x=antibiotic, y = mx, group = interaction(organism, gender))) + geom_point(aes(col = organism, pch = gender)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_hline(yintercept = mean(sum_index$mx)) + 
  geom_hline(yintercept = quantile(sum_index$mx)[2], lty = "dashed") + geom_hline(yintercept = quantile(sum_index$mx)[4], lty = "dashed")+ 
  ggtitle(characteristic) + 
  scale_y_continuous("Maximum difference in MIC\nacross groupings")

g2 <- ggplot(sum_index, aes(x=antibiotic, y = mx, group = organism)) + geom_point(aes(col = organism)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_hline(yintercept = mean(sum_index$mx)) + 
  geom_hline(yintercept = quantile(sum_index$mx)[2], lty = "dashed") + geom_hline(yintercept = quantile(sum_index$mx)[4], lty = "dashed") + 
  scale_y_continuous("Maximum difference in MIC\nacross groupings")

g1 / g2 + plot_layout(guides = "collect")
ggsave(paste0("plots/", characteristic, "index.pdf"), height = 10, width = 7)


high_all <- sum_index %>% filter(mx > quantile(sum_index$mx)[4])
high_gender <- sum_index_gender %>% filter(mx > quantile(sum_index$mx)[4])

100*table(high_all$organism) / dim(high_all)[1] 
# In men
100*table(high_gender %>% ungroup() %>% filter(gender == "m") %>% select(organism)) / dim(high_gender %>% filter(gender=="m"))[1]
# In women
100*table(high_gender %>% ungroup() %>% filter(gender == "f") %>% select(organism)) / dim(high_gender %>% filter(gender=="f"))[1]
