# Vivli initial cleaning and screening for interesting bugs
library(tidyverse);library(ggplot2);library(cowplot); library(patchwork)
theme_set(theme_bw())

characteristic <- "key_source" # "age_group"

# read in the data
index_comparison_gender <-read_csv(paste0("plots/gender_",characteristic,"index_store.csv"))
                          
index_comparison <- read_csv(paste0("plots/",characteristic,"index_store.csv"))

sum_index_gender <- index_comparison_gender %>% 
  group_by(antibiotic, organism, gender) %>% 
  summarise(mx = max(abs(dff)), n_big = sum(dff > 0.1)) 
sum_index <- index_comparison %>% group_by(antibiotic, organism) %>% summarise(mx = max(dff), n_big = sum(dff > 0.2)) 

g1 <- ggplot(sum_index_gender %>% filter(n_big > 3), aes(x=antibiotic, y = mx, group = interaction(organism, gender))) + geom_point(aes(col = organism, pch = gender)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_hline(yintercept = mean(sum_index$mx)) + 
  geom_hline(yintercept = quantile(sum_index$mx)[2], lty = "dashed") + geom_hline(yintercept = quantile(sum_index$mx)[4], lty = "dashed")+ 
  ggtitle(characteristic) + 
  scale_y_continuous("Maximum difference in MIC\nacross groupings")

g2 <- ggplot(sum_index %>% filter(n_big > 3), aes(x=antibiotic, y = mx, group = organism)) + geom_point(aes(col = organism)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_hline(yintercept = mean(sum_index$mx)) + 
  geom_hline(yintercept = quantile(sum_index$mx)[2], lty = "dashed") + geom_hline(yintercept = quantile(sum_index$mx)[4], lty = "dashed") + 
  scale_y_continuous("Maximum difference in MIC\nacross groupings")

g1 / g2 + plot_layout(guides = "collect")
ggsave(paste0("plots/", characteristic, "index.pdf"), height = 10, width = 7)

### Stats for report 
high_all <- sum_index %>% filter(mx > quantile(sum_index$mx)[4])
high_gender <- sum_index_gender %>% filter(mx > quantile(sum_index$mx)[4])

100*table(high_all$organism) / dim(high_all)[1] 
# In men
100*table(high_gender %>% ungroup() %>% filter(gender == "m") %>% select(organism)) / dim(high_gender %>% filter(gender=="m"))[1]
# In women
100*table(high_gender %>% ungroup() %>% filter(gender == "f") %>% select(organism)) / dim(high_gender %>% filter(gender=="f"))[1]
