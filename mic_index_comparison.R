# Vivli initial cleaning and screening for interesting bugs
library(tidyverse);library(ggplot2);library(cowplot); library(patchwork)
theme_set(theme_bw())

characteristic <- "key_source" #"age_group" # "key_source"

# read in the data
index_comparison_gender <-read_csv(paste0("plots/gender_",characteristic,"index_store.csv"))

index_comparison <- read_csv(paste0("plots/",characteristic,"index_store.csv"))

sum_index_gender <- index_comparison_gender %>% 
  group_by(antibiotic, organism, gender, MIC) %>%
  summarise(df_mic = max(dff)) %>% # Get one value per MIC 
  group_by(antibiotic, organism, gender) %>%
  summarise(mx = max(df_mic), # Max diff for this bug_drug 
            n_big = sum(unique(df_mic) > 0.1)) # Count how many MIC have > 10% diffs
sum_index <- index_comparison %>%
  group_by(antibiotic, organism, MIC) %>%
  summarise(df_mic = max(dff)) %>% # Get one value per MIC 
  group_by(antibiotic, organism) %>%
  summarise(mx = max(df_mic), # Max diff for this bug_drug 
            n_big = sum(unique(df_mic) > 0.1)) # Count how many MIC have > 10% diffs



g1 <- ggplot(sum_index_gender %>% filter(n_big > 3), aes(x=antibiotic, y = mx, group = interaction(organism, gender))) + geom_point(aes(col = organism, pch = gender)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
 # geom_hline(yintercept = mean(sum_index$mx)) + 
#  geom_hline(yintercept = quantile(sum_index$mx)[2], lty = "dashed") + geom_hline(yintercept = quantile(sum_index$mx)[4], lty = "dashed")+ 
  ggtitle(characteristic) + 
  scale_y_continuous("Maximum difference in MIC\nacross groupings")
ggsave(paste0("plots/", characteristic, "index_all.pdf"), height = 10, width = 7)

g2 <- ggplot(sum_index %>% filter(n_big > 3), aes(y=antibiotic, x = mx, group = organism)) + geom_point(aes(col = organism)) + 
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_hline(yintercept = 0.1) + 
 # geom_hline(yintercept = mean(sum_index$mx)) + 
#  geom_hline(yintercept = quantile(sum_index$mx)[2], lty = "dashed") + geom_hline(yintercept = quantile(sum_index$mx)[4], lty = "dashed") + 
  scale_x_continuous("Maximum difference in MIC\nacross groupings")

g1 / g2 + plot_layout(guides = "collect")& theme(legend.position = 'bottom')
ggsave(paste0("plots/", characteristic, "index.pdf"), height = 10, width = 7)

plot_index <- rbind(sum_index_gender, sum_index %>% mutate(gender = "N"))

ggplot(plot_index %>% filter(n_big > 3), aes(x=antibiotic, y = mx, group = interaction(organism, gender))) + 
  geom_point(aes(col = organism, pch = gender), size = 3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle(characteristic) + 
  scale_y_continuous(limits = c(0,0.4), "Maximum difference in MIC\nacross groupings") + 
  scale_shape_discrete("sex",breaks = c("f","m","N"), labels = c("Female","Male","Both")) + 
  geom_hline(yintercept = c(0.1,0.2,0.3), lty = "dashed") + 
  theme(legend.text = element_text(face = "italic"))
ggsave(paste0("plots/", characteristic, "index_tog.pdf"), height = 10, width = 7)

write.csv(plot_index, paste0("plots/", characteristic, "index.csv"))

### Stats for report 
high_all <- plot_index %>% filter(n_big > 3)

round(100*table(high_all%>% ungroup() %>% filter(gender == "N") %>% select(organism)) / dim(high_all%>% ungroup() %>% filter(gender == "N"))[1],0)
# In men
round(100*table(high_all %>% ungroup() %>% filter(gender == "m") %>% select(organism)) / dim(high_all %>% filter(gender=="m"))[1],0)
# In women
round(100*table(high_all %>% ungroup() %>% filter(gender == "f") %>% select(organism)) / dim(high_all %>% filter(gender=="f"))[1],0)

