# MICAG: This script creates the figures used in the report as part of the Data Challenge

library(data.table);library(ggplot2);library(cowplot); library(patchwork); library(tidyverse)
theme_set(theme_bw(base_size = 16))
# read in the data
#full_data <- as.data.table(read.csv("data/full_data.csv"))
source("overlapping_drugs.R")

# Which characteristics explored? 
characteristics <- c("age_group", "key_source") # "age_group"

############################# Get data
output_data <- c()
output_index <- c()

for(i in characteristics){
  print(i)
  
  ###### Output no time 
  output1 <- read.csv(paste0("plots/gender_",i, "output.csv")) %>% mutate(charac = i) %>% rename("charac_value" = i)
  output2 <- read.csv(paste0("plots/",i, "output.csv")) %>% mutate(gender = "N", charac = i) %>% rename("charac_value" = i)
  output_data <- rbind(rbind(output_data, 
                             output1 %>% select("gender", "MIC", "charac", "charac_value", "N", "Total", "prop", "cumulative_sum", 
                                                "antibiotic", "organism")), 
                       output2 %>% select("gender", "MIC", "charac", "charac_value", "N", "Total", "prop", "cumulative_sum", 
                                          "antibiotic", "organism"))
  
  ###### Index comparison
  index_comparison <- read.csv(paste0("plots/", i, "index.csv")) %>% mutate(charac = i)
  output_index <- rbind(output_index, index_comparison)
  
}

### EXAMPLES
combinations <- as.data.frame(rbind(c("Staphylococcus aureus", "levofloxacin"),
                                    c("Escherichia coli", "levofloxacin"),
                                    c("Staphylococcus aureus", "meropenem"),
                                    c("Escherichia coli", "meropenem"),
                                    c("Staphylococcus aureus", "ampicillin"),
                                    c("Escherichia coli", "ampicillin")))
colnames(combinations) <- c("organism","antibiotic")


# Grab just the data wanted for the examples
plot_data <- c()
plot_index <- c()

for(i in 1:nrow(combinations)){
  plot_data <- rbind(plot_data,
                     output_data %>% filter(organism == combinations[i,"organism"],
                                            antibiotic == combinations[i,"antibiotic"]))
  
  plot_index <- rbind(plot_index, 
                      output_index %>% filter(organism == combinations[i,"organism"],
                                              antibiotic == combinations[i,"antibiotic"])) 
}

#################################### Age only
plot_age <- plot_data %>% filter(charac == "age_group")
plot_age$charac_value <- factor(plot_age$charac_value, 
                                levels = c("0 to 2 Years","3 to 12 Years", "13 to 18 Years",
                                           "19 to 64 Years", "65 to 84 Years", "85 and Over"))

ggplot(plot_age %>% filter(gender == "N"), 
       aes(x=MIC, y = cumulative_sum, group = charac_value)) + 
  geom_line(aes(col = charac_value)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_log10("MIC", labels = scales::comma) + 
  scale_y_continuous("Cumulative proportion of isolates tested") + 
  scale_color_discrete("Age group")
ggsave("plots/age_only.pdf")

# Age and gender 
g1 <- ggplot(plot_age %>% filter(!gender == "N"), 
             aes(x=MIC, y = cumulative_sum, group = interaction(gender,charac_value))) + 
  geom_line(aes(col = charac_value, lty = gender)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_log10("MIC", labels = scales::comma) + 
  ggtitle("Age and Sex") + 
  scale_y_continuous("Cumulative proportion of isolates tested") + 
  scale_color_discrete("Age group") + 
  scale_linetype_discrete("Sex", labels = c("Female","Male"), breaks = c("f","m")) + 
  theme(legend.position = "bottom", strip.text = element_text(face = "italic"))
ggsave("plots/age_sex.pdf")


g2 <- ggplot(output_index %>% filter(charac == "age_group",n_big > 3),
             aes(y=antibiotic, x = mx, group = interaction(organism, gender))) + 
  geom_point(aes(col = organism, pch = gender), size = 3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle("Age and Sex") + 
  scale_color_discrete("Organism") + 
  scale_x_continuous(limits = c(0.1,0.4),"Maximum difference in MIC across groupings") + 
  scale_shape_discrete("Sex",breaks = c("f","m","N"), labels = c("Female","Male","Both")) + 
  geom_vline(xintercept = c(0.1,0.2,0.3), lty = "dashed") + 
  theme(legend.text = element_text(face = "italic"))
ggsave("plots/index_age_sex.pdf")

######################## Source
ggplot(plot_data %>% filter(charac == "key_source", gender == "N"), 
       aes(x=MIC, y = cumulative_sum, group = charac_value)) + 
  geom_line(aes(col = charac_value)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_log10("MIC") + 
  scale_y_continuous("Cumulative proportion of isolates tested") + 
  scale_color_discrete("Age group")
ggsave("plots/source.pdf")

# Source and gender 
g3 <- ggplot(plot_data %>% filter(charac == "key_source", !charac_value == "", !gender == "N"), 
             aes(x=MIC, y = cumulative_sum, group = interaction(gender,charac_value))) + 
  geom_line(aes(col = charac_value, lty = gender)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_log10("MIC") + 
  ggtitle("Source") + 
  scale_y_continuous("Cumulative proportion of isolates tested") + 
  scale_color_discrete("Isolate source") + 
  scale_linetype_discrete("Sex", labels = c("Female","Male"), breaks = c("f","m")) + 
  theme(legend.position = "bottom", strip.text = element_text(face = "italic"))
ggsave("plots/source_sex.pdf")

g4 <- ggplot(output_index %>% filter(charac == "key_source",n_big > 3), 
             aes(y=antibiotic, x = mx, group = interaction(organism, gender))) + 
  geom_point(aes(col = organism, pch = gender), size = 3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle("Source") + 
  scale_color_discrete("Organism") + 
  scale_x_continuous(limits = c(0.1,0.3), "Maximum difference in MIC across groupings") + 
  scale_shape_discrete("Sex",breaks = c("f","m","N"), labels = c("Female","Male","Both")) + 
  geom_vline(xintercept = c(0.1,0.2,0.3), lty = "dashed") + 
  theme(legend.text = element_text(face = "italic"))
ggsave("plots/index_key_source.pdf")

#### Figure 2
a <- g2 + g4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
b <- g1 + g3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
a / b + plot_layout(heights = c(1, 1.8)) + plot_annotation(tag_levels = 'A')
ggsave("plots/fig2.pdf", width = 20, height = 11)

### Figure for index graphic
ggplot(plot_age %>% filter(gender == "N", antibiotic == "levofloxacin", organism == "Staphylococcus aureus"), 
             aes(x=MIC, y = cumulative_sum, group = interaction(gender,charac_value))) + 
  geom_point(aes(col = charac_value), size = 10, pch = "x") + 
  geom_line(aes(col = charac_value), size = 2) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_log10("MIC", labels = scales::comma) + 
  scale_y_continuous("Cumulative proportion of isolates tested") + 
  scale_color_discrete("Age group") + 
  scale_linetype_discrete("Sex", labels = c("Female","Male"), breaks = c("f","m")) + 
  theme(legend.position = "none", strip.background = element_blank(),
        strip.text.x = element_blank(), strip.text.y = element_blank())



###### Final figure 
##### OVER TIME: OPTIONAL
# read in the data
############################# Get data
output_datat <- c()

for(i in characteristics){
  print(i)
  
  ###### Output with time 
  output1t <- read.csv(paste0("plots/year_gender_",i, "output.csv")) %>% mutate(charac = i) %>% rename("charac_value" = i)
  output2t <- read.csv(paste0("plots/year_",i, "output.csv")) %>% mutate(gender = "N", charac = i) %>% rename("charac_value" = i)
  output_datat <- rbind(rbind(output_datat, 
                              output1t %>% select("year","gender", "MIC", "charac", "charac_value", "N", "Total", "prop", "cumulative_sum", 
                                                  "antibiotic", "organism")), 
                        output2t %>% select("year","gender", "MIC", "charac", "charac_value", "N", "Total", "prop", "cumulative_sum", 
                                            "antibiotic", "organism"))
}

### EXAMPLES
combinations <- as.data.frame(rbind(c("Staphylococcus aureus", "levofloxacin"),
                                    c("Escherichia coli", "levofloxacin"),
                                    c("Staphylococcus aureus", "meropenem"),
                                    c("Escherichia coli", "meropenem"),
                                    c("Staphylococcus aureus", "ampicillin"),
                                    c("Escherichia coli", "ampicillin")))
colnames(combinations) <- c("organism","antibiotic")


# Grab just the data wanted for the examples
plot_datat <- c()

for(i in 1:nrow(combinations)){
  plot_datat <- rbind(plot_datat,
                      output_datat %>% filter(organism == combinations[i,"organism"],
                                              antibiotic == combinations[i,"antibiotic"]))
}

# Index 
### Over time: for MICAG analysis most interested in age_group comparisons with time
characteristic <- "age_group"
index_comparison_gender_yr <-read_csv(paste0("plots/year_gender_",characteristic,"index_store.csv"))

# Explore data: how many high level and extract max level over time 
sum_index_gender_yr <- index_comparison_gender_yr %>% 
  group_by(antibiotic, organism, gender, MIC, year) %>%
  summarise(df_mic = max(dff)) %>% # Get one value per MIC 
  group_by(antibiotic, organism, gender, year) %>%
  summarise(mx = max(df_mic), # Max diff for this bug_drug 
            n_big = sum(unique(df_mic) > 0.1)) # Count how many MIC have > 10% diffs

# heat map
gg <- sum_index_gender_yr %>% filter(n_big > 3)
ggplot(gg, aes(x=year, y = antibiotic, z = mx)) + 
  geom_tile(aes(fill = mx)) + 
  facet_grid(gender~organism) + 
  ggtitle(characteristic) + 
  scale_fill_continuous("Maximum\nindex") + 
  theme(strip.text = element_text(face = "italic"))
ggsave(paste0("plots/", characteristic, "index_time_heat_map_allbac.pdf"), height = 7, width = 15)

g1t <- ggplot(gg %>% filter(organism %in% c("Staphylococcus aureus","Escherichia coli")), aes(x=year, y = antibiotic, z = mx)) + 
  geom_tile(aes(fill = mx)) + 
  facet_grid(gender~organism) + 
  ggtitle("Age Group") + 
  scale_fill_continuous("Maximum\nindex") + 
  theme(strip.text = element_text(face = "italic")) + 
ggsave(paste0("plots/", characteristic, "index_time_heat_map.pdf"), height = 7, width = 15)

## Over time 
plot_datat_staphlevo <- plot_datat %>% filter(antibiotic == "levofloxacin",
                                              organism == "Staphylococcus aureus", 
                                              charac == characteristic) %>%
  ungroup %>% 
  group_by(gender, MIC, charac_value) %>% mutate(total_iso = sum(Total))

g2t <- ggplot(plot_datat_staphlevo %>% filter(total_iso > 1000), 
       aes(x = year, y = cumulative_sum, colour = charac_value, 
           group = interaction(gender, charac_value))) + 
  geom_line(aes(linetype = factor(gender))) + 
  facet_wrap(~MIC) + 
  scale_x_log10()
ggsave(paste0("plots/", characteristic, "time_cumulative.pdf"), height = 7, width = 15)

ggplot(plot_datat_staphlevo %>% filter(total_iso > 10000), 
       aes(x = year, y = cumulative_sum, colour = charac_value, 
           group = interaction(gender, charac_value))) + 
  geom_line(aes(linetype = factor(gender))) + 
  facet_wrap(~MIC) + 
  scale_x_log10()

g1t + g2t & theme(legend.position = "bottom")
ggsave(paste0("plots/", characteristic, "time_figure.pdf"), height = 7, width = 15)


### time plot
samples_store <- data.table(read.csv(paste0("plots/year_gender_",characteristic, "samples_store.csv")))

g_samples <- ggplot(samples_store, aes(x=Year, y=N, fill = age_group))+
  geom_col(col = "black")+
  labs(x = "Year",
       y = "Number of samples",
       fill = "Age group")+
  facet_grid(Gender~MIC)  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
