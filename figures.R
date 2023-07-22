# Vivli figures 
library(data.table);library(ggplot2);library(cowplot); library(patchwork)

# read in the data
#full_data <- as.data.table(read.csv("data/full_data.csv"))
source("overlapping_drugs.R")

# Which characteristics explored? 
characteristics <- c("age_group", "key_source") # "age_group"

############################# Get data
output_data <- c()
for(i in characteristics){
  output1 <- read.csv(paste0("plots/gender_",i, "output.csv")) %>% mutate(charac = i) %>% rename("charac_value" = i)
  output2 <- read.csv(paste0("plots/",i, "output.csv")) %>% mutate(gender = "N", charac = i) %>% rename("charac_value" = i)
  output_data <- rbind(rbind(output_data, 
                             output1 %>% select("gender", "MIC", "charac", "charac_value", "N", "Total", "prop", "cumulative_sum", 
                                                "antibiotic", "organism")), 
                       output2 %>% select("gender", "MIC", "charac", "charac_value", "N", "Total", "prop", "cumulative_sum", 
                                          "antibiotic", "organism"))
}

###### Index comparison 
index_comparison_gender <-read_csv(paste0("plots/gender_",characteristic,"index_store.csv"))
index_comparison <- read_csv(paste0("plots/",characteristic,"index_store.csv"))

sum_index_gender <- index_comparison_gender %>% 
  group_by(antibiotic, organism, gender) %>% 
  summarise(mx = max(abs(dff)), n_big = sum(dff > 0.1)) 
sum_index <- index_comparison %>% group_by(antibiotic, organism) %>% summarise(mx = max(dff), n_big = sum(dff > 0.2)) %>% mutate(gender = "N")

index_data <- rbind(sum_index_gender, sum_index)

### EXAMPLES
combinations <- as.data.frame(rbind(c("Staphylococcus aureus", "levofloxacin"),
                                    c("Escherichia coli", "levofloxacin"),
                                    c("Staphylococcus aureus", "meropenem"),
                                    c("Escherichia coli", "meropenem")))
colnames(combinations) <- c("organism","antibiotic")


# Grab just the data wanted for the examples
plot_data <- c()
plot_index <- c()

for(i in 1:nrow(combinations)){
  plot_data <- rbind(plot_data,
                     output_data %>% filter(organism == combinations[i,"organism"],
                                            antibiotic == combinations[i,"antibiotic"]))
  plot_index <- rbind(plot_index, 
                      index_data %>% filter(organism == combinations[i,"organism"],
                                 antibiotic == combinations[i,"antibiotic"])) 
}

#################################### Age only
plot_age <- plot_data %>% filter(charac == "age_group")
plot_age$age_group <- factor(plot_age$age_group, 
                                 levels = c("0 to 2 Years","3 to 12 Years", "13 to 18 Years",
                                            "19 to 64 Years", "65 to 84 Years", "85 and Over"))

ggplot(plot_age %>% filter(gender == "N", MIC < 10), 
       aes(x=MIC, y = cumulative_sum, group = charac_value)) + 
  geom_line(aes(col = charac_value)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_continuous("MIC") + 
  scale_y_continuous("Cumulative sum of isolates tested") + 
  scale_color_discrete("Age group")
ggsave("plots/fig2_age.pdf")

# Age and gender 
g1 <- ggplot(plot_age %>% filter(!gender == "N", MIC < 1), 
       aes(x=MIC, y = cumulative_sum, group = interaction(gender,charac_value))) + 
  geom_line(aes(col = charac_value, lty = gender)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_continuous("MIC") + 
  scale_y_continuous("Cumulative sum of isolates tested") + 
  scale_color_discrete("Age group") + 
  scale_linetype_discrete("Sex")
ggsave("plots/fig2_age_sex_zoom.pdf")

g2 <- ggplot(plot_age %>% filter(!gender == "N"), 
             aes(x=MIC, y = cumulative_sum, group = interaction(gender,charac_value))) + 
  geom_line(aes(col = charac_value, lty = gender)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_continuous("MIC") + 
  scale_y_continuous("Cumulative sum of isolates tested") + 
  scale_color_discrete("Age group") + 
  scale_linetype_discrete("Sex")
ggsave("plots/fig2_age_sex.pdf")

g1 + g2 + plot_layout(guides = "collect")
ggsave("plots/fig2_age_sex_tog.pdf")

ggplot(plot_index %>% filter(n_big > 3), 
       aes(x=antibiotic, y = mx, group = interaction(organism, gender))) + geom_point(aes(col = organism, pch = gender)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  geom_hline(yintercept = mean(sum_index$mx)) + 
  geom_hline(yintercept = quantile(sum_index$mx)[2], lty = "dashed") + geom_hline(yintercept = quantile(sum_index$mx)[4], lty = "dashed")+ 
  ggtitle(characteristic) + 
  scale_y_continuous("Maximum difference in MIC\nacross groupings")

######################## Source
ggplot(plot_data %>% filter(charac == "key_source", gender == "N"), 
       aes(x=MIC, y = cumulative_sum, group = charac_value)) + 
  geom_line(aes(col = charac_value)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_continuous("MIC") + 
  scale_y_continuous("Cumulative sum of isolates tested") + 
  scale_color_discrete("Age group")
ggsave("plots/fig3_source.pdf")

# Source and gender 
g1 <- ggplot(plot_data %>% filter(charac == "key_source", !charac_value == "", !gender == "N", MIC < 1), 
       aes(x=MIC, y = cumulative_sum, group = interaction(gender,charac_value))) + 
  geom_line(aes(col = charac_value, lty = gender)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_continuous("MIC") + 
  scale_y_continuous("Cumulative sum of isolates tested") + 
  scale_color_discrete("Isolate source") + 
  scale_linetype_discrete("Sex")
ggsave("plots/fig3_source_sex_zoom.pdf")

g2 <- ggplot(plot_data %>% filter(charac == "key_source", !charac_value == "", !gender == "N"), 
       aes(x=MIC, y = cumulative_sum, group = interaction(gender,charac_value))) + 
  geom_line(aes(col = charac_value, lty = gender)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_continuous("MIC") + 
  scale_y_continuous("Cumulative sum of isolates tested") + 
  scale_color_discrete("Isolate source") + 
  scale_linetype_discrete("Sex")
ggsave("plots/fig3_source_sex.pdf")

g1 + g2 +  plot_layout(guides = "collect")
ggsave("plots/fig3_source_sex_tog.pdf")




