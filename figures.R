# Vivli figures 
library(data.table);library(ggplot2);library(cowplot); library(patchwork); library(tidyverse)
theme_set(theme_bw())
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
                      index_data %>% filter(organism == combinations[i,"organism"],
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
  scale_y_continuous("Cumulative sum of isolates tested") + 
  scale_color_discrete("Age group")
ggsave("plots/fig2_age.pdf")

# Age and gender 
g1 <- ggplot(plot_age %>% filter(!gender == "N"), 
       aes(x=MIC, y = cumulative_sum, group = interaction(gender,charac_value))) + 
  geom_line(aes(col = charac_value, lty = gender)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_log10("MIC", labels = scales::comma) + 
  scale_y_continuous("Cumulative sum of isolates tested") + 
  scale_color_discrete("Age group") + 
  scale_linetype_discrete("Sex", labels = c("Female","Male"), breaks = c("f","m")) + 
  theme(legend.position = "bottom", strip.text = element_text(face = "italic"))
ggsave("plots/fig2_age_sex.pdf")


g2 <- ggplot(output_index %>% filter(charac == "age_group",n_big > 3),
             aes(y=antibiotic, x = mx, group = interaction(organism, gender))) + 
  geom_point(aes(col = organism, pch = gender), size = 3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle("Age and Sex") + 
  scale_color_discrete("Organism") + 
  scale_x_continuous(limits = c(0,0.4),"Maximum difference in MIC across groupings") + 
  scale_shape_discrete("Sex",breaks = c("f","m","N"), labels = c("Female","Male","Both")) + 
  geom_vline(xintercept = c(0.1,0.2,0.3), lty = "dashed") + 
  theme(legend.text = element_text(face = "italic"))
ggsave("plots/fig2_index_age_sex.pdf")

######################## Source
ggplot(plot_data %>% filter(charac == "key_source", gender == "N"), 
       aes(x=MIC, y = cumulative_sum, group = charac_value)) + 
  geom_line(aes(col = charac_value)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_log10("MIC") + 
  scale_y_continuous("Cumulative sum of isolates tested") + 
  scale_color_discrete("Age group")
ggsave("plots/fig3_source.pdf")

# Source and gender 
g3 <- ggplot(plot_data %>% filter(charac == "key_source", !charac_value == "", !gender == "N"), 
             aes(x=MIC, y = cumulative_sum, group = interaction(gender,charac_value))) + 
  geom_line(aes(col = charac_value, lty = gender)) + 
  facet_grid(organism ~ antibiotic, scales = "free") + 
  scale_x_log10("MIC") + 
  scale_y_continuous("Cumulative sum of isolates tested") + 
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
  scale_x_continuous(limits = c(0,0.4), "Maximum difference in MIC across groupings") + 
  scale_shape_discrete("Sex",breaks = c("f","m","N"), labels = c("Female","Male","Both")) + 
  geom_vline(xintercept = c(0.1,0.2,0.3), lty = "dashed") + 
  theme(legend.text = element_text(face = "italic"))
ggsave("plots/index_key_source.pdf")

#### Figure 2
a <- g2 + g4 + plot_layout(guides = "collect") 

a / (g1 + g3 + plot_layout(guides = "collect") & theme(legend.position = "bottom"))
ggsave("plots/fig2.pdf", width = 18, height = 10)

### Version 2

a <- g2 + g4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
b <- g1 + g3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
a / b + plot_layout(heights = c(1, 1.8))
ggsave("plots/fig2_v2.pdf", width = 20, height = 11)



