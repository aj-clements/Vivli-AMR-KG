### MIC distribution differences 

library(tidyverse)

full_data <- read.csv("data/full_data.csv")

####****** Is the MIC distribution different between datasets? 
levo_staph <-  full_data[organism_clean %in% c("Staphylococcus aureus") & antibiotic %in% c("levofloxacin")]

test <- levo_staph[, .N, by = .(age_group, mic,data )]
test2 <- levo_staph[, .N, by = .(age_group, data)]
test[test2, on = c("age_group", "data"), total := i.N]
test[,prop := N/total]
test <- test[order(mic, data, age_group)]
for_plot <-test[, cumulative_sum := cumsum(prop), by = c("data", "age_group")]
# does MIC vary across data sets?
ggplot(for_plot, aes(x= mic, y =cumulative_sum, colour = data)) + 
  geom_line()+
  scale_x_log10() + 
  facet_grid(.~age_group) + 
  theme_linedraw() 

# check relative numbers from different data sets
for_plot[,sum(N), by = data]




