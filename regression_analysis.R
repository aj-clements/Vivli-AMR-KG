# regresssion analysis
library(data.table)
library(MASS)

full_data <- as.data.table(read.csv("data/full_data.csv"))

# specify which bacteria and antibiotic of interest
# NOTE - must match spelling in the data table
target_antibiotic <- "levofloxacin"
target_bug <- 'Staphylococcus aureus'

# subset to just look at one bug-dryg
sub_data <- full_data[antibiotic == target_antibiotic & 
                         organism_clean == target_bug]

# make children the base age group 
sub_data$age_group <- factor(sub_data$age_group, levels = c(
  "19 to 64 Years", 
  "0 to 2 Years",
  "3 to 12 Years", 
   "13 to 18 Years",
   "65 to 84 Years", 
   "85 and Over"
  
))

# convert to categorical for eah value
sub_data[, mic_cat_all := round(log(mic)/log(2))]
add_to_make_0 <- -min(unique(sub_data$mic_cat_all))
sub_data[, mic_cat_all := factor(add_to_make_0 + round(log(mic)/log(2)))]

# try running a proportional odds ordinal model
ord_mod <- polr(mic_cat_all ~ age_group + gender  + key_source , data = sub_data)
summary(ord_mod)

summary_table <- coef(summary(ord_mod))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table <- as.data.frame(summary_table)
summary_table$parameter <- rownames(summary_table)
summary_table <- data.table(summary_table)
summary_table[, Odds := exp(Value)]

summary_table <- summary_table[1:11,c("parameter", "Value", "Std. Error", "p value", "Odds")]

summary_table$Value <- round(summary_table$Value, 3)
summary_table$`Std. Error` <- round(summary_table$`Std. Error`, 3)
summary_table$Odds <- round(summary_table$Odds, 3)
write.csv(summary_table, file = paste0("regresssion_coefficients_", target_antibiotic, 
                                       "_", target_bug,".csv"))
