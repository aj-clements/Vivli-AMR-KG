# regression model 

# # full data weird rounding, so first round to nearest 0.00
# full_data[, mic2 := round(mic, digits = 3)]
# 
# # try transforming the data and then plot
# 
# full_data[,mic3 := log(mic2)/log(2)]
# 
# ggplot(full_data, aes(x = mic, y = prop)) 
# 
# testing <- full_data[, .N , by = mic2]

full_data$age_group <- factor(full_data$age_group, levels = 
                                c("0 to 2 Years", "3 to 12 Years", 
                                  "13 to 18 Years",  
                                  "19 to 64 Years", "65 to 84 Years",
                                  "85 and Over"))
full_data <- data.table(full_data)

# make some summarised categories
full_data[mic <= 0.008, mic_cat := "very low"]
full_data[mic > 0.008 &  mic <=0.06, mic_cat := "low"]
full_data[mic > 0.06 &  mic <=0.5, mic_cat := "mid-low"]
full_data[mic > 0.5 &  mic <=2, mic_cat := "mid-high"]
full_data[mic > 2 &  mic <=16, mic_cat := "high"]
full_data[mic > 16, mic_cat := "very high"]
full_data$mic_cat <- factor(full_data$mic_cat, levels =
                              c("very low","low", "mid-low", "mid-high",
                                "high", "very high"))

# subset to just look at one bug-dryg
lev_staph <- full_data[antibiotic == "levofloxacin" & 
                         organism_clean == 'Staphylococcus aureus']

#try a simple chi2
test <- lev_staph[, .N, by = c("age_group", "mic_cat")]
tab <- data.frame(dcast(test, age_group ~ mic_cat, value.var = "N"))
rownames(tab) <- tab$age_group
tab <- tab[-c(1)]

chisq.test(tab)
# this tells us that there are differences between the groups! But it doesn't control for anyhting

# A plot to understand
ggplot(lev_staph, aes(x = age_group, y = mic_cat)) + 
  geom_jitter(alpha = 0.3) + 
  labs( y = "MIC cat", title = "levo - staph") 

# convert to categorical for eah value
lev_staph[, mic_cat_all := factor(11 + round(log(mic)/log(2)))]

# try running a proportional odds ordinal model
ord_mod <- polr(mic_cat ~ age_group + gender + year + key_source , data = lev_staph)
summary(ord_mod)
#odds of the variables
exp(coef(ord_mod))
# older = more likely to have higher MIC. Gender and age make a difference but much less


