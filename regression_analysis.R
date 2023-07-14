# regresssion analysis
library(data.table)
full_data <- as.data.table(read.csv("data/full_data.csv"))
# subset to just look at one bug-dryg
lev_staph <- full_data[antibiotic == "levofloxacin" & 
                         organism_clean == 'Staphylococcus aureus']

# convert to categorical for eah value
lev_staph[, mic_cat_all := factor(11 + round(log(mic)/log(2)))]

# try running a proportional odds ordinal model
ord_mod <- polr(mic_cat_all ~ age_group + gender  + key_source , data = lev_staph)
summary(ord_mod)
#odds of the variables
exp(coef(ord_mod))
#percentage change
(exp(coef(ord_mod))-1)*100
# older = more likely to have higher MIC. Gender and source make a difference but much less
#but also varies a lot by source!
