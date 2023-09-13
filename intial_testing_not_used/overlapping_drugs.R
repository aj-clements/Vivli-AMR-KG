# check for double drugs

library(data.table)
full_data <- as.data.table(read.csv("data/full_data.csv"))
bugs_wanted <- c("Staphylococcus aureus", "Escherichia coli")
bugs_data <- full_data[organism %in% bugs_wanted]

staph_drugs <- unique(bugs_data[organism %in% bugs_wanted[1]]$antibiotic)
ecoli_drugs <- unique(bugs_data[organism %in% bugs_wanted[2]]$antibiotic)
drugs_in_both <- staph_drugs[which(staph_drugs %in% ecoli_drugs)]
