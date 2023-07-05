# Vivli initial cleaning and screening for interesting bugs
library(data.table);library(ggplot2);library(cowplot)

# SPECIFY What characteristic to look at. (Note: Must match column name)
characteristic <- "Source"#"Age.Group"

# read in the data
#atlas <- read.csv("~/Desktop/atlas_input.csv")
atlas <- 
atlas <- data.table(atlas)

# function to replace the greater than etc. symbols across the datatable
remove_symbols <- function(input_column){
  # specify symbols to remove
  input_column <- gsub("<=", "", as.character(input_column))
  input_column <- gsub("=>", "", as.character(input_column))
  input_column <- gsub("<", "", as.character(input_column))
  input_column <- gsub(">", "", as.character(input_column))
  #change "" to NAs
  input_column[which(input_column == "")] <- NA
  # return it back
  return(input_column)
}

# apply the function to the data.table (takes a couple mins)
atlas <- atlas[, lapply(.SD, remove_symbols)]

# order the age groups
atlas$Age.Group <- factor(atlas$Age.Group, 
                          levels = c("0 to 2 Years",  "3 to 12 Years", "13 to 18 Years",
                                     "19 to 64 Years","65 to 84 Years", "85 and Over", 
                                     "Unknown"))

#find all the drug names
colnames_atlas <- colnames(atlas_sub)
drugs_I <- colnames_atlas[grepl("_I", colnames_atlas, fixed = T)]
drugs <- gsub("_I", "", drugs_I)

# look at just one bug for now (might loop this later!)
for(j in c("Staphylococcus aureus", "Escherichia coli", "Klebsiella pneumoniae")){
  atlas_sub <- atlas[Species == j]
  
  # vector for storing relevant drugs and plots
  drug_holder <- c()
  plot_store <- list()
  
  # check which drugs are relevant for this bug (stored in drug_holder)
  for(i in  1:length(drugs)){
    # specify drugs
    target_drug <- drugs[i]
    #check to see if there are any MIC values for the drugs
    if(dim(table(atlas_sub[,..target_drug])) != 0){
      drug_holder <- c(drug_holder, target_drug)}
  }
  
  #for each of the relevant drugs
  for(i in drug_holder){
    # subset cols of interest
    interested_cols <- c("Country", "Gender", "Age.Group", "Source", "Year", "In...Out.Patient", i)
    atlas_sub_drug <- atlas_sub[,..interested_cols]
    # remove NA values
    atlas_sub_drug <- atlas_sub_drug[!is.na(Age.Group) & !is.na(Gender) &
                                       !is.na(get(i))]
    #count observations by subset
    test <- atlas_sub_drug[, .N, by = .(Gender, get(i),get(characteristic) )]
    colnames(test) <- c("Gender", "MIC", characteristic, "N")
    test$MIC <- as.numeric(test$MIC)
    # also need out of total observations for the age_group/gender
    test2 <- atlas_sub_drug[, .N, by = .(Gender, get(characteristic))]
    colnames(test2) <- c("Gender", characteristic, "N")
    # note total number of MIC samples
    tot_samps <- sum(test2$N)
    # combine the two together so can work out proportion
    test[test2, on = c("Gender", characteristic), Total := i.N]
    #work out proportion
    test[, prop := N/Total]
    # cumulative sum of proportion (first order)
    test <- test[order(MIC, Gender, get(characteristic))]
    for_plot <-test[, cumulative_sum := cumsum(prop), by = c("Gender", characteristic)]
    # if it's the first one make and take legend first
    if(i == drug_holder[1]){
      temp <- ggplot(for_plot, aes(x= MIC, y =cumulative_sum, colour = !!sym(characteristic))) + 
        labs(colour = characteristic) + 
        facet_grid(.~Gender) 
      # extract legend
      shared_legend <- get_legend(temp)
    }
    # store plot
    for_plot <- for_plot[N>100]
    if(nrow(for_plot)>0){
     temp<- ggplot(for_plot[N>=100], aes(x= MIC, y =cumulative_sum, colour = !!sym(characteristic))) + 
      geom_line()+
      labs(title = paste0("MIC by age group - ", i, paste0(". Tot samples = ", tot_samps)), x = "MIC value", 
           y = "cumulative proportion of samples by age group and gender", 
           colour = characteristic) + 
      scale_x_log10() + 
      facet_grid(.~Gender) + 
      theme_linedraw() + 
      theme(legend.position = "NONE")
     }
    
plot_store[[i]] <- temp
  }
  
  tiff(paste0(j , "_", characteristic, "_MICs.tiff"), width = 2500, height = 1500)
  print(cowplot::plot_grid(plotlist =  plot_store) )
  dev.off()  
}

