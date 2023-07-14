# Vivli initial cleaning and screening for interesting bugs
library(data.table);library(ggplot2);library(cowplot)

# SPECIFY What characteristic to look at. (Note: Must match column name)
characteristic <- "key_source"
include_gender <- T # T or F. should also split by gender?

# after specified the two above items, can just run the whole script and it will 
# generate the desired plots


# read in the data
full_data <- as.data.table(read.csv("data/full_data.csv"))
# make sure tehre's a folder to store the plots
dir.create(file.path("plots"), showWarnings = FALSE)

if(include_gender == F){
# look at just one bug for now (might loop this later!)
for(j in c("Staphylococcus aureus", "Escherichia coli", "Klebsiella pneumoniae")){
  data_sub <- full_data[organism == j]
  
  # vector for storing relevant drugs and plots
  drugs <- unique(data_sub$antibiotic)
  plot_store <- list()

  #for each of the relevant drugs
  for(i in drugs){
    data_sub_drug <- data_sub[antibiotic == i]
    #count observations by subset
    test <- data_sub_drug[, .N, by = .(mic, get(characteristic) )]
    colnames(test) <- c("MIC", characteristic, "N")
    # also need out of total observations for the age_group/gender
    test2 <- data_sub_drug[, .N, by = .(get(characteristic))]
    colnames(test2) <- c(characteristic, "N")
    # note total number of MIC samples
    tot_samps <- sum(test2$N)
    # combine the two together so can work out proportion
    test[test2, on = c(characteristic), Total := i.N]
    #work out proportion
    test[, prop := N/Total]
    # cumulative sum of proportion (first order)
    test <- test[order(MIC, get(characteristic))]
    for_plot <-test[, cumulative_sum := cumsum(prop), by = c(characteristic)]

    # store plot
    for_plot <- for_plot[N>100]
    if(nrow(for_plot)>0){
     temp<- ggplot(for_plot[N>=100], aes(x= MIC, y =cumulative_sum, colour = !!sym(characteristic))) + 
      geom_line()+
      labs(title = paste0("MIC - ", i, paste0(". Tot samples = ", tot_samps)), x = "MIC value", 
           y = paste0("cumulative proportion of samples by ", characteristic), 
           colour = characteristic) + 
      scale_x_log10() + 
      theme_linedraw() 
     }
    
plot_store[[i]] <- temp
  }
  
  tiff(paste0("plots/",j , "_", characteristic, "_MICs.tiff"), width = 2500, height = 1500)
  print(cowplot::plot_grid(plotlist =  plot_store) )
  dev.off()  
}
}

if(include_gender == T){
  # look at just one bug for now (might loop this later!)
  for(j in c("Staphylococcus aureus", "Escherichia coli", "Klebsiella pneumoniae")){
    
    data_sub <- full_data[organism == j]
    # vector for storing relevant drugs and plots
    drugs <- unique(data_sub$antibiotic)
    plot_store <- list()
    
    #for each of the relevant drugs
    for(i in drugs){
      data_sub_drug <- data_sub[antibiotic == i]
      #count observations by subset
      test <- data_sub_drug[, .N, by = .(gender, mic, get(characteristic) )]
      colnames(test) <- c("gender","MIC", characteristic, "N")
      # also need out of total observations for the age_group/gender
      test2 <- data_sub_drug[, .N, by = .(gender, get(characteristic))]
      colnames(test2) <- c("gender",characteristic, "N")
      # note total number of MIC samples
      tot_samps <- sum(test2$N)
      # combine the two together so can work out proportion
      test[test2, on = c(characteristic, "gender"), Total := i.N]
      #work out proportion
      test[, prop := N/Total]
      # cumulative sum of proportion (first order)
      test <- test[order(MIC, gender,  get(characteristic))]
      for_plot <-test[, cumulative_sum := cumsum(prop), by = c("gender", characteristic)]
    
      # store plot
      
      if(nrow(for_plot)>0){
        temp<- ggplot(for_plot, aes(x= MIC, y =cumulative_sum, colour = !!sym(characteristic), 
                                            linetype = gender)) + 
          geom_line()+
          labs(title = paste0("MIC by age group - ", i, paste0(". Tot samples = ", tot_samps)), x = "MIC value", 
               y = paste0("cumulative proportion of samples by ", characteristic), 
               colour = characteristic) + 
          scale_x_log10() + 
          theme_linedraw() 
      }
      
      plot_store[[i]] <- temp
    }
    
    tiff(paste0("plots/gender_",j , "_", characteristic, "_MICs.tiff"), width = 2500, height = 1500)
    print(cowplot::plot_grid(plotlist =  plot_store) )
    dev.off()  
  }
}
