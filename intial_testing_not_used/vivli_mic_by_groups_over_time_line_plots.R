# Vivli plots for MIC by characteristics/groupings over time (and gender)
library(data.table);library(ggplot2);library(cowplot); library(dplyr)

# Read in the data
full_data <- as.data.table(read.csv("data/full_data.csv"))
# Ensure there's a folder to store the plots
dir.create(file.path("plots"), showWarnings = FALSE)

# ---------------------------------------------------------------------

### CHOOSE BUG-DRUG COMBINATION
bug <- "Staphylococcus aureus"
drug <- "levofloxacin"
### SPECIFY WHICH CHARACTERISTIC/GROUPING TO STUDY (must match column name)
characteristic <- "age_group"
### INCLUDE GENDER? (T for yes, F for no)
include_gender <- T
### CHOOSE THRESHOLD NUMBER OF YEARLY SAMPLES FOR INCLUDING MEASURES
thresh <- 0

# ---------------------------------------------------------------------

# Set up store
plot_store <- list()

# Filter data by bug-drug and lift all reported MIC values
data_sub <- full_data[organism==bug]
data_sub_drug <- data_sub[antibiotic==drug]
miclist <- sort(unique(data_sub_drug$mic))

if(include_gender==F){
  # Loop to plot time graph for each mic
  for(x in 1:length(miclist)){
    # Filter by MIC
    data_sub_drug_m <- data_sub_drug[mic==miclist[x]]
    # Count samples with that MIC by characteristic/grouping AND year
    test_m_1 <- data_sub_drug_m[,.N,by=.(get(characteristic),year)]
    colnames(test_m_1) <- c(characteristic,"Year","N")
    # Count samples with that MIC by year ONLY
    test_m_2 <- data_sub_drug_m[,.N,by=.(year)]
    colnames(test_m_2) <- c("Year","N")
    # Merge filtered datasets
    test_m_1[test_m_2, on=c("Year"),Total:=i.N]
    # Calculate proportion of samples with that MIC per year for each characteristic/grouping
    test_m_1[,prop:=N/Total]
    # Include data for plotting if more samples in year than 'threshold'
    for_plot_m <- test_m_1[Total>thresh]
    # Plot proportions of samples with that MIC per year by characteristic/grouping
    if(nrow(for_plot_m)>0){
      temp_m <- ggplot(for_plot_m, aes(x=Year, y=prop, colour = !!sym(characteristic)))+
        geom_line(size=1.5)+
        labs(title = paste0("MIC = ", miclist[x],"mg/L"), x = "Year",
            y = paste0("Proportion of samples per year by ", characteristic),
            colour = characteristic)+
        scale_x_continuous(breaks=2004:2022,limits=c(2003,2023))+
        scale_y_continuous(limits=c(0,1))+
        theme_linedraw()
      
    # Store the plot for that MIC
    plot_store[[x]] <- temp_m
    }
  }
  
  # Combine plots for each MIC into final figure output
  tiff(paste0("plots/",bug,"_",drug,"_",characteristic,"_MICs_over_time.tiff"),width=2500,height=1500)
  print(cowplot::plot_grid(plotlist=plot_store))
  dev.off()
}

if(include_gender==T){
  # Loop to plot time graph for each mic
  for(x in 1:length(miclist)){
    # Filter by MIC
    data_sub_drug_m <- data_sub_drug[mic==miclist[x]]
    # Count samples with that MIC by characteristic/grouping AND gender AND year
    test_m_1 <- data_sub_drug_m[,.N,by=.(gender,get(characteristic),year)]
    colnames(test_m_1) <- c("Gender",characteristic,"Year","N")
    # Count samples with that MIC by gender AND year, NOT characteristic/grouping
    test_m_2 <- data_sub_drug_m[,.N,by=.(gender,year)]
    colnames(test_m_2) <- c("Gender","Year","N")
    # Merge filtered datasets
    test_m_1[test_m_2, on=c("Gender","Year"),Total:=i.N]
    # Calculate proportion of samples with that MIC in each gender per year for each characteristic/grouping
    test_m_1[,prop:=N/Total]
    # Include data for plotting if more samples in year than 'threshold'
    for_plot_m <- test_m_1[Total>thresh]
    # Plot proportions of samples with that MIC in each gender per year by characteristic/grouping
    if(nrow(for_plot_m)>0){
      temp_m <- ggplot(for_plot_m, aes(x=Year, y=prop, linetype=Gender, colour = !!sym(characteristic)))+
        geom_line(size=1.5)+
        labs(title = paste0("MIC = ", miclist[x],"mg/L"), x = "Year",
             y = paste0("Proportion of samples per year by ", characteristic),
             colour = characteristic)+
        scale_x_continuous(breaks=2004:2022,limits=c(2003,2023))+
        scale_y_continuous(limits=c(0,1))+
        theme_linedraw()
      
      # Store the plot for that MIC
      plot_store[[x]] <- temp_m
    }
  }
  
  # Combine plots for each MIC into final figure output
  tiff(paste0("plots/",bug,"_",drug,"_",characteristic,"_MICs_over_time_by_gender.tiff"),width=2500,height=1500)
  print(cowplot::plot_grid(plotlist=plot_store))
  dev.off()
}

# Clean environment

rm(list=c("bug",
          "drug",
          "characteristic",
          "include_gender",
          "thresh",
          "plot_store",
          "data_sub",
          "data_sub_drug",
          "miclist",
          "data_sub_drug_m",
          "test_m_1",
          "test_m_2",
          "for_plot_m",
          "temp_m",
          "x"))
