# function to draw box plots of TPM measurements ordered by abundance
# while labeling genes of interest subset

library(tidyverse)


# Function for box plot, takes TPM data frame with gene IDs as row names 
#    and table for GOI name IDs with Gene.Name and spp ID columns
plot.abundance.labelGOI <- function(DF.TPM, DF.GOI.NAMES, 
                                    TITLE = NULL, GOI_LAB = "Genes Of Interest",
                                    LOGTPM = TRUE){
  
  # take logTPM unless specified
  if(LOGTPM){ 
    DF.TPM <- log(DF.TPM + 0.1)
    TPMlab = "logTPM"
  } else {
    TPMlab = "TPM"
  }
  
  # meanTPMs for ordering 
  meanTPM <- rowMeans(DF.TPM)
  
  # pivot df to long form
  DF.TPM$GeneIDs <- rownames(DF.TPM)
  DF.long <- DF.TPM %>% gather(key = rep, value = TPMval, -GeneIDs)
  
  # Formatting GOI names table
  # make sure GOI name dataframe has Gene.Name as first column
  GOI_names <- DF.GOI.NAMES %>% select(Gene.Name, everything())
  # standardize column names
  colnames(GOI_names) <- c("Gene.Name", "GeneIDs")
  # remove NAs
  GOI_names <- GOI_names[!is.na(GOI_names$GeneIDs),]
  
  # Label which SPFs are GOIs in long data frame and format name and display order
  DF.long$GOI <- is.element(DF.long$GeneIDs, GOI_names$GeneIDs)
  DF.long$GOI[DF.long$GOI] = GOI_LAB
  DF.long$GOI[!is.element(DF.long$GeneIDs, GOI_names$GeneIDs)] = paste("Not", GOI_LAB, sep = " ")
  DF.long$GOI <- factor(DF.long$GOI, levels = c(GOI_LAB, paste("Not", GOI_LAB, sep = " ")))
  
  # Build data table for GOI gene name label positions
  GOI_labels <- DF.long[DF.long$GOI == GOI_LAB,] %>% 
    group_by(GeneIDs) %>% 
    summarise(ypos = max(TPMval) + 0.1) %>%
    inner_join(., GOI_names)
  GOI_labels$GOI = GOI_LAB
  
  # call boxplot from long form df
  ggplot(DF.long, aes(x = factor(GeneIDs, levels = names(meanTPM[order(-meanTPM)])), 
                      y = TPMval, fill = GOI)) + 
    geom_boxplot() + 
    theme_minimal() +
    scale_fill_manual(values = c("gold", "cornflowerblue")) +
    ggtitle(TITLE) +
    labs(fill = "Gene Category") +
    xlab("Ranked abundance") +
    ylab(paste("Mean abundance (", TPMlab, ")", sep = "")) +
    theme(axis.text.x = element_blank()) +
    geom_text(data = GOI_labels, aes(label = Gene.Name, y = ypos),
              show.legend = FALSE,
              angle = 90, size = 2, hjust = "left", fontface = "bold")
  
}

# Make some example data for demo
set.seed(46)
random.TPMs.baseline <- rnorm(n = 99, mean = 1000, sd = 300)

EX_TPM <- data.frame(rep1 = random.TPMs.baseline + rnorm(n = 99, mean = 0, sd = 100),
                     rep2 = random.TPMs.baseline + rnorm(n = 99, mean = 0, sd = 110),
                     rep3 = random.TPMs.baseline + rnorm(n = 99, mean = 0, sd = 90),
                     row.names = paste("Gene", sprintf("%02d", 1:99), sep = "_"))

EX_GOI <- data.frame(Gene.Name = c("CoolGeneA", "MFG", "YFG", "PlotMe"),
                     Gene.ID = c("Gene_14", "Gene_67", "Gene_04", "Gene_23"))

plot.abundance.labelGOI(EX_TPM, EX_GOI, 
                        TITLE = "Demo GOI Plot",
                        GOI_LAB = "Cool Genes")
