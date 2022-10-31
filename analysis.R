library(MAGeCKFlute)
library(ggplot2)
library(patchwork)

qc <- function(count_summary, outdir) {
  # check count_summary
  if (!file.exists(count_summary)) {
    stop('ERROR: No such file!')
  } else {
    data <- read.delim(count_summary, check.names = FALSE)
  }
  
  # Gini index
  gini_index <- BarView(data, x = "Label", y = "GiniIndex",
          ylab = "Gini index", main = "Evenness of sgRNA reads")
  
  # Missed sgRNAs
  data$Missed = log10(data$Zerocounts)
  miss_sgrnas <- BarView(data, x = "Label", y = "Missed", fill = "#394E80",
          ylab = "Log10 missed gRNAs", main = "Missed sgRNAs")
  
  # Read mapping
  mapprates <- MapRatesView(data) + 
    theme(axis.text.x = element_text(angle=0, hjust = 0.5))
  
  p <- gini_index + miss_sgrnas + mapprates + plot_layout(nrow=1, widths = 5, heights = 5)
  ggsave(p, 
         filename = paste(outdir, '/qc.png', collapse = '', sep=''), 
         device = 'png',
         width = 40,
         height = 20,
         units = 'cm')
  
}


analysis <- function(gene_summary, sgRNA_summary) {
  # check gene_summary and sgRNA_summary
  if (!file.exists(gene_summary)) {
    stop(paste('ERROR: No such file (', gene_summary, ')!', sep=''))
  } else if (!file.exists(sgRNA_summary)) {
    stop(paste('ERROR: No such file (', sgRNA_summary, ')!', sep=''))
  }
  
  # read in gene_summary
  
}

qc('../Desktop/Edwin_libo/all_samples.countsummary.txt', 
   outdir = '../Desktop/Edwin_libo/')
