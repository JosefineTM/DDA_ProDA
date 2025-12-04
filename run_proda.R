#--------------------------------
# ProDA
#--------------------------------


library(tidyverse)

if(!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("proDA")
library(proDA)
library(optparser)

# -------------------------
# Functions
# -------------------------

load_dataset <- function(data_file) {
  data <- read.csv(data_file, row.names = 1, check.names = FALSE)
  data <- as.matrix(data)
  
  return(data)
}


load_labels <- function(labels_file) {
  labels <- read.csv(labels_file, row.names = 1, header = FALSE) 
  colnames(labels) <- "condition"
  labels <- labels %>%pull(condition)
  return(labels)
}


# -------------------
# Run
# --------------------

# parse options
option_list <- list(
  make_option(c("-d", "--data.matrix")),
  make_option(c("-l", "--data.true_labels")),
  make_option(c("-o", "--output_dir")), 
  make_option(c("-n", "--name")) 
)

parser <- OptionParser(option_list = option_list)
args <- parse_args(parser)

# data <- load_dataset('/Users/bkg618/Desktop/out/data/s1/default/s1.synthetic_dataset.csv')
data <- load_dataset(read.csv(args$data.matrix, check.names = FALSE, stringsAsFactors = FALSE))

# labels <- load_labels('/Users/bkg618/Desktop/out/data/s1/default/s1.true_labels.csv')
labels <- load_labels(read.csv(args$data.true_labels, header= FALSE))

# Fit data
fit <- proDA(data, design = labels)

# Run diff test
test <- test_diff(fit, `0` - `1`)
test <- as.data.frame(test) %>%
  select(c(name, pval, diff)) %>%
  rename(P.Value = pval, Protein = name, effect_size = diff)

# Save
output_filename <- paste0(args[["name"]], "_results.csv")
write.csv(test, file=file.path(args[["output_dir"]], output_filename), row.names = FALSE)

