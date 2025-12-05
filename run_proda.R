#!/usr/bin/env Rscript


#--------------------------------
# ProDA
#--------------------------------


library(dplyr)

# if(!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("proDA")
library(proDA)
library(optparse)

# suppressPackageStartupMessages({
#   library(optparse)
# })

# -------------------------
# Functions
# -------------------------

load_dataset <- function(data_file) {
  data <- read.csv(data_file, check.names = FALSE, stringsAsFactors = FALSE) #row.names = 1
  rownames(data) <- make.unique(data[[1]]) # TODO: should not be done here- duplicates should be remove when the cancer data is created
  data[[1]] <- NULL
  data <- as.matrix(data)
  return(data)
}


load_labels <- function(labels_file) {
  labels <- read.csv(labels_file, header = FALSE)  # row.names = 1
  labels_vec <- unlist(labels[,2])
  #colnames(labels) <- "condition"
  labels <- factor(labels_vec)
  #labels <- labels %>%pull(condition)
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
data_path <- args$data.matrix
data <- load_dataset(data_path)

# labels <- load_labels('/Users/bkg618/Desktop/out/data/s1/default/s1.true_labels.csv')
labels_path = args$data.true_labels
labels <- load_labels(labels_path)

# Fit data
fit <- proDA(data, design = labels)

# Run diff test
test <- test_diff(fit, `0` - `1`)
test <- as.data.frame(test) %>% 
        select(c(name, pval, diff)) %>%
        rename(ID= name, P.Value = pval, effect_size = diff)

  #select(c(name, pval, diff)) %>%
  #select(c(pval, diff)) %>%
  #rename(P.Value = pval, effect_size = diff)
  #rename(P.Value = pval, ID = name, effect_size = diff)
test$ID <- sub("\\.\\d+$", "", test$ID)
#test$ID <- rownames(test)
# Strip numeric suffixes added by make.unique
#test$ID <- sub("\\.\\d+$", "", rownames(test))
# remove row names
rownames(test) <- NULL
#test <- test[, c("ID", setdiff(names(test), "ID"))]
# Save
output_filename <- paste0(args[["name"]], "_results.csv")
write.csv(test, file=file.path(args[["output_dir"]], output_filename), row.names = FALSE)

