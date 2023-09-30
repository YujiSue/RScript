requireNamespace("BiocManager")
library(edgeR)


args <- commandArgs(trailingOnly = T)
root <- args[1]
input <- args[2]
groups <- strsplit(args[3], ",")[[1]]
output <- args[3]
options <- list(
    export = list(
        disp = FALSE,
        dens = FALSE,
        box = FALSE,
        mat = FALSE,
        dendro = FALSE,
        volca = FALSE,
        ma = FALSE,
        scat = FALSE
    ),
    pval = 0.05,
    img = "png"
)
optargs <- strsplit(args[4], ",")


# Set root directory
setwd(root)

count
group <- factor(c("A", "A", "A", "B", "B", "B"))

