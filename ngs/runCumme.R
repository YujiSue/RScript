library(cummeRbund)
args <- commandArgs(trailingOnly = T)
root <- args[1]
input <- args[2]
output <- args[3]
options <- list(
    export = list(
        disp = FALSE,
        scv = FALSE,
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
#print(optargs)
for (o in 1:length(optargs[[1]])) {
    #print(as.character(optargs[[1]][o]))
    param <- strsplit(as.character(optargs[[1]][o]), "=")
    parname <- as.character(param[[1]][1])
  #  print(parname)
    if (parname == "pval") options$pval = as.numeric(param[[1]][2])
    else if (parname == "img") options$img = as.character(param[[1]][2])
    else if (parname == "export") {
 #       print(as.character(param[[1]][2]))
        exp <- strsplit(as.character(param[[1]][2]), ":")
        for (e in 1:length(exp[[1]])) {
#            print(as.character(exp[[1]][e]))
            expitem <- as.character(exp[[1]][e])
            if (expitem == "disp") options$export$disp = TRUE
            else if (expitem == "scv") options$export$scv = TRUE
            else if (expitem == "dens") options$export$dens = TRUE
            else if (expitem == "box") options$export$box = TRUE
            else if (expitem == "mat") options$export$mat = TRUE
            else if (expitem == "dendro") options$export$dendro = TRUE
            else if (expitem == "volca") options$export$volca = TRUE
            else if (expitem == "ma") options$export$ma = TRUE
            else if (expitem == "scat") options$export$scat = TRUE
        }
    }
}
# Set root directory
setwd(root)
if(file.exists("Rplots.pdf")) unlink("Rplots.pdf", recursive = FALSE, force = FALSE)
# Import cuffdiff result
cuff<-readCufflinks(input)
print(cuff)
group <-colnames(fpkmMatrix(genes(cuff)))
group_count <- length(group)
print(group, group_count)
# Export plots
if (options$export$disp == TRUE) {
    dispersionPlot(genes(cuff))
    path <- paste(output, "/dispersion.", options$img, sep = "")
    ggsave(path)
}
if (options$export$scv == TRUE) {
    fpkmSCVPlot(genes(cuff))
    path <- paste(output, "/scv.", options$img, sep = "")
    ggsave(path)
}
if (options$export$dens == TRUE) {
    csDensity(genes(cuff))
    path <- paste(output, "/density.", options$img, sep = "")
    ggsave(path)
}
if (options$export$box == TRUE) {
    csBoxplot(genes(cuff))
    path <- paste(output, "/box.", options$img, sep = "")
    ggsave(path)
}
if (options$export$mat == TRUE) {
    csScatterMatrix(genes(cuff))
    path <- paste(output, "/scatter_mat.", options$img, sep = "")
    ggsave(path)
}
if (options$export$dendro == TRUE) {
    csDendro(genes(cuff))
    path <- paste(output, "/dendro.pdf", sep = "")
    move_files("Rplots.pdf", path, overwrite = TRUE)
    #ggsave(path)
}
if (options$export$volca == TRUE) {
    csVolcanoMatrix(genes(cuff))
    path <- paste(output, "/volcano.", options$img, sep = "")
    ggsave(path)
}
if (options$export$ma == TRUE) {
    for (g in 2:group_count) {
        csScatter(genes(cuff), group[1], group[g], smooth=T)
        path <- paste(output, "/scatter_",group[1], "_vs_", group[g], ".", options$img, sep = "")
        ggsave(path)
    }
}
if (options$export$scat == TRUE) {
    for (g in 2:group_count) {
        MAplot(genes(cuff), group[1], group[g])
        path <- paste(output, "/ma_",group[1], "_vs_", group[g], ".", options$img, sep = "")
        ggsave(path)
    }
}
# Filter genes
sig_gene_ids<-getSig(cuff, alpha=options$pval, level='genes')
sig_genes<-getGenes(cuff, sig_gene_ids)
# Clustering genes by their expression level
cluster_num <- (2^group_count)
print(cluster_num)
cl <- csCluster(sig_genes, k=cluster_num)
# Export cluster result (txt)
print(cl)
path <- paste(output, "/cluster.csv", sep = "")
write.table(cl$clustering, path, append = FALSE, sep = ",", dec = ".",
            row.names = TRUE, col.names = TRUE)
# Export cluster plot (image)
csClusterPlot(cl)
path <- paste(output, "/cluster.", options$img, sep = "")
ggsave(path)
# Export heatmap
csHeatmap(sig_genes, cluster='both')
path <- paste(output, "/sigheat.", options$img, sep = "")
ggsave(path)
