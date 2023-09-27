
rearrangeDF<-function(data, type="table", dir="V", nfactor=1) {
    if (type=="table") {
        if (nfactor = 1) {
            value <- c()
            factor <- c()
            if (dir == "V") {
                for (col in c(1:ncol(data))) {
                    count = length(data[,col])
                    factor <- c(factor, rep(col, count))
                    value <- append(value, data[,col])
                }
            }
            else {
                for (row in c(1:nrow(data))) {
                    count = length(data[,col])
                    factor <- c(factor, rep(col, count))
                    value <- append(value, data[,col])
                }
            }
            available <- complete.cases(value, factor)
            value <- value[available]
            factor <- factor(factor[available])            
            return(data.frame(Factor=factor(factor), Value=value))
        }
        else {

        }
        

    }
    else if (type=="ts") {

    }
    


    df<-data.frame()
    

}

binomTest<-function(data, prob) {
    testname<-""
    estimate<-prob
    colnames <- colnames(data)
    cnum <- length(colnames)
    trials<-c()
    successes<-c()
    pvals<-c()
    confs<-c()
    for (i in c(1:cnum)) {
        nrow <- 20
        trials<-append(trials, nrow)
        ntrue <- 10
        successes<-append(successes, ntrue)
        res <- binom.test(ntrue, nrow, p = prob)
        if (testname == "") testname = res$method
        pvals<-append(pvals, res$pvalue)
        confs<-append(confs, res$confidence)
    }
    
}
fisherTest<-function(data) {
    colnames <- colnames(data)
    cnum <- length(colnames) - 1
    if (cnum == 2) {
        mat <- matrix(rep(0, cnum * 2), nrow = 2)
        mat[1,1] = data[1, 2]
        mat[1,2] = data[1, 3]
        mat[2,1] = data[2, 2]
        mat[2,2] = data[2, 3]
        res <- fisher.test(mat)
        
    }
    else {
        loadLibraries(c("BiocManager"))
        loadBMLibraries(C("mixOmics"))
        loadLibraries(c("RVAideMemoire"))
        mat <- matrix(rep(0, cnum * 2), nrow = 2)
        for (i in 1:cnum) {
            mat[1,i] = data[1, i + 1]
            mat[2,i] = data[2, i + 1]
        }
        res <- fisher.multcomp(mat, p.method = correction)
        
    }
}
mcnemarTest<-function(data) {
    colnames <- colnames(data)
    cnum <- length(colnames) - 1
    mat <- matrix(rep(0, cnum * 2), nrow = 2)
    mat[1,1] = data[1, 2]
    mat[1,2] = data[1, 3]
    mat[2,1] = data[2, 2]
    mat[2,2] = data[2, 3]
    res <- mcnemar.test(mat)

}
normalityTest <- function(data, method) {
    if (method == "ad") {
        loadLibraries(c("nortest"))        
    }
    colnames <- colnames(data)
    cnum <- length(colnames)
    pvals <- c()
    for (f in c(1:cnum)) {
        dat <- data[,1]
        if (method == "ks") res = ks.test(dat, "pnorm", mean = mean(dat), sd = sd(dat))
        else if (method == "sw") res = shapiro.test(dat)
        else if (method == "ad") res = ad.test(dat)
        pvals <- append(pvals, res$p.value)
    }
    structure(
        list(
            method=res$method,
            array=list(
                title=factors,
                pvalue=pvals
            )
        )
    )
}
varianceTest<-function(data) {
    cnum<-ncol(rdata)
    if (cnum == 2) {
        res <- var.test(data[,1], data[,2])

    }
    else {
        loadLibraries(c("car"))
        data<-rearrangeDF(data, type="", nfactor=1)
        res <- bartlett.test(Value ~ Group, data = data)


    }
}
twoComp<-function(data, parametric=TRUE) {
    if(parametric) {

    }
    else {
        res = wilcox.test(data[,1], data[,2])

    }
}
kruskalTest<-function(data) {
    data<-rearrangeDF(data,nfactor=1)
    res <- kruskal.test(Value ~ Group, data = data)

}

mulComp<-function(data, method) {
    
    data <- rearrangeDF(data, nfactor=1)
    if (method == "dunnett") {

    }
    else if (method == "tukey") {

    }
    else if (method == "scheffe") {

        
# Test
nrows <- table(group)
gcount <- length(nrows)
total <- sum(nrows)
means <- c()
uvars <- c()
for (g in 1:gcount) {
    means <- append(means, mean(values[group==g]))
    uvars <- append(uvars, var(values[group==g]))
}
df <- total - gcount
Vw <- sum(uvars*(nrows-1))/df
ths <- c()
vths <- c()
confs <- c()
fvals <- c()
pvals <- c()
for (i in 1:gcount) {
    if (i == gcount) break
    k <- i + 1
    for (j in k:gcount) {
        g0 <- (1:gcount)[-c(i, j)]
        n0 <- gcount - 2
        weight <- rep(c(1, -1, 0), c(1, 1, n0))[order(c(c(i), c(j), g0))]
        theta <- sum(weight*means)
        ths <- append(ths, theta)
        Vtheta <- Vw*sum(weight^2/nrows)
        vths <- append(vths, Vtheta)
        confs <- append(confs, theta - c(1, -1) * sqrt((gcount-1)*qf(alpha, (gcount-1), df, lower.tail=FALSE)*Vtheta))
        F0 <- theta^2/(gcount-1)/Vtheta
        fvals <- append(fvals, F0)
        p <- pf(F0, (gcount-1), df, lower.tail=FALSE)
        pvals <- append(pvals, p)
    }
}


    }
    else if (method == "steel") {

    }
    else if (method == "steeldwass") {

    }
}


lrtest<-function(data) {


}

exportResult<-function(result, format="csv", file=""){
    resfile <- file(file)
    cat(result["title"], "\n", sep="", file = resfile, fill = TRUE)
    
    
    close(header)
    body <- file(path, open="a")
    names<-names[names!="method"]
    cat(paste(names, collapse=","),"\n",sep="", file = body)
    close(body)

    header <- file(path)
    cat(report["method"][[1]], "\n", sep="", file = header, fill = TRUE)
    close(header)
    body <- file(path, open="a")
    names<-names[names!="method"]
    cat(paste(names, collapse=","),"\n",sep="", file = body)
    close(body)
}

printResult <- function(result) {
  if (format == "raw") print(report)
  else if (display == "csv") {
    names<-names(report)
    cat(report$method[[1]],"\n")
    if (length(report$param)) {
      pkeys = names(report$param)
      for (p in 1:length(pkeys)) {
        cat(paste(pkeys[p], ": ", report$param[pkeys[p]][[1]]),"\n",sep="")
      }
    }
    if (length(report$array)) {
      cat(",",paste(report$array$title, collapse=","), "\n", sep="")
      akeys = names(report$array)
      for (a in 1:length(akeys)) {
        if (akeys[a] != "title") cat(akeys[a], ",", paste(report$array[akeys[a]][[1]], collapse=","),"\n",sep="")
      }
    }
    if (length(report$table)) {
      tkeys = names(report$table)
      nrow <- length(report$table[tkeys[0]])
      for (r in 1:nrow) {
        for (t in 1:length(tkeys)) {
          cat(paste(report$array[akeys[a]], report$array[akeys[a]][r],"\n",sep=""))
        }
      }
    }
  }
}