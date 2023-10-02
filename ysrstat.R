
rearrangeDF<-function(data, type="table", dir="V", nfactor=1) {
    if (type=="table") {
        if (nfactor == 1) {
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
fisherTest<-function(data, correction="") {
    colnames <- colnames(data)
    cnum <- length(colnames) - 1
    if (cnum == 2) {
        mat <- matrix(rep(0, cnum * 2), nrow = 2)
        mat[1,1] = data[1, 2]
        mat[1,2] = data[1, 3]
        mat[2,1] = data[2, 2]
        mat[2,2] = data[2, 3]
        res <- fisher.test(mat)
        
        str(res)
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
        
        str(res)
    }
}
mcnemarTest<-function(data) {
    factor1 <- colnames(data)[2:ncol(data)]
    factor2 <- data[,1]
    ret <- list(
        title = "",
        factor = list(
            '1' <- factor1,
            '2' <- factor2
        ),
        test = list(
            statistic = NA,
            pvalue = NA,
            df = NA
        )        
    )
    mat <- matrix(c(data[1, 2], data[1, 2], data[2, 2], data[2, 3]), nrow = 2, byrow=T)
    res <- mcnemar.test(mat)
    ret$title = res$method
    ret$test$statistic = unname(res$statistic)
    ret$test$pvalue = res$p.value
    ret$test$df = unname(res$parameter)
    return(structure(ret))
}
normalityTest <- function(data, method, alternative="two.sided") {
    colnames <- colnames(data)
    cnum <- length(colnames)
    if (method == "ad") {
        loadLibraries(c("nortest"))        
    }
    ret <- list(
        title = "",
        labels = colnames,
        test = list(
            count = rep(NA, cnum),
            statistic = rep(NA, cnum),
            pvalue = rep(NA, cnum)
        )        
    )
    for (f in c(1:cnum)) {
        dat <- data[!is.na(data[,f]),f]
        if (method == "ks") {
            ret$alternative = alternative
            res <- ks.test(dat, "pnorm", mean = mean(dat), sd = sd(dat), alternative = alternative)
        }
        else if (method == "sw") res <- shapiro.test(dat)
        else if (method == "ad") res <- ad.test(dat)
        ret$test$count[f] <- length(dat)
        ret$test$statistic[f] <- res$statistic
        ret$test$pvalue[f] <- res$p.value
        if (f == 1) ret$title <- res$method
    }
    return(structure(ret))
}


rearrange.df<-function(data) {
    colnames<-colnames(data)
    cnum<-length(colnames)
    value<-c()
    group<-c()
    for (g in c(1:cnum)) {
        dat <- data[!is.na(data[,g]),g]
        group <- c(group, rep(g, length(dat)))
        value <- append(value, dat)
    }                      
    df <- data.frame(value=value,group=group)
}

varianceTest<-function(data, alternative="two.sided", level=0.95) {
    rdata <- rearrange.df(data)
    ngourp <- length(unique(rdata$group))
    ret <- list(
        title = "",
        labels = colnames(data),
        test = list(
            statistic = NA,
            pvalue = NA
        )
    )
    if (ngourp == 2) {
        res <- var.test(value ~ group, data=rdata, alternative=alternative, conf.level=level)
        ret$alternative = alternative
        ret$conf.level = level
        ret$test$df = unname(res$parameter)
        attributes(res$conf.int) <- NULL
        ret$test$confidence = res$conf.int
    }
    else {
        loadLibraries(c("car"))
        res <- bartlett.test(value ~ group, data=rdata)
        ret$test$df = unname(res$parameter)
    }
    ret$title = res$method
    ret$test$statistic = unname(res$statistic)
    ret$test$pvalue = res$p.value
    return(structure(ret))
}
twoComp<-function(data, alternative=alternative, parametric=TRUE, var=TRUE, paired=FALSE) {
    cnames <- colnames(data)
    ret <- list(
        title = "",
        alternative=alternative,
        labels = c(cnames[1], cnames[2]),
        test = list(
            statistic = NA,
            pvalue = NA
        )
    )
    if(parametric) {
        if (paired) {
            res <- t.test(data[,1], data[,2], paired=paired)
        }
        else {
            res <- t.test(data[,1], data[,2], var=var)
        }
        attributes(res$estimate) <- NULL
        ret$test$means = res$estimate
        attributes(res$conf.int) <- NULL
        ret$test$confidence = res$conf.int        
        ret$test$df = unname(res$parameter)
        ret$test$stderr = unname(res$stderr)
    }
    else {
        res <- wilcox.test(data[,1], data[,2], paired=paired)
    }
    ret$title <- res$method
    ret$test$statistic <- unname(res$statistic)
    ret$test$pvalue <- res$p.value
    return(structure(ret))
}
aov1Test<-function(data,paired=False) {
    rdata <- rearrange.df(data)
    if (paired) {

    }
    else {

    }
}
kruskalTest<-function(data) {
    rdata <- rearrange.df(data)
    ngourp <- length(unique(rdata$group))
    ret <- list(
        title = "",
        labels = colnames(data),
        test = list(
            df = NA,
            statistic = NA,
            pvalue = NA
        )
    )
    res <- kruskal.test(value ~ group, data = rdata)
    ret$title = res$method
    ret$test$df = unname(res$parameter)
    ret$test$statistic = unname(res$statistic)
    ret$test$pvalue = unname(res$p.value)
    return(structure(ret))
}
friedmanTest<-function(data) {
    rdata <- rearrange.df(data)
    ngourp <- length(unique(rdata$group))
    ret <- list(
        title = "",
        labels = colnames(data),
        test = list(
            df = NA,
            statistic = NA,
            pvalue = NA
        )
    )
    res <- friedman.test(value ~ group, data = rdata)
    str(res)
    ret$title = res$method
    ret$test$df = unname(res$parameter)
    ret$test$statistic = unname(res$statistic)
    ret$test$pvalue = unname(res$p.value)
    return(structure(ret))
}
multiComp<-function(data, method, alternative="two.sided", level=0.95) {
    rdata <- rearrange.df(data)
    ret <- list(
        title = "",
        alternative=alternative,
        lvel=level,
        labels = colnames(data),
        test = list(
            compare = NA,
            statistic = NA,
            pvalue = NA
        )
    )
    if (method == "dunnett") {
        res = glht(aov(value ~ group, data=rdata), alternative=alternative, linfct=mcp(Group="Dunnett"))
        res <- confint(res, level=level)
        res <- summary(res)
        cnum <- ncol(data)
        ret$title <- "Dunnett's Test"
        ret$test$compare <- rep('', cnum-1)
        ret$test$sigma = rep(0, cnum-1)
        ret$test$coefficients = rep(0, cnum-1)
        ret$test$statistic <- rep(0, cnum-1)
        ret$test$pvalue <- rep(0, cnum-1)
        ret$test$confidence = rep(NA, cnum-1)
        for (i in 2:cnum) {
            ret$test$compare[i-1] <- cat("1:", i ,sep="")
            ret$test$sigma[i-1] <- res$test$sigma[i-1]
            ret$test$coefficients[i-1] <- res$test$coefficients[i-1]
            ret$test$statistic[i-1] <- res$test$tstat[i-1]
            ret$test$pvalue[i-1] <- res$test$pvalues[i-1]
            attributes(res$confint) <- NULL
            ret$test$confidence[i-1] <- res$confint
        }
    }
    else if (method == "steel") {
        cnum <- ncol(data)


        nrows <- table(rdata$group)
        gcount <- length(nrows)
        ctrl <- rdata$values[rdata$group == 1]
        clen <- length(ctrl)
        tvals <- numeric(gcount)
        pvals <- numeric(gcount)
        rho <- 0.5
        if (sum(nrows == clen) != gcount) {
            rho <- outer(nrows, nrows,
                        function(x, y) { sqrt(x/(x+nrows[1])*y/(y+nrows[1])) }
                        )
            diag(rho) <- 0
            rho <- sum(rho[-1, -1])/(gcount-2)/(gcount-1)
        }
        for (i in 2:gcount) {
            ret$test$compare[i] <- cat("1:",i,sep="")
            r <- rank(c(ctrl, rdata$values[rdata$group == i]))
            R <- sum(r[1:clen])
            N <- clen + nrows[i]
            E <- clen * (N + 1)/2
            V <- clen * nrows[i]/N/(N-1) * (sum(r^2) - N * (N+1)^2/4)
            tvals[i] <- abs(R - E)/sqrt(V)
            corr <- diag(gcount - 1)
            corr[lower.tri(corr)] <- rho
            pmvt.lower <- -Inf
            pmvt.upper <- Inf
            if (alternative == "less") {
                pmvt.lower <- -tvals[i]
                pmvt.upper <- Inf
            }
            else if (alternative == "greater") {
                pmvt.lower <- tvals[i]
                pmvt.upper <- Inf
            }
            else {
                tvals[i] <- abs(tvals[i])
                pmvt.lower <- -tvals[i]
                pmvt.upper <- tvals[i]
            }
            pvals[i] <- 1 - pmvt(lower = pmvt.lower, upper = pmvt.upper, delta = numeric(gcount - 1), df = 0, corr = corr, abseps = 0.0001)
        }
        ret$title <- "Steel's test"
        ret$test$compare <- rep('', cnum-1)
        ret$test$rho = rep(0, cnum-1)
        ret$test$coefficients = rep(0, cnum-1)
        ret$test$statistic <- rep(0, cnum-1)
        ret$test$pvalue <- rep(0, cnum-1)
    }
    else if (method == "tukey") {
        res = glht(aov(value ~ group, data=rdata), alternative=alternative, linfct=mcp(group="Tukey"))
        res <- confint(res, level=level)
        res <- summary(res)
        cnum <- ncol(data)
        num <- choose(n=ncol(data),k=2)
        ret$title <- "Tukey's test"
        ret$test$compare <- rep('', num)
        ret$test$sigma = rep(0, num)
        ret$test$coefficients = rep(0, num)
        ret$test$statistic <- rep(0, num)
        ret$test$pvalue <- rep(0, num)
        ret$test$confidence = rep(NA, num)
        count = 1
        for (i in 1:(cnum-1)) {
            for (j in (i+1):num) {
                ret$test$compare[count] = paste(factors[i], "vs", factors[2], sep="_")
                ret$test$sigma[count] = res$test$sigma[count][[1]]
                ret$test$coefficient[count] = res$test$coefficients[count][[1]]
                ret$test$statistic[count] = res$test$tstat[count][[1]]
                ret$test$confidence[count] = res$confint[count]
                ret$test$pvalue[count] = res$test$pvalues[count][[1]]
                count <- count+1
            }
        }
    }
    else if (method == "scheffe") {
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
            for (j in (i+1):gcount) {
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
}   

lrtest<-function(data) {


}

survivalTest<-function(data, mode) {
    loadLibraries(c("survival"))

    if (mode == "lrank") {
        
        
    }
    else if (mode == "gwilcox") {

    }
    
}


exportResult<-function(result, format="csv", file=""){
    resfile <- file(file)
    cat(result$title, "\n", sep="", file = resfile, fill = TRUE)
    
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