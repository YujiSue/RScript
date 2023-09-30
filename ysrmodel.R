embedData <- function(data, mode, seed) {
    if (mode == "pca") {
        loadLibararies(c("stats"))
        
        model <- prcomp(iris_standardized)
        summary(model)
    }
    else if (mode == "tsne") {
        loadLibararies(c("Rtsne"))
        set.seed(seed)
        model <- Rtsne(iris_numeric)
        summary(model)
    }
    else if (mode == "umap") {
        loadLibararies(c("umap"))
        model <- umap(iris_numeric)
        summary(model)
    }
    return(model)
}
lmFit <- function(data) {
    model <- lm(data[,1] ~ data[,0])

    summary(model)
}
quantFit <- function(data) {
    installLibraries(c("quantreg"))
    tvalus <- seq(0.25, 0.75, by = 0.25)
    models <- c()
    for (t in tvalues) {
        models <- append(models, rq(Response ~ Predictor, data = data, tau = t)
    }
    
}
lmmFit <- function(data) {
    installLibraries(c("lme4"))
    model <- lmer(Response ~ Predictor + (1 | Subject), data = data)


    summary(model)
}
glmmFit <- function(data, mode) {
    installLibraries(c("lme4"))
    model <- glmer(Response ~ Predictor + (1 | Subject), data = data, family = binomial)

    summary(model)

}
glmFit<-function(data, mode) {

    if (mode == "logistic") {
        model <- glm(Species_binary ~ Sepal.Length + Sepal.Width, data = iris, family = binomial)
    }
    else if (mode == "poisson") {
        model <- glm(vs ~ hp + wt, data = mtcars, family = poisson)

    }
    summary(model)
}
gamFit<-function(data) {
    installLibraries(c("mgcv"))

    model <- gam(Response ~ s(Predictor), data = data)
    
    summary(model)
}

hBayesFit <- function(data) {

}

coxFit<-function(data) {

}

ridgeFit<-function() {}
lassoFit<-function() {}
erasticFit<-function() {}


arFit <- function(data, ma=TRUE) {

}
svmFit <- function(data) {

}
