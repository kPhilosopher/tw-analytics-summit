# Rattle is Copyright (c) 2006-2012 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2013-07-18 20:02:08 x86_64-apple-darwin11.2.0 

# Rattle version 2.6.20 user 'davej'

# Export this log textview to a file using the Export button or the Tools 
# menu to save a log of all activity. This facilitates repeatability. Exporting 
# to file 'myrf01.R', for example, allows us to the type in the R Console 
# the command source('myrf01.R') to repeat the process automatically. 
# Generally, we may want to edit the file to suit our needs. We can also directly 
# edit this current log textview to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

library(rattle)

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building

# The colorspace package is used to generate the colours used in plots, if available.

library(colorspace)

# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2013-07-18 20:02:58 x86_64-apple-darwin11.2.0 

# Load the data.

crs$dataset <- read.csv("file:///Users/davej/TW/tw-analytics-summit/data/astro/sdss2.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2013-07-18 20:03:04 x86_64-apple-darwin11.2.0 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 59560 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 41692 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 8934 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 8934 observations

# The following variable selections have been noted.

crs$input <- c("ra", "dec", "u", "g",
     "r", "i", "z", "redshift",
     "ug", "gr", "ri", "iz")

crs$numeric <- c("ra", "dec", "u", "g",
     "r", "i", "z", "redshift",
     "ug", "gr", "ri", "iz")

crs$categoric <- NULL

crs$target  <- "objtype"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-07-18 20:03:33 x86_64-apple-darwin11.2.0 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 59560 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 41692 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 8934 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 8934 observations

# The following variable selections have been noted.

crs$input <- c("ug", "gr", "ri", "iz")

crs$numeric <- c("ug", "gr", "ri", "iz")

crs$categoric <- NULL

crs$target  <- "redshift"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("ra", "dec", "u", "g", "r", "i", "z", "objtype")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-07-18 20:04:12 x86_64-apple-darwin11.2.0 

# Transform variables by rescaling. 

# The 'reshape' package provides the 'rescaler' function.

require(reshape, quietly=TRUE)

# Rescale redshift.

crs$dataset[["R01_redshift"]] <- crs$dataset[["redshift"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_redshift"]] <-  rescaler(crs$dataset[["redshift"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_redshift"]] <- (crs$dataset[["redshift"]] - -0.002484)/abs(4.747850 - -0.002484)
}

#============================================================
# Rattle timestamp: 2013-07-18 20:04:13 x86_64-apple-darwin11.2.0 

# Note the user selections. 

# The following variable selections have been noted.

crs$input <- c("ug", "gr", "ri", "iz")

crs$numeric <- c("ug", "gr", "ri", "iz")

crs$categoric <- NULL

crs$target  <- "R01_redshift"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("ra", "dec", "u", "g", "r", "i", "z", "redshift", "objtype")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-07-18 20:04:26 x86_64-apple-darwin11.2.0 

# Transform variables by rescaling. 

# The 'reshape' package provides the 'rescaler' function.

require(reshape, quietly=TRUE)

# Rescale redshift.

crs$dataset[["R01_redshift"]] <- crs$dataset[["redshift"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_redshift"]] <-  rescaler(crs$dataset[["redshift"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_redshift"]] <- (crs$dataset[["redshift"]] - -0.002484)/abs(4.747850 - -0.002484)
}

#============================================================
# Rattle timestamp: 2013-07-18 20:04:27 x86_64-apple-darwin11.2.0 

# Note the user selections. 

# The following variable selections have been noted.

crs$input <- c("ug", "gr", "ri", "iz")

crs$numeric <- c("ug", "gr", "ri", "iz")

crs$categoric <- NULL

crs$target  <- "R01_redshift"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("ra", "dec", "u", "g", "r", "i", "z", "redshift", "objtype")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-07-18 20:04:55 x86_64-apple-darwin11.2.0 

# Transform variables by rescaling. 

# The 'reshape' package provides the 'rescaler' function.

require(reshape, quietly=TRUE)

# Rescale ug.

crs$dataset[["R01_ug"]] <- crs$dataset[["ug"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_ug"]] <-  rescaler(crs$dataset[["ug"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_ug"]] <- (crs$dataset[["ug"]] - -2.805956)/abs(12.298345 - -2.805956)
}

# Rescale gr.

crs$dataset[["R01_gr"]] <- crs$dataset[["gr"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_gr"]] <-  rescaler(crs$dataset[["gr"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_gr"]] <- (crs$dataset[["gr"]] - -11.148813)/abs(2.800526 - -11.148813)
}

# Rescale ri.

crs$dataset[["R01_ri"]] <- crs$dataset[["ri"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_ri"]] <-  rescaler(crs$dataset[["ri"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_ri"]] <- (crs$dataset[["ri"]] - -12.469640)/abs(9.665233 - -12.469640)
}

# Rescale iz.

crs$dataset[["R01_iz"]] <- crs$dataset[["iz"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_iz"]] <-  rescaler(crs$dataset[["iz"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_iz"]] <- (crs$dataset[["iz"]] - -13.246935)/abs(10015.211578 - -13.246935)
}

#============================================================
# Rattle timestamp: 2013-07-18 20:04:56 x86_64-apple-darwin11.2.0 

# Note the user selections. 

# The following variable selections have been noted.

crs$input <- c("R01_ug", "R01_gr", "R01_ri", "R01_iz")

crs$numeric <- c("R01_ug", "R01_gr", "R01_ri", "R01_iz")

crs$categoric <- NULL

crs$target  <- "R01_redshift"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("ra", "dec", "u", "g", "r", "i", "z", "redshift", "objtype", "ug", "gr", "ri", "iz")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-07-18 20:05:11 x86_64-apple-darwin11.2.0 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 59560 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 41692 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 8934 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 8934 observations

# The following variable selections have been noted.

crs$input <- c("R01_ug", "R01_gr", "R01_ri", "R01_iz")

crs$numeric <- c("R01_ug", "R01_gr", "R01_ri", "R01_iz")

crs$categoric <- NULL

crs$target  <- "R01_redshift"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("ra", "dec", "u", "g", "r", "i", "z", "redshift", "objtype", "ug", "gr", "ri", "iz")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-07-18 20:05:16 x86_64-apple-darwin11.2.0 

# Neural Network 

# Build a neural network model using the nnet package.

require(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(R01_redshift ~ .,
    data=crs$dataset[crs$sample,c(crs$input, crs$target)],
    size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print.summary.nnet.rattle(summary(crs$nnet))
cat('\n')

# Time taken: 5.23 secs

#============================================================
# Rattle timestamp: 2013-07-18 20:05:27 x86_64-apple-darwin11.2.0 

# Evaluate model performance. 

# NNET: Generate a Predicted v Observed plot for nnet model on sdss2.csv [validate].

crs$pr <- predict(crs$nnet, crs$dataset[crs$validate, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$validate,], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(R01_redshift=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2]), digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="R01_redshift", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Neural Net Model
 sdss2.csv [validate]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2013-07-18 20:06:19 x86_64-apple-darwin11.2.0 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 59560 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 41692 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 8934 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 8934 observations

# The following variable selections have been noted.

crs$input <- c("ug", "gr", "ri", "iz")

crs$numeric <- c("ug", "gr", "ri", "iz")

crs$categoric <- NULL

crs$target  <- "objtype"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("ra", "dec", "u", "g", "r", "i", "z", "redshift", "R01_redshift", "R01_ug", "R01_gr", "R01_ri", "R01_iz")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-07-18 20:06:35 x86_64-apple-darwin11.2.0 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

require(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(objtype) ~ .,
      data=crs$dataset[crs$train,c(crs$input, crs$target)],
      kernel="rbfdot",
      prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 53.31 secs

#============================================================
# Rattle timestamp: 2013-07-18 20:07:35 x86_64-apple-darwin11.2.0 

# Evaluate model performance. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- predict(crs$ksvm, na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

table(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$objtype, crs$pr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing percentages.

round(100*table(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$objtype, crs$pr, 
        dnn=c("Actual", "Predicted"))/length(crs$pr))

#============================================================
# Rattle timestamp: 2013-07-18 20:07:56 x86_64-apple-darwin11.2.0 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 59560 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.5*crs$nobs) # 29780 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.25*crs$nobs) # 14890 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 14890 observations

# The following variable selections have been noted.

crs$input <- c("ug", "gr", "ri", "iz")

crs$numeric <- c("ug", "gr", "ri", "iz")

crs$categoric <- NULL

crs$target  <- "objtype"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("ra", "dec", "u", "g", "r", "i", "z", "redshift", "R01_redshift", "R01_ug", "R01_gr", "R01_ri", "R01_iz")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-07-18 20:08:00 x86_64-apple-darwin11.2.0 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 59560 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.5*crs$nobs) # 29780 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.25*crs$nobs) # 14890 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 14890 observations

# The following variable selections have been noted.

crs$input <- c("ug", "gr", "ri", "iz")

crs$numeric <- c("ug", "gr", "ri", "iz")

crs$categoric <- NULL

crs$target  <- "objtype"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("ra", "dec", "u", "g", "r", "i", "z", "redshift", "R01_redshift", "R01_ug", "R01_gr", "R01_ri", "R01_iz")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-07-18 20:08:15 x86_64-apple-darwin11.2.0 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

require(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(objtype) ~ .,
      data=crs$dataset[crs$train,c(crs$input, crs$target)],
      kernel="polydot",
      kpar=list("degree"=3),
      prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 55.46 mins

#============================================================
# Rattle timestamp: 2013-07-18 21:43:06 x86_64-apple-darwin11.2.0 

# Evaluate model performance. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- predict(crs$ksvm, na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

table(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$objtype, crs$pr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing percentages.

round(100*table(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$objtype, crs$pr, 
        dnn=c("Actual", "Predicted"))/length(crs$pr))
