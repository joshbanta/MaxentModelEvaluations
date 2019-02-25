##### Install Packages and Files #####

jdk.location <- list.files(path = "C:/Program Files/Java/", pattern = "^jdk.*", full.names = TRUE)[1]
if (is.na(jdk.location))
{
  stop("You must install a Java JDK.")
}

Sys.setenv(JAVA_HOME = jdk.location)
rm(jdk.location)

if (!require(rJava, quietly = TRUE)) {
  install.packages("rJava")
  require(rJava)
}

if (!require(dismo, quietly = TRUE))
{
  install.packages('dismo')
  
  if (!file.exists(paste0(system.file("java", package="dismo"),"/maxent.jar")))
  {
    file.copy(from = "maxent.jar",
              to = paste0(system.file("java", package="dismo"),"/maxent.jar"))
  }
  
  require(dismo)
}

if (!require(devtools, quietly = TRUE))
{
  install.packages('devtools')
  require(devtools)
}

if (!require(ENMeval, quietly = TRUE))
{
  install.packages('ENMeval')
  install_github("jscavetta95/ENMeval")
  require(ENMeval)
}


##### Functions #####

create.maxent.models <- function(predictors, occurence.points, outputFilename) 
{
  models <- ENMevaluate(occ = occurence.points, 
                        env = predictors, 
                        categoricals = 1, # Change this depending on which layers are categorical.
                        method = "block",
                        RMvalues = seq(0.5, 4, 0.5), 
                        fc = c("L","LQ","H","LQH","LQHP","LQHPT"),
                        algorithm = 'maxent.jar', 
                        parallel = TRUE)
  
  # Output model comparisons.
  write.csv(models@results, paste0(outputFilename,"results.csv"))
  
  # Save model performance figure.
  png(paste0(outputFilename,"auc_model_performance.png"))
  eval.plot(models@results, 'avg.test.AUC', var='var.test.AUC', legend.position = "bottomright")
  dev.off()
  
  png(paste0(outputFilename,"AICc_model_performance.png"))
  eval.plot(models@results, 'delta.AICc', legend.position = "bottomright")
  dev.off()
  
  png(paste0(outputFilename,"kappa_model_performance.png"))
  eval.plot(models@results, 'avg.test.kappa', var='var.test.kappa', legend.position = "bottomright")
  dev.off()
  
  # Save all model plots and variable importance plots.
  for(model_index in 1:ncol(models@predictions[]))
  {
    png(paste0(outputFilename,"_model_", models@predictions[[model_index]]@data@names[1], ".png"), width = 1000, height = 800)
    plot(models@predictions[[model_index]], labels=FALSE, tck=FALSE)
    dev.off()
    
    var.imp <- var.importance(models@models[[model_index]])
    
    png(paste0(outputFilename,"_variable_importance_", models@predictions[[model_index]]@data@names[1], ".png"))
    barplot(var.imp$permutation.importance, names.arg = var.imp$variable, las = 2, ylab = "Permutation Importance")
    dev.off()
  }
}


##### Script #####

if(!dir.exists("./models")) dir.create("./models")
predictors <- stack(list.files("./layers", pattern = '\\.asc$', full.names = TRUE))
for(species in list.files("./occurences", full.names = TRUE)) 
{
  occurence.points <- read.csv(species)[,-1]
  
  outputFilename <- gsub("occurences", "models", species)
  outputFilename <- gsub("\\.csv", "/", outputFilename)
  if(!dir.exists(outputFilename)) dir.create(outputFilename)
  
  create.maxent.models(predictors, occurence.points, outputFilename)
}