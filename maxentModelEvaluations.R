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
  install_github("jscavetta95/ENMeval") # This is a forked version of ENMeval, it is required for MaxKappa.
  require(ENMeval)
}

if (!require(MASS, quietly = TRUE))
{
  install.packages('MASS')
  require(MASS)
}


##### Functions #####

create.bias.file <- function(predictors, occurence.points, output.filename)
{
  occurence.raster <- rasterize(occurence.points, predictors, 1)
  occurence.coordinates <- coordinates(occurence.raster)[which(values(occurence.raster) == 1), ]
  
  kernel.density <- kde2d(occurence.coordinates[,1], occurence.coordinates[,2], 
                          n = c(nrow(occurence.raster), 
                                ncol(occurence.raster)), 
                          lims = c(extent(predictors)@xmin, 
                                   extent(predictors)@xmax, 
                                   extent(predictors)@ymin, 
                                   extent(predictors)@ymax))
  
  bias.density <- resample(raster(kernel.density, predictors), predictors)
  
  writeRaster(bias.density, paste0(output.filename,"biasfile.asc"), overwrite = TRUE)
  
  background.points <- xyFromCell(bias.density, 
                                  sample(which(!is.na(values(predictors[[1]]))), 
                                         ifelse(sum(values(bias.density)>0) >= 10000, 10000, sum(values(bias.density)>0)), 
                                         prob=values(bias.density)[!is.na(values(predictors[[1]]))]))

  write.csv(background.points, paste0(output.filename,"background_points.csv"))
  
  return(background.points)
}

create.maxent.models <- function(predictors, occurence.points, background.points, output.filename) 
{
  models <- ENMevaluate(occ = occurence.points, 
                        env = predictors, 
                        categoricals = c(1), # Change this depending on which layers are categorical.
                        bg.coords = background.points,
                        method = "block",
                        RMvalues = c(1,1.5,2,2.5), 
                        fc = c("LQ","LQH","LQHPT"),
                        algorithm = 'maxent.jar', 
                        parallel = TRUE)
  
  # Output model comparisons.
  write.csv(models@results, paste0(output.filename,"results.csv"))
  
  # Save model performance figure.
  png(paste0(output.filename,"auc_model_performance.png"))
  eval.plot(models@results, 'avg.test.AUC', var='var.test.AUC', legend.position = "bottomright")
  dev.off()
  
  png(paste0(output.filename,"AICc_model_performance.png"))
  eval.plot(models@results, 'avg.test.orMTP', var='var.test.orMTP', legend.position = "bottomright")
  dev.off()
  
  png(paste0(output.filename,"kappa_model_performance.png"))
  eval.plot(models@results, 'avg.test.kappa', var='var.test.kappa', legend.position = "bottomright")
  dev.off()
  
  png(paste0(output.filename,"AICc_model_performance.png"))
  eval.plot(models@results, 'delta.AICc', legend.position = "bottomright")
  dev.off()
  
  # Save all model plots and variable importance plots.
  for(model_index in 1:ncol(models@predictions[]))
  {
    png(paste0(output.filename,"model_", models@predictions[[model_index]]@data@names[1], ".png"), width = 1000, height = 800)
    
    #prediction.cloglog <- predict(models@models[[model_index]], predictors, args=c("outputformat=cloglog"))
    #plot(prediction.cloglog, labels=FALSE, tck=FALSE)
    
    plot(models@predictions[[model_index]], labels=FALSE, tck=FALSE)
    dev.off()
    
    var.imp <- var.importance(models@models[[model_index]])
    
    png(paste0(output.filename,"variable_importance_", models@predictions[[model_index]]@data@names[1], ".png"))
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
  
  output.filename <- gsub("occurences", "models", species)
  output.filename <- gsub("\\.csv", "/", output.filename)
  if(!dir.exists(output.filename)) dir.create(output.filename)
  
  background.points <- create.bias.file(predictors, occurence.points, output.filename)
  
  create.maxent.models(predictors, occurence.points, background.points, output.filename)
}
