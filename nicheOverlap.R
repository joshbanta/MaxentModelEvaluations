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


##### Functions #####

subsample.niche.overlap <- function(species1.occurence, species2.occurence, predictors, factors=NULL, args=NULL, iterations)
{
  progress.bar <- txtProgressBar(max = iterations*2)
  
  occurence.points <- rbind(species1.occurence, species2.occurence)
  scores <- lapply(1:iterations, function(i) 
  {
    rows <- sample(nrow(occurence.points), size = nrow(species1.occurence), replace = FALSE)
    
    pseudo1.occurence <- occurence.points[rows,]
    pseudo2.occurence <- occurence.points[-rows,]
    
    pseudo1.prediction <- predict(maxent(predictors, pseudo1.occurence, factors=factors, args=args), predictors)
    progress.bar$up(progress.bar$getVal() + 1)
    
    pseudo2.prediction <- predict(maxent(predictors, pseudo2.occurence, factors=factors, args=args), predictors)
    progress.bar$up(progress.bar$getVal() + 1)
    
    return(nicheOverlap(pseudo1.prediction, pseudo2.prediction))
  })
  
  close(progress.bar)
  return(scores)
}


##### Script #####

# Parameters to change.
factors <- c(1)
args <- c("betamultiplier=1",                        # Regularization multiplier.
          "linear=true",                             # Allow linear features.
          "quadratic=true",                          # Allow quadratic features.
          "product=false",                           # Allow product features.
          "threshold=false",                         # Allow threshold features.
          "hinge=false",                             # Allow hinge features.
          "pictures=false",                          # Generate pictures for output.
          paste0("threads=",parallel::detectCores()) # Number of cores for parallel processing. 
        )
iterations <- 3

species.predictions <- list()
predictors <- stack(list.files("./layers", pattern = '\\.asc$', full.names = TRUE))
species.comparisons <- combn(list.files("./occurences", full.names = TRUE), 2)
for(j in 1:ncol(species.comparisons))
{
  cat(species.comparisons[1,j], "vs", species.comparisons[2,j])
  
  species1.occurence <- read.csv(species.comparisons[1,j])[,-1]
  species2.occurence <- read.csv(species.comparisons[2,j])[,-1]
  
  model1.prediction <- species.predictions[[species.comparisons[1,j]]]
  if(is.null(model1.prediction))
  {
    model1.prediction <- predict(maxent(predictors, species1.occurence, factors=factors, args=args), predictors)
    species.predictions[species.comparisons[1,j]] <- model1.prediction
  }
  
  model2.prediction <- species.predictions[[species.comparisons[2,j]]]
  if(is.null(model2.prediction))
  {
    model2.prediction <- predict(maxent(predictors, species2.occurence, factors=factors, args=args), predictors)
    species.predictions[species.comparisons[2,j]] <- model2.prediction
    
  }
  
  real.score <- nicheOverlap(model1.prediction, model2.prediction)
  
  pseudo.scores <- subsample.niche.overlap(species1.occurence, species2.occurence, predictors, factors, args, iterations)
  p.value <- length(pseudo.scores[which(pseudo.scores <= real.score),]) / 100
  p.value
}