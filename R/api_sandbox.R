


## install.packages("remotes")
remotes::install_gitlab("dickoa/robotoolbox")

## installing dependent packages
get.packages <- function(packages, Base_R_Best_R = F){
  if(Base_R_Best_R){
    print("No packages required!")
  }
  else{
    for(i in seq.int(length(packages))){
      if(!require(packages[i], character.only = T)){
        install.packages(packages[i])
      }
      library(packages[i], character.only = T)
    }
  }
}
#example
#get.packages(c("crul", "RcppSimdJson", "dm","labelled"))
