


## install.packages("remotes")
# remotes::install_gitlab("dickoa/robotoolbox")

## installing dependent packages using the function below
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

## After installing the relevant packages,...
## The first step is to get your API token from your account

library(robotoolbox)

token3 <- "07cdf3134d0f4352b54cf83edaa59101bc02990e"


## With the token (that you can also get from your account) 
# You can now setup the robotoolbox package.

kobo_setup(url = "https://kf.kobotoolbox.org/", token = token3)
kobo_settings() ## just to check
## <robotoolbox settings>
##    KoBoToolbox URL: https://{kpi-url}/
##    KoBoToolbox API Token: xxxxxxxxxxxxxxxxxxxxxxxxxx

# Now you can access all your projects and read the data from the project you picked. 
# You can list project using kobo_asset_list().

library(tidyverse)
l <- kobo_asset_list() 
glimpse(l) # l is a data.frame with as many rows as projects


# Let’s pick the second project and load it

uid <- l$uid[2]
asset <- kobo_asset(uid)
asset


# With the asset you can now read your data
data <- kobo_data(asset)
glimpse(data)
