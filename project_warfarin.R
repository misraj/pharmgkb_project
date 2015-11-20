#Load all libraries

install.packages("RCurl")
install.packages("dplyr")
library(RCurl)
library(dplyr)



########~~~~Get and clean data~~~~~~#######

my.url <- getURL("https://raw.githubusercontent.com/misraj/pharmgkb_project/master/warfarin_data.txt") #Getting warfarin data
warfarin <- read.delim(text = my.url, header=TRUE)
missingness <- rep(0, 67)
for(i in 1:67){
  table <-table(is.na(warfarin[,i]))
  if(length(table) == 1){
    missingness[i] = table/5700
  }
  else{
    missingness[i] = table[1]/5700
  }
}
miss <- data.frame(names(warfarin),missingness)
#warfarin <- select(warfarin, )
#warfarin <- filter(warfarin, complete.cases(warfarin))
#During feature selection, 10% of data will be held aside until a model is chosen. Once the model is chose, held out data will be brough back into the training/test set. The model will then be trained on 75% of the dataset, and tested on the 25% of the dataset. 