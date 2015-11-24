#Load all libraries

install.packages("RCurl")
install.packages("dplyr")
library(RCurl)
library(dplyr)
library(devtools)
library(ggplot2)
install_github("ggbiplot", "vqv")


########~~~~Get and clean data~~~~~~#######

my.url <- getURL("https://raw.githubusercontent.com/misraj/pharmgkb_project/master/warfarin_data.txt") #Getting warfarin data
warfarin <- read.delim(text = my.url, header=TRUE)
missingness_feature <- rep(0, 23)
for(i in 1:23){
  table <-table(is.na(warfarin[,i]))
  if(length(table) == 1){
    missingness_feature[i] = table/1738
  }
  else{
    missingness_feature[i] = table[1]/1738
  }
}
miss_feature<- data.frame(names(warfarin),missingness_feature)
sorted_miss_feature<- arrange(miss_feature, desc(missingness_feature))

missingness_subjects <- rep(0,5700)
for(i in 1:5700){
  table <- table(is.na(warfarin[i,]))
  if(length(table) == 1){
    missingness_subjects[i] = table/63
  }
  else{
    missingness_subjects[i] = table[1]/63
  }
}
miss_subject <- data.frame(rownames(warfarin),missingness_subjects)
sorted_miss_subject <- arrange(miss_subject, desc(missingness_subjects))

#Filter data accordingly

warfarin <- filter(warfarin, complete.cases(Therapeutic.Dose))
warfarin <- filter(warfarin, complete.cases(Gender)) #
warfarin <- filter(warfarin, complete.cases(Age))
warfarin <- filter(warfarin, complete.cases(Reached.Stable.Dose))
warfarin <- filter(warfarin, complete.cases(Cyp2C9))

warfarin <- filter(warfarin, complete.cases(Weight))
warfarin <- filter(warfarin, complete.cases(Race.Reported)) #
warfarin <- filter(warfarin, complete.cases(Indication))

warfarin <- filter(warfarin, complete.cases(INR.Reported))
warfarin <- filter(warfarin, complete.cases(Height))
warfarin <- filter(warfarin, complete.cases(VKORC1.1))

warfarin <- filter(warfarin, complete.cases(Heart.probs))
warfarin <- filter(warfarin, complete.cases(Valve))
warfarin <- filter(warfarin, complete.cases(Simvastatin))
warfarin <- filter(warfarin, complete.cases(Aspirin))

warfarin <-select(warfarin, Subject.ID,Sample.ID, Site, Gender, Race.Reported, Race.OMB, Genotyped.QC2.Cyp2C9, Genotyped.QC3.Cyp2C9, Combined.QC4.CYP2C9,VKORC1.1_QC, Age, CYP2C9_consensus, Weight, Indication, Heart.probs, Valve, Aspirin,Simvastatin,Cyp2C9,VKORC1.1,INR.Reported,Reached.Stable.Dose, Therapeutic.Dose)
###Change values to factors#######
warfarin <- mutate(warfarin, Diabetes = factor(Diabetes, levels=c(0, 1), labels=c("yes", "no")))
warfarin <- mutate(warfarin, Herbal.Medications.Vitamins.Supps = factor(Herbal.Medications.Vitamins.Supps, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Anti.fungal.Azoles = factor(Anti.fungal.Azoles, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Macrolide.Antibiotics = factor(Macrolide.Antibiotics, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Sulfonamide.Antibiotics = factor(Sulfonamide.Antibiotics, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Rifam = factor(Rifam, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Phenytoin= factor(Phenytoin, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Carbamazepine = factor(Carbamazepine, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Amiodarone = factor(Amiodarone, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Cerivastatin = factor(Cerivastatin, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Rosuvastatin = factor(Rosuvastatin, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Pravastatin= factor(Pravastatin, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Lovastatin = factor(Lovastatin, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Fluvastatin = factor(Fluvastatin, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Atorvastatin = factor(Atorvastatin, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Simvastatin = factor(Simvastatin, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, High.Dose.Ty = factor(High.Dose.Ty, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Tylenol = factor(Tylenol, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Aspirin = factor(Aspirin, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Valve = factor(Valve, levels=c(0, 1), labels=c("no", "yes")))
warfarin <- mutate(warfarin, Heart.probs = factor(Heart.probs, levels=c(0, 1), labels=c("no", "yes")))


#SPLIT THE DATA BY



barplot(table(warfarin$Gender))
barplot(table(warfarin$Race.Reported))
barplot(table(warfarin$Site))
barplot(table(warfarin$Age))
hist(table(warfarin$Height))
hist(table(warfarin$Weight))

statins <-data.frame(table(warfarin$Cerivastatin)[2], table(warfarin$Atorvastatin)[2], table(warfarin$Simvastatin)[2],table(warfarin$Fluvastatin)[2], table(warfarin$Rosuvastatin)[2],table(warfarin$Lovastatin)[2])
colnames(statins) <- cbind("Cerivstatin","Atorvastatin", "Simvastatin", "Fluvastatin", "Rosuvastatin", "Lovastatin" )



#Perform Linear Regression

variables <- names(warfarin)
for( i in 3:23){
  datas.glm <- glm(warfarin$Therapeutic.Dose~ warfarin[,i:i], data=warfarin, family= gaussian)
  pv <-summary(datas.glm)$coefficients[2,4]
  #variables[i,2] = pv
  if(pv <= 0.05){
    print(variables[i])
    print(pv)
  }
}
