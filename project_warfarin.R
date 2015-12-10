#Load all libraries

#install.packages("RCurl")
#install.packages("dplyr")
library(RCurl)
library(dplyr)
library(devtools)
library(ggplot2)
install_github("ggbiplot", "vqv")
library(randomForest)
library(pROC)
#library(MASS)


########~~~~Get and clean data~~~~~~#######

my.url <- getURL("https://raw.githubusercontent.com/misraj/pharmgkb_project/master/warfarin_data.txt") #Getting warfarin data
warfarin <- read.delim(text = my.url, header=TRUE)

#Filter out observations that did not have a thereapeutic dosage data available
warfarin <- filter(warfarin, complete.cases(Therapeutic.Dose))

#Plot dosage data
plot(seq(1:5527), warfarin$Therapeutic.Dose, xlab  = "Subject", ylab = "Therapeutic Warfarin Dosage")

#Remove outliers

warfarin_site <- filter(warfarin, complete.cases(Site))
site_reg <- lm( Therapeutic.Dose ~ Site , data = warfarin_site)
summary(site_reg) #<2e-16

warfarin_gender <- filter(warfarin, complete.cases(Gender))
gender_reg <- lm( Therapeutic.Dose ~ Gender , data = warfarin_gender)
summary(gender_reg) #1.78e-06

warfarin<-dplyr::select(warfarin,-(Cerivastatin))

uni_reg <- data.frame(seq(1:60),seq(1:60))
colnames(uni_reg) <- c("Variable", "Pval")
for( i in 3:62){
  w <- filter(warfarin, complete.cases(names(warfarin)[i]))
  datas.glm <- lm(Therapeutic.Dose~ warfarin[,i:i], data=warfarin)
  pv <-summary(datas.glm)$coefficients[2,4]
  uni_reg[i-2,1] <-names(warfarin)[i]
  uni_reg[i-2,2] <-pv
  i
}

sort_uni_reg <- arrange(uni_reg,Pval)



#####################Looking at missingness rate############################
missingness_feature <- rep(0, 63)
for(i in 1:63){
  table <-table(is.na(warfarin[,i]))
  if(length(table) == 1){
    missingness_feature[i] = table/5699
  }
  else{
    missingness_feature[i] = table[1]/5699
  }
}
miss_feature<- data.frame(names(warfarin),missingness_feature)
sorted_miss_feature<- arrange(miss_feature, desc(missingness_feature))

missingness_subjects <- rep(0,5699)
for(i in 1:5699){
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
############################################################################

##Power estimate##
#1.5 or 2
#In order to detect this association how many samples do i need?
#This is the number  of smaples i need to explain idfference in therapeutic dosage.

#Mutate 
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

#Filter data accordingly

warfarin <- filter(warfarin, complete.cases(Gender)) #
warfarin <- filter(warfarin, complete.cases(Age))
warfarin <- filter(warfarin, complete.cases(Reached.Stable.Dose))
warfarin <- filter(warfarin, complete.cases(Cyp2C9))

warfarin <- filter(warfarin, complete.cases(Weight))
warfarin <- filter(warfarin, complete.cases(Height))

warfarin <- filter(warfarin, complete.cases(Race.Reported)) #
warfarin <- filter(warfarin, complete.cases(Indication))

warfarin <- filter(warfarin, complete.cases(INR.Reported))
warfarin <- filter(warfarin, complete.cases(Height))
warfarin <- filter(warfarin, complete.cases(VKORC1.1))

warfarin <- filter(warfarin, complete.cases(Heart.probs))
warfarin <- filter(warfarin, complete.cases(Valve))
warfarin <- filter(warfarin, complete.cases(Simvastatin))
warfarin <- filter(warfarin, complete.cases(Aspirin))

bmi <- warfarin2$Weight/ (warfarin2$Height*warfarin2$Height)
for(i in 1:length(warfarin2$Weight)){
  h <- warfarin2$Height[i]/100
  den <- h *h
  bmi[i] <- warfarin2$Weight[i]/ den
  if(bmi[i] < 18.5){
    bmi[i] <- 1
  }
  else if(bmi[i] >=18.5 && bmi[i] <24.9 ){
    bmi[i] <- 2
  }
  else if(bmi[i] >= 25.0 && bmi[i] < 29.9 ){
    bmi[i] <- 3
  }
  else if(bmi[i] >=30.0 && bmi[i] <34.9 ){
    bmi[i] <- 4
  }
  else if(bmi[i] >=35.0 && bmi[i] <39.9 ){
    bmi[i] <- 5
  }
  else{
    bmi[i] <- 6
  }
  
}
hist(bmi)
warfarin <- cbind(warfarin, bmi)
warfarin <- mutate(warfarin, bmi = factor(bmi, levels=c(1, 2,3,4,5,6), labels=c("underweight", "normal", "overweight", "obesity1", "obesity2", "obesity3")))

clean_warfarin <- dplyr::select(warfarin, Subject.ID, Sample.ID, Site, Gender, Race.Reported, Race.OMB, Weight, Height,bmi, Age, Indication, Heart.probs, Valve, Aspirin, Simvastatin, Genotyped.QC2.Cyp2C9,Genotyped.QC3.Cyp2C9, Combined.QC4.CYP2C9, Cyp2C9, VKORC1.1,VKORC1.1_QC, INR.Reported,Reached.Stable.Dose)

# Split data into train (2/3) and test (1/3) sets
n = 1684
train_rows <- sample(1:n, .66*n)
warf.train <- as.matrix(clean_warfarin[train_rows, ])
warf.test <- as.matrix(clean_warfarin[-train_rows, ])

dose.train <- warfarin$Therapeutic.Dose[train_rows]
dose.test <- warfarin$Therapeutic.Dose[-train_rows]

fit.lasso <- glmnet(warf.train, dose.train, family="gaussian", alpha=1)

missingness_feature <- rep(0, 23)
for(i in 1:23){
  table <-table(is.na(warfarin[,i]))
  if(length(table) == 1){
    missingness_feature[i] = table/1684
  }
  else{
    missingness_feature[i] = table[1]/1684
  }
}
miss_feature<- data.frame(names(warfarin),missingness_feature)
sorted_miss_feature<- arrange(miss_feature, desc(missingness_feature))
#SPLIT THE DATA BY
ggplot(warfarin, aes(x = Gender)) + geom_bar(stat = "bin")
ggplot(warfarin, aes(x = Race.Reported)) + geom_bar(stat = "bin")
ggplot(warfarin, aes(x = Race.OMB)) + geom_bar(stat = "bin")
ggplot(warfarin, aes(x = Site)) + geom_bar(stat = "bin")
ggplot(warfarin, aes(x = Age)) + geom_bar(stat = "bin")
ggplot(warfarin, aes(x = bmi)) + geom_bar(stat = "bin")

ggplot(warfarin, aes(x = factor(Reached.Stable.Dose))) + geom_bar(stat = "bin")
table(warfarin$Reached.Stable.Dose)
qplot(Height, data=warfarin, geom="histogram")
qplot(Weight, data=warfarin, geom="histogram")
statins <-data.frame(table(warfarin$Cerivastatin)[2], table(warfarin$Atorvastatin)[2], table(warfarin$Simvastatin)[2],table(warfarin$Fluvastatin)[2], table(warfarin$Rosuvastatin)[2],table(warfarin$Lovastatin)[2])
colnames(statins) <- cbind("Cerivstatin","Atorvastatin", "Simvastatin", "Fluvastatin", "Rosuvastatin", "Lovastatin" )



#Perform Linear Regression

variables <- names(warfarin)
for( i in 3:25){
  datas.glm <- glm(warfarin$Therapeutic.Dose~ warfarin[,i:i], data=warfarin, family= gaussian)
  pv <-summary(datas.glm)$coefficients[2,4]
  #variables[i,2] = pv
  if(pv <= 0.05){
    print(variables[i])
    print(pv)
  }
}


reg <- glm(warfarin$Therapeutic.Dose~ warfarin$Site + warfarin$Race.OMB + warfarin$Weight + warfarin$Height + warfarin$Valve +warfarin$Aspirin + warfarin$Simvastatin + warfarin$Combined.QC4.CYP2C9 + warfarin$VKORC1.1 + warfarin$VKORC1.1_QC, data=warfarin, family= gaussian)
summary(reg)
best.reg <- predict.lm(reg,warfarin, type = "response")
#best.reg <- data.frame(best.reg)
#Err = sum(abs(warfarin$Therapeutic.Dose - best.reg))
#Err/1684
##Evaluation### Complexity vs Fit
#table
#Stepwise, slowly get rid of variables


#ELASTIC NET REGRESSION
#EVALUATION


#K-Fold Cross Validation
N = nrow(warfarin)
K = 10
set.seed(1234)
s = sample(1:K, size=N, replace=T)
pred_outputs.lm <- vector(mode="numeric", length=N)
#pred_outputs.rf <- vector(mode="numeric", length=N)
obs_outputs <- vector(mode="numeric", length=N)
offset <- 0
for(i in 1:K){
  train <- filter(warfarin, s != i)
  test <- filter(warfarin, s == i)
  obs_outputs[1:length(s[s==i]) + offset] <- test$Therapeutic.Dose
  
  #LM train/test
  lm <- glm(Therapeutic.Dose ~ Site + Race.OMB + Weight + Height + Valve + Aspirin + Simvastatin + Combined.QC4.CYP2C9 + VKORC1.1 + VKORC1.1_QC, data=train, family=gaussian)
  lm.pred.curr <- predict(lm, test, type="response")
  pred_outputs.lm[1:length(s[s==i]) + offset] <- lm.pred.curr
  offset <- offset + length(s[s==i])
}

warfarin1 <- filter(warfarin, complete.cases(Weight)) #
warfarin1 <- filter(warfarin1, complete.cases(Height))
warfarin1 <- filter(warfarin1, complete.cases(Race.OMB))
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.4_consensus)) #2718
#Deleted VKORC1.4 because it cuts down too much  #2578
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.1_consensus)) #2208
#warfarin1 <- filter(warfarin1, complete.cases(VKORC1.1)) #
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.3_consensus))
warfarin1 <- filter(warfarin1, complete.cases(INR.Reported))
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.6_consensus))
#warfarin1 <- filter(warfarin1, complete.cases(VKORC1.6))
warfarin1 <- filter(warfarin1, complete.cases(Site))
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.3_QC))
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.6_QC ))
#warfarin1 <- filter(warfarin1, complete.cases(VKORC1.5_consensus))
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.5))
warfarin1 <- filter(warfarin1, complete.cases(Amiodarone))
#warfarin1 <- filter(warfarin1, complete.cases(Herbal.Medications.Vitamins.Supps))
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.1_QC))
#warfarin1 <- filter(warfarin1, complete.cases(VKORC1.2_consensus))
warfarin1 <- filter(warfarin1, complete.cases(Combined.QC4.CYP2C9))
warfarin1 <- filter(warfarin1, complete.cases(Race.Reported))
warfarin1 <- filter(warfarin1, complete.cases(Gender))
#warfarin1 <- filter(warfarin1, complete.cases(VKORC1.2))
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.4_QC))
#warfarin1 <- filter(warfarin1, complete.cases(Tylenol))
#warfarin1 <- filter(warfarin1, complete.cases(Smoker))
warfarin1 <- filter(warfarin1, complete.cases(Genotyped.QC3.Cyp2C9))
warfarin1 <- filter(warfarin1, complete.cases(Genotyped.QC4.Cyp2C9))

