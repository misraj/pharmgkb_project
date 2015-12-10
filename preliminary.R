#Load all libraries
library(RCurl)
library(dplyr)
library(devtools)
library(ggplot2)



########~~~~Get and clean data~~~~~~#######
my.url <- getURL("https://raw.githubusercontent.com/misraj/pharmgkb_project/master/warfarin_data.txt") #Getting warfarin data
warfarin <- read.delim(text = my.url, header=TRUE)

#Filter out observations that did not have a thereapeutic dosage data available
warfarin <- filter(warfarin, complete.cases(Therapeutic.Dose))
N = length(warfarin$Subject.ID)
V = length(names(warfarin))
#Plot dosage data
plot(seq(1:N), warfarin$Therapeutic.Dose, xlab  = "Subject", ylab = "Therapeutic Warfarin Dosage")
warfarin <- filter(warfarin, Therapeutic.Dose <= 70)
plot(seq(1:N), warfarin$Therapeutic.Dose, xlab  = "Subject", ylab = "Therapeutic Warfarin Dosage")

#Remove Variables that give us 0 information
warfarin<-dplyr::select(warfarin,-(Cerivastatin))

#Look at individual univariate associations between variable and Therapeutic Dosage
uni_reg <- data.frame(seq(1:V-2),seq(1:V-2))
colnames(uni_reg) <- c("Variable", "Pval")
for( i in 3:V){
  datas.glm <- lm(Therapeutic.Dose~ warfarin[,i:i], data=warfarin)
  pv <-summary(datas.glm)$coefficients[2,4]
  uni_reg[i-2,1] <-names(warfarin)[i]
  uni_reg[i-2,2] <-pv
  i
}
#Sort by most significantly associated
#uni_reg is now a table of variables and their p-values
uni_reg <- arrange(uni_reg,Pval)

#####################Looking at missingness rate############################
missingness_feature <- rep(0, V)
for(i in 1:V){
  table <-table(is.na(warfarin[,i]))
  if(length(table) == 1){
    missingness_feature[i] = table/N
  }
  else{
    missingness_feature[i] = table[1]/N
  }
}
miss_feature<- data.frame(names(warfarin),missingness_feature)
miss_feature<- arrange(miss_feature, desc(missingness_feature))

#Check for subjects with high missingness rate
missingness_subjects <- rep(0,N)
for(i in 1:N){
  table <- table(is.na(warfarin[i,]))
  if(length(table) == 1){
    missingness_subjects[i] = table/V
  }
  else{
    missingness_subjects[i] = table[1]/V
  }
}
miss_subject <- data.frame(rownames(warfarin),missingness_subjects)
miss_subject <- arrange(miss_subject, desc(missingness_subjects))
############################################################################
 
###Mutate/Change values to factors#######
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
######################################################################
#Filter data accordingly WARFARIN 1
warfarin1 <- filter(warfarin, complete.cases(Gender)) #
warfarin1 <- filter(warfarin1, complete.cases(Age))
warfarin1 <- filter(warfarin1, complete.cases(Reached.Stable.Dose))
warfarin1 <- filter(warfarin1, complete.cases(Cyp2C9))
warfarin1 <- filter(warfarin1, complete.cases(Weight))
warfarin1 <- filter(warfarin1, complete.cases(Height))
warfarin1 <- filter(warfarin1, complete.cases(Race.Reported)) #
warfarin1 <- filter(warfarin1, complete.cases(Indication))
warfarin1 <- filter(warfarin1, complete.cases(INR.Reported))
warfarin1 <- filter(warfarin1, complete.cases(Height))
warfarin1 <- filter(warfarin1, complete.cases(VKORC1.1))
warfarin1 <- filter(warfarin1, complete.cases(Heart.probs))
warfarin1 <- filter(warfarin1, complete.cases(Valve))
warfarin1 <- filter(warfarin1, complete.cases(Simvastatin))
warfarin1 <- filter(warfarin1, complete.cases(Aspirin))


#Filter data accordingly WARFARIN 2
warfarin2 <- filter(warfarin, complete.cases(Gender)) #
warfarin2 <- filter(warfarin2, complete.cases(Rifam))
warfarin2 <- filter(warfarin2, complete.cases(Site))
warfarin2 <- filter(warfarin2, complete.cases(Smoker))
warfarin2 <- filter(warfarin2, complete.cases(Weight))
warfarin2 <- filter(warfarin2, complete.cases(Height))
warfarin2 <- filter(warfarin2, complete.cases(Race.Reported)) #
warfarin2 <- filter(warfarin2, complete.cases(INR.Reported))
warfarin2 <- filter(warfarin2, complete.cases(VKORC1.1))
warfarin2 <- filter(warfarin2, complete.cases(Valve))
warfarin2 <- filter(warfarin2, complete.cases(Herbal.Medications.Vitamins.Supps))
warfarin2 <- filter(warfarin2, complete.cases(Amiodarone))
warfarin2 <- filter(warfarin2, complete.cases(Combined.QC4.CYP2C9))


##Choose genotypic data##
table(warfarin2$VKORC1.1) #Yes
table(warfarin2$VKORC1.1_QC) #No
table(warfarin2$VKORC1.1_consensus)  #Yes
table(warfarin2$VKORC1.4_consensus)  #No
table(warfarin2$VKORC1.4) #No
table(warfarin2$VKORC1.3_consensus) #no
table(warfarin2$VKORC1.6_consensus) #no
table(warfarin2$VKORC1.6)  #no

warfarin1 <- dplyr::select(warfarin1, Subject.ID, Sample.ID, Site, Gender, Race.Reported, Race.OMB, Weight, Height, Age, Indication, Heart.probs, Valve, Aspirin, Simvastatin, Genotyped.QC2.Cyp2C9,Genotyped.QC3.Cyp2C9, Combined.QC4.CYP2C9, Cyp2C9, VKORC1.1,VKORC1.1_QC, INR.Reported,Reached.Stable.Dose,Therapeutic.Dose)
warfarin2 <- dplyr::select(warfarin2, Subject.ID, Sample.ID, Site, Gender,Race.Reported, Race.OMB, Weight, Height, Smoker, Rifam, Valve, Combined.QC4.CYP2C9, Genotyped.QC2.Cyp2C9,Genotyped.QC3.Cyp2C9, Combined.QC4.CYP2C9, Cyp2C9, VKORC1.1,VKORC1.1_QC, INR.Reported,Amiodarone, Herbal.Medications.Vitamins.Supps, Therapeutic.Dose)

########Explore the filtered data#############
ggplot(warfarin1, aes(x = Gender)) + geom_bar(stat = "bin")
ggplot(warfarin2, aes(x = Gender)) + geom_bar(stat = "bin")

ggplot(warfarin2, aes(x = Race.Reported)) + geom_bar(stat = "bin")
ggplot(warfarin2, aes(x = Race.OMB)) + geom_bar(stat = "bin")
ggplot(warfarin2, aes(x = Site)) + geom_bar(stat = "bin")
#ggplot(warfarin2, aes(x = Age)) + geom_bar(stat = "bin")
#ggplot(warfarin2, aes(x = factor(Reached.Stable.Dose))) + geom_bar(stat = "bin")
#table(warfarin1$Reached.Stable.Dose)
qplot(Height, data=warfarin2, geom="histogram")
qplot(Weight, data=warfarin2, geom="histogram")


########################################
#Get total points
n1 = length(warfarin1$Subject.ID)
#Split data into x.train, x.test, y.train, and y.test
train_rows1 <- sample(1:n1, .66*n1)
warf.train1 <- warfarin1[train_rows1, ]
warf.test1 <- warfarin1[-train_rows1, ]
dose.train1 <- warfarin1$Therapeutic.Dose[train_rows1]
dose.test1 <- warfarin1$Therapeutic.Dose[-train_rows1]

#Get rid of y in dataset
warf.train1 <- dplyr::select(warf.train1, -(Therapeutic.Dose))
warf.test1 <- dplyr::select(warf.test1, -(Therapeutic.Dose))
#Get rid of ID, it will mess up stepwise regression: test and train
warf.test1 <- dplyr::select(warf.test1, -(Subject.ID))
warf.train1 <- dplyr::select(warf.train1, -(Subject.ID))
warf.test1 <- dplyr::select(warf.test1, -(Sample.ID))
warf.train1 <- dplyr::select(warf.train1, -(Sample.ID))

null1 = lm(dose.train1 ~ Weight, data = warf.train1)
full1 = lm(dose.train1 ~ Gender + Age + Reached.Stable.Dose + Cyp2C9 + Weight + Height + Race.Reported + Indication + INR.Reported + Height + VKORC1.1 + Heart.probs + Valve + Simvastatin + Aspirin, data = warf.train1)
null1a = lm(dose.train1 ~ 1, data = warf.train1)
full1a = lm(dose.train1 ~ ., data = warf.train1)
model_selection1 = step(null1, scope=list(lower=null1, upper=full1), direction="both", criterion = "BIC")
model_selection1a = step(null1a, scope=list(lower=null1a, upper=full1a), direction="both", criterion = "BIC")

n2 = length(warfarin2$Subject.ID)
train_rows2 <- sample(1:n2, .66*n2)
warf.train2 <- warfarin2[train_rows2, ]
warf.test2 <- warfarin2[-train_rows2, ]
dose.train2 <- warfarin2$Therapeutic.Dose[train_rows2]
dose.test2 <- warfarin2$Therapeutic.Dose[-train_rows2]

#Get rid of y in dataset
warf.train2 <- dplyr::select(warf.train2, -(Therapeutic.Dose))
warf.test2 <- dplyr::select(warf.test2, -(Therapeutic.Dose))
#Get rid of ID, it will mess up stepwise regression: test and train
warf.test2 <- dplyr::select(warf.test2, -(Subject.ID))
warf.train2 <- dplyr::select(warf.train2, -(Subject.ID))
warf.test2 <- dplyr::select(warf.test2, -(Sample.ID))
warf.train2 <- dplyr::select(warf.train2, -(Sample.ID))

null2 = lm(dose.train2  ~ Weight, data = warf.train2)
full2 = lm(dose.train2  ~ Gender + Rifam + Site + Smoker + Weight + Height + Race.Reported + INR.Reported + VKORC1.1 + Valve + Herbal.Medications.Vitamins.Supps + Combined.QC4.CYP2C9, data = warf.train2)
null2a = lm(dose.train2  ~ Weight, data = warf.train2)
full2a = lm(dose.train2  ~ ., data = warf.train2)
model_selection2 = step(null2, scope=list(lower=null2, upper=full2), direction="both", criterion = "BIC")
#model_selection2a = step(null2a, scope=list(lower=null2a, upper=full2a), direction="forward", criterion = "BIC")
#getting error

###EVALUATION on TRAINING SET FOR MODEL 1###
warf_train1_reg <- lm(dose.train1 ~ VKORC1.1 + Age + Weight + Cyp2C9 + Race.Reported +  Reached.Stable.Dose + Site,data = warf.train1)
summary(warf_train1_reg)
best.warf_train1_reg <- predict.lm(warf_train1_reg,warf.train1, type = "response")
best.warf_train1_reg <-as.integer(best.warf_train1_reg)
compare_warf_train1 <- data.frame(seq(1:length(warf.train1$Weight)),dose.train1,best.warf_train1_reg )
dose1<- arrange(compare_warf_train1, desc(dose.train1))
colnames(dose1)<- c("Subject","True","Predicted")
SSE = sum((dose1$Predicted - dose1$True)^2)
err = mean(abs(dose1$Predicted - dose1$True))
err.sd = sd(abs(dose1$Predicted - dose1$True))
avg_dose_predicted = mean(dose1$Predicted)
avg_dose_true = mean(dose1$True)
dose_pred.sd = sd(dose1$Predicted)
dose_true.sd = sd(dose1$True)
acc = dose1$Predicted - dose1$True
est = seq(1:length(dose1$Dose))
for( i in 1:length(acc)){
  if (acc[i] == 0){
    est[i] = "c"
  }
  else if(acc[i] > 0){
    est[i] = "o"
  }
  else{
    est[i] = "u"
  }
}
dose1$Label = est
ggplot(data=dose1, aes(x= Predicted, y= True,colour = Label)) +
  geom_point( size=1, shape=21, fill="white")
ggplot(data=dose1, aes(x= Label)) + geom_bar()

###EVALUATION ON TESTING SET FOR MODEL 1###
warf_test1_reg <- lm(dose.test1 ~ VKORC1.1 + Age + Weight + Cyp2C9 + Race.Reported +  Reached.Stable.Dose + Site,data = warf.test1)
summary(warf_test1_reg)
best.warf_test1_reg <- predict.lm(warf_test1_reg,warf.test1, type = "response")
best.warf_test1_reg <-as.integer(best.warf_test1_reg)
compare_warf_test1 <- data.frame(seq(1:length(warf.test1$Weight)),dose.test1,best.warf_test1_reg )
dose1.t<- arrange(compare_warf_test1, desc(dose.test1))
colnames(dose1.t)<- c("Subject","True","Predicted")
SSE1t = sum((dose1.t$Predicted - dose1.t$True)^2)
err1t = mean(abs(dose1.t$Predicted - dose1.t$True))
err.sd1t = sd(abs(dose1.t$Predicted - dose1.t$True))
avg_dose_predicted1t = mean(dose1.t$Predicted)
avg_dose_true1t = mean(dose1.t$True)
dose_pred.sd1t = sd(dose1.t$Predicted)
dose_true.sd1t = sd(dose1.t$True)
acc1t = dose1.t$Predicted - dose1.t$True
est1t = seq(1:length(dose1.t$Dose))
for( i in 1:length(acc1t)){
  if (acc1t[i] == 0){
    est1t[i] = "c"
  }
  else if(acc1t[i] > 0){
    est1t[i] = "o"
  }
  else{
    est1t[i] = "u"
  }
}
dose1.t$Label = est1t
ggplot(data=dose1.t, aes(x= Predicted, y= True,colour = Label)) +
  geom_point( size=1, shape=21, fill="white")

ggplot(data=dose1.t, aes(x= Label)) + geom_bar()


###EVALUATION###
warf_train2_reg <- lm(dose.train2 ~ Weight + Rifam + VKORC1.1 + Race.Reported + Herbal.Medications.Vitamins.Supps + Smoker + Height + Gender + INR.Reported + Combined.QC4.CYP2C9,data = warf.train2)
summary(warf_train2_reg)
best.warf_train2_reg <- predict.lm(warf_train2_reg,warf.train2, type = "response")
best.warf_train2_reg <-as.integer(best.warf_train2_reg)
compare_warf_train2 <- data.frame(seq(1:length(warf.train2$Weight)),dose.train2,best.warf_train2_reg )
dose2<- arrange(compare_warf_train2, desc(dose.train2))
colnames(dose2)<- c("Subject","True","Predicted")
SSE2 = sum((dose2$Predicted - dose2$True)^2)
err2 = mean(abs(dose2$Predicted - dose2$True))
err.sd2 = sd(abs(dose2$Predicted - dose2$True))
avg_dose_predicted2 = mean(dose2$Predicted)
avg_dose_true2 = mean(dose2$True)
dose_pred.sd2 = sd(dose2$Predicted)
dose_true.sd2 = sd(dose2$True)
acc2 = dose2$Predicted - dose2$True
est2 = seq(1:length(dose1$Dose))
for( i in 1:length(acc2)){
  if (acc2[i] == 0){
    est2[i] = "c"
  }
  else if(acc2[i] > 0){
    est2[i] = "o"
  }
  else{
    est2[i] = "u"
  }
}
dose2$Label = est2
ggplot(data=dose2, aes(x= Predicted, y= True,colour = Label)) +
  geom_point( size=1, shape=21, fill="white")

ggplot(data=dose2, aes(x= Label)) + geom_bar()

###EVALUATION###
warf_test2_reg <- lm(dose.test2 ~ Weight + Rifam + VKORC1.1 + Race.Reported + Herbal.Medications.Vitamins.Supps + Smoker + Height + Gender + INR.Reported + Combined.QC4.CYP2C9,data = warf.test2)
summary(warf_test2_reg)
best.warf_test2_reg <- predict.lm(warf_test2_reg,warf.test2, type = "response")
best.warf_test2_reg <-as.integer(best.warf_test2_reg)
compare_warf_test2 <- data.frame(seq(1:length(warf.test2$Weight)),dose.test2,best.warf_test2_reg )
dose2.t<- arrange(compare_warf_test2, desc(dose.test2))
colnames(dose2.t)<- c("Subject","True","Predicted")
SSE2t = sum((dose2.t$Predicted - dose2.t$True)^2)
err2t = mean(abs(dose2.t$Predicted - dose2.t$True))
err.sd2t = sd(abs(dose2.t$Predicted - dose2.t$True))
avg_dose_predicted2t = mean(dose2.t$Predicted)
avg_dose_true2t = mean(dose2.t$True)
dose_pred.sd2t = sd(dose2.t$Predicted)
dose_true.sd2t = sd(dose2.t$True)
acc2t = dose2.t$Predicted - dose2.t$True
est2t = seq(1:length(dose1$Dose))
for( i in 1:length(acc2t)){
  if (acc2t[i] == 0){
    est2t[i] = "c"
  }
  else if(acc2t[i] > 0){
    est2t[i] = "o"
  }
  else{
    est2t[i] = "u"
  }
}
dose2.t$Label = est
ggplot(data=dose2.t, aes(x= Predicted, y= True,colour = Label)) +
  geom_point( size=1, shape=21, fill="white")
ggplot(data=dose2.t, aes(x= Label)) + geom_bar()

#############################Include BMI################################
bmi <- warfarin$Weight
for(i in 1:1230){
  h <- warfarin$Height[i]/100
  den <- h *h
  bmi[i] <- warfarin$Weight[i]/ den
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
warfarin <- cbind(warfarin, bmi)
warfarin <- mutate(warfarin, bmi = factor(bmi, levels=c(1, 2,3,4,5,6), labels=c("underweight", "normal", "overweight", "obesity1", "obesity2", "obesity3")))


#################################################################################

#leaps = regsubsets(Therapeutic.Dose ~. , data = warfarin1, nbest = 10, really.big = T)