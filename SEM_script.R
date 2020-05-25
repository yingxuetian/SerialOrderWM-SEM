working_dir = 'DATA'
outlier_dir = 'sub_outlier'

##################### >>>>>>>>> 1.1. fast subject-level outlier detection w/ stimulus logRT <<<<<<<<<<<<<<< #######################
##################### intra-individual trimming trial-outliers for each subject #######################
##################### calculate stimulus logRT after cleaning #######################
setwd(raw_dir)
subjectfiles <- list.files(pattern='*\\.xlsx', recursive=TRUE,full.names = TRUE)
Temp <- list()
library(gdata)
#### for each individual, cutoff 10s, 3IQR from the mean trimming, log10 transform 
for(i in 1:length(subjectfiles)) {
  print(i)
  print(round(i/length(subjectfiles), 3)) #progress %
  # for list[i]
  X <- gdata::read.xls(subjectfiles[i],sheet= 7, header = T)

  #### extract subject's RT for correct trials (raw RT) 
  Y1<- as.numeric(as.character(X[5:124,3]))#i-lt
  Y2<- as.numeric(as.character(X[5:124,4]))#i-wd
  Y3<- as.numeric(as.character(X[5:124,5]))#i-loc
  Y4<- as.numeric(as.character(X[5:124,6]))#i-arw
  Y5<- as.numeric(as.character(X[5:124,8]))#o-lt
  Y6<- as.numeric(as.character(X[5:124,9]))#o-wd
  Y7<- as.numeric(as.character(X[5:124,10]))#o-loc
  Y8<- as.numeric(as.character(X[5:124,11]))#o-arw
  Y9<- as.numeric(as.character(X[5:124,13]))#yes-lt
  Y10<- as.numeric(as.character(X[5:124,14]))#yes-wd
  Y11<- as.numeric(as.character(X[5:124,15]))#yes-loc
  Y12<- as.numeric(as.character(X[5:124,16]))#yes-arw
  
  Y13 <- c(Y1,Y5,Y9) #i+o+yes #lt
  Y14 <- c(Y2,Y6,Y10) #i+o+yes #wd
  Y15 <- c(Y3,Y7,Y11) #i+o+yes #loc
  Y16 <- c(Y4,Y8,Y12) #i+o+yes #arw
  Y <- cbind(Y13,Y14,Y15,Y16) #4*360 per person
  colnames(Y) <- c("letter", "word","corsi","arrow") #Y is the whole RT dataset for each participant; 4 cols and 360 rows (item 1-120, order 121-240, yes 241-360)
  
  #### >>> remove trial-outlier in RT
  #### cutoff RT that exceeds 10s (only keep RT <= 10s) 
  for (j in 1:4){
    for (k in 1:360){
      if (!(is.na(Y[k,j]))) {
        if(as.numeric(Y[k,j])>10000){Y[k,j]=NA}
      }}}
  
  ###### cut off RT that's out of 3IQR for each stimulus type from subject's mean (trimRT); only leave [Q1-3IQR,Q3+3IQR] 
  for (l in 1:4){
    BoxMe = Y[,l] # for each stimulus
    BPlot = boxplot(BoxMe, range=3, col="lightgray") #here range=3 sets whiskers to 3IQR from Q1 and Q3, so no need to calculate whiskers
    points(mean(BoxMe,na.rm=T), pch=15)
    for (m in 1:length(BoxMe)) {
      BoxMe[m][BoxMe[m] < BPlot$stats[1,]] = NA # replace low
      BoxMe[m][BoxMe[m] > BPlot$stats[5,]] = NA # replace high
      #if do 3IQR by hand
      #IQR <- BPlot$stats[4,]-BPlot$stats[2,]     
      #upper = min(max(BoxMe,na.rm=TRUE), BPlot$stats[4,] + 3 * IQR) #if dont set range value, 1 and 5th stats is 1.5IQR from Q1 and Q3 
      #lower = max(min(BoxMe,na.rm=TRUE), BPlot$stats[2,] - 3 * IQR)
      #if(!(is.na(BoxMe[m]))) {
      #BoxMe[m][BoxMe[m] < lower] = NA    # replace low
      #BoxMe[m][BoxMe[m] > upper] = NA    # replace high
    }
  }
  ##### log tranform trimRT 
  logY <- log10(Y)
  colnames(logY) = paste("log",colnames(Y),sep="_")
  #### average for each task 
  Z <- c(colMeans(Y,na.rm = TRUE),colMeans(logY,na.rm = T))
  ##### write out for subject i 
  Temp[[i]] <- c(subjectfiles[i],Z)
}   
Data <- do.call(rbind, Temp)
Data = as.data.frame(Data)
writexl::write_xlsx(Data,"stimlogtrimRT_outlier.xlsx")


#### fast suject-level outlier detecting w/ stimulus_log10_RT ######
# across subjects, set outlier bar for Q1-3IQR and Q3+3*IQR
# manually transformed "stimuluslogRT_3IQRwithintrimmed.csv" into "stimuluslogRT_pre_outlier.xlsx"
setwd(outlier_dir)
acrosssubj <- readxl::read_excel("stimlogtrimRT_outlier.xlsx")
    psych::describe(acrosssubj)  

# 3IQR as hinge      
BPlot1 = boxplot(acrosssubj[,6:9], range=3,ylim = c(2,4),staplelwd = 2, staplelty = 3,staplecol = "red",outline=F,boxlty=0,whisklty=0,medlty = "blank" )
# regular boxplot for visualization
BPlot = boxplot(acrosssubj[,6:9], range=1.5,boxwex=0.8,ylim = c(2,4),col="lightgray", whisklty = 1, names = c("Letter", "Word", "Location", "Arrow"), ylab = "Stimulus logRT")
points(colMeans(acrosssubj[,6:9]), pch=15, col="blue")
segments(x0=1:4-0.4,x1=1:4+0.4,y0= BPlot1$stats[1,], lwd = 2, lty = 3,col = "red")
segments(x0=1:4-0.4,x1=1:4+0.4,y0= BPlot1$stats[5,], lwd = 2, lty = 3,col = "cyan")

##### fast subject outlier
acrosssubj$subj[which(acrosssubj$log_letter< BPlot1$stats[1,1])]
acrosssubj$subj[which(acrosssubj$log_word< BPlot1$stats[1,2])]
# subject 48
acrosssubj$subj[which(acrosssubj$log_corsi< BPlot1$stats[1,3])]
acrosssubj$subj[which(acrosssubj$log_arrow< BPlot1$stats[1,4])]


############## >>>>>>>>> 1.2. low subject-level outlier detection w/ stimulus accuracy <<<<<<<<< #########
# revised from acc_notrial.R
######### calculate stim_accuracy ###########
setwd(working_dir)
X = readxl::read_excel("sum_acc.xlsx")
X = as.data.frame(X)
X$letterAcc <- (as.numeric(as.character(X$I_letter_acc))*30+as.numeric(as.character(X$O_letter_acc))*30+as.numeric(as.character(X$letterYacc))*60)/120 #letter
X$wordAcc <- (as.numeric(as.character(X$I_word_acc))*30+as.numeric(as.character(X$O_word_acc))*30+as.numeric(as.character(X$wordYacc))*60)/120 #word
X$locationAcc <- (as.numeric(as.character(X$I_location_acc))*30+as.numeric(as.character(X$O_location_acc))*30+as.numeric(as.character(X$locationYacc))*60)/120 #location
X$arrowAcc <- (as.numeric(as.character(X$I_arrow_acc))*30+as.numeric(as.character(X$O_arrow_acc))*30+as.numeric(as.character(X$arrowYacc))*60)/120 #arrow
        # # for two subjects w/ less trials
        #   setwd(raw_dir)
        #   # sub 49 letter
        #   X = gdata::read.xls("./49.xlsx",sheet= 1, header = T)
        #   mean(X$correct.1[5:124],na.rm = T)  # 0.754717
        #   X$letterAcc[X$subj==49] = 0.754717
        #   # sub 106 word
        #   X = gdata::read.xls("./106.xlsx",sheet= "word", header = T)
        #   mean(X$correct.1[5:124],na.rm = T)  # 0.625
        #   X$wordAcc[X$subj==106] = 0.625
# behavioral data frame Y
Y<- cbind(X$subj,X$letterAcc,X$wordAcc,X$locationAcc,X$arrowAcc)
colnames(Y) <- c("subj","letterAcc", "wordAcc","locationAcc","arrowAcc")
writexl::write_xlsx(Y,"stimacc_outlier.xlsx")

setwd(outlier_dir)
Y = readxl::read_excel("stimacc_outlier.xlsx")
# descrptive stats
psych::describe(Y)

# visualization of stimulus accuracy
BPlot = boxplot(Y[,2:5], ylim = c(0,1),col="lightgray", names = c("Letter", "Word", "Location", "Arrow"), ylab = "Stimulus Accuracy")
points(colMeans(Y[,2:5]), pch=15)
abline(h=0.575, col = "Red", lty = 5, lwd = 3)
BPlot$stats

# visualization w/ boxplot on the left and scatterplot on the right
# boxplot
op <- par(mfrow=c(1,2))
BPlot = boxplot(Y[1:153,2:5], range=1.5,ylim = c(0,1),col="lightgray", whisklty = 1, names = c("Letter", "Word", "Location", "Arrow"), ylab = "Stimulus Accuracy")
points(colMeans(Y[1:153,2:5]), pch=15, col="blue")
abline(h=0.575, col = "Red", lty = 3, lwd = 2)
# scatterplot
stripchart(Y[1:153,2:5], ylim=c(0,1),vertical = TRUE, pch=1, method="jitter")
points(colMeans(Y[1:153,2:5]), pch=15, col="blue") # add means
# points(c(1:4),colMedians(Y[,2:5]), pch=5,col="blue") # add medians
abline(h=0.575, col = "Red", lty = 3, lwd = 2) 
# reset graphics parameters
par(op) 

#### low outlier ####
Y$subj[Y$letterAcc <=0.575]
# 11  33  50  66  69  88 123 130 134
Y$subj[Y$wordAcc <=0.575]
# 5   7  11  18  32  69  88 134
Y$subj[Y$locationAcc <=0.575]
# 14  18  33  41  49  69  88  94  96 106 130 134 135
Y$subj[Y$arrowAcc <=0.575]
# 2  11  12  14  15  18  23  25  32  33  35  41  46  55  58  64  66  88  90  96 100 103 106 108 121 130 134 135 136 138 144 145 147 159

##### 1. subject outlier summary #######
      # # fast outlier
        # word: subject 48
      # # low outlier
        # letter: 11  33  50  66  69  88 123 130 134
        # word: 5   7  11  18  32  69  88 134
        # location: 14  18  33  41  49  69  88  94  96 106 130 134 135
        # arrow: 2  11  12  14  15  18  23  25  32  33  35  41  46  55  58  64  66  88  90  96 100 103 106 108 121 130 134 135 136 138 144 145 147 159

################### >>>>>>>>>>>>>> 2. accuracy <<<<<<<<<<############################
setwd(working_dir)
datacc <- gdata::read.xls("sum_acc.xlsx", header = TRUE)
dat01 <- datacc[1:153,-1]

####### >>> 8 indicators: descriptive & intercorrelation  ########
descriptive_acc <- psych::describe(dat01)
writexl::write_xlsx(descriptive_acc,"descriptive_acc.xlsx")

eight_ind <- c("I_letter_acc","O_letter_acc","I_word_acc","O_word_acc","I_location_acc","O_location_acc","I_arrow_acc","O_arrow_acc")
dat1 = dat01[,eight_ind]

boxplot(dat1,ylim = c(0,1),
        col=c("purple","orangered","purple","orangered","dodgerblue","orange","dodgerblue","orange"),
        names=c("Item Letter","Order Letter","Item Word","Order Word", "Item Location","Order Location", "Item Arrow","Order Arrow"))
title(ylab="Accuracy", line=2, cex.lab=1.2)
points(colMeans(dat1), pch=15)

#### *** task difficulty  ###########
colMeans(dat1)
#### factorial rm-ANOVA w/ 4 tasks -- domain*I/O -- #####
# use wide-format data -- all task difficulty levels for acc 
# 2-by-2 ANOVA (I/o-by-v/nv)
Anv10 = lm(cbind(dat1$I_letter_acc,dat1$O_letter_acc,dat1$I_word_acc,dat1$O_word_acc, dat1$I_location_acc, dat1$O_location_acc, dat1$I_arrow_acc, dat1$O_arrow_acc)~1)
Dom = factor(c(rep("verbal", 4), rep("nonverbal",4)))
Typ = factor(rep(c("item", "order"), 4))
Anv11 = car::Anova(Anv10, idata=data.frame(Dom, Typ), idesign=~Dom*Typ)
summary(Anv11, multivariate=F) # F(1,152) = 87.36
# omega^2 = (SS_effect - df_effect*ES_effect/den_df_effect) / (SS_effect+ES_effect+ ES_effect/den_df_effect)
omegasquared = (.58-1*1.0157/152)/(.58+1.0157+1.0157/152) = 0.3577909

###### multivariate normality #######
# visualize w/ histogram to see if it's roughly normally distributed
for (i in 1:12) {
  hist(dat01[,i],xlab=names(dat01[i]),main=NULL,breaks=seq(0,1,by=0.1))
}
# multivariate normality
library(MVN)
MVN::mardiaTest(dat1, cov = TRUE, qqplot = FALSE)
#g1p: Mardia's multivariate skewness statistic
#chi.skew: Chi-square value of the skewness statistic
#p.value.skew: p-value of the skewness statistic
#g2p: Mardia's multivariate kurtosis statistic
#z.kurtosis: z value of thekurtosis statistic
#p.value.kurt: p-value of the kurtosis statistic
#chi.small.skew: Chi-square value of the small sample skewness statistic
#p.value.small: p-value of small sample skewness statistic
# For multivariate normality, both p-values of skewness and kurtosis statistics should be greater than 0.05. 
# If sample size (n) is less than 20 then 'p.value.small' should be used as significance value of skewness instead of 'p.value.skew'. 

####### correlation matrix of 8 indicators ####### 
cor_acc <- Hmisc::rcorr(as.matrix(dat1), type="spearman") 
writexl::write_xlsx(cor_acc$r, "cor_acc_sp.xlsx")

####### splithalf reliability ####### 
cor_acc_sh <- Hmisc::rcorr(as.matrix(dat01[,13:36], method="spearman"))
writexl::write_xlsx(cor_acc_sh$r, "cor_acc_sh.xlsx")

####### >>> 16 indicators: descriptive & intercorrelation  ########
datacc <- readxl::read_excel("sum_acc.xlsx", 1,header = TRUE)
dat01 <- datacc[1:153,-1] 
#16 parceled accuracy dataframe
sixteen_ind = c("I_letter_acc1","I_letter_acc2","O_letter_acc1","O_letter_acc2","I_word_acc1","I_word_acc2","O_word_acc1","O_word_acc2","I_location_acc1","I_location_acc2","O_location_acc1","O_location_acc2","I_arrow_acc1","I_arrow_acc2","O_arrow_acc1","O_arrow_acc2")   
dat1 = dat01[,sixteen_ind]

######## descriptives for parceled accuracy ###########
descriptive_acc_parcel <- psych::describe(dat01[,13:36])
writexl::write_xlsx(descriptive_acc_parcel, "descriptive_acc_parcel.xlsx")
# boxplot
boxplot(dat1,ylim = c(0,1),cex.axis=0.8,
        col=c("purple","purple","orangered","orangered","purple","purple","orangered","orangered","dodgerblue","dodgerblue","orange","orange","dodgerblue","dodgerblue","orange","orange"),
        names=c("ItemLetter1","ItemLetter2","OrderLetter1","OrderLetter2","ItemWord1","ItemWord2","OrderWord1","OrderWord2", "ItemLocation1","ItemLocation2","OrderLocation1","OrderLocation2", "ItemArrow1","ItemArrow2","OrderArrow1","OrderArrow2"))
points(colMeans(dat1), pch=15)
title(ylab="Parceled Accuracy", line=2, cex.lab=0.8)
# histogram to see if it's roughly normally distributed
for (i in 1:16) {
  hist(dat1[,i],xlab=names(dat1[i]),main=NULL,breaks=seq(0,1,by=0.1))
}
# multivariate normality
### http://www.biosoft.hacettepe.edu.tr/MVN/
MVN::mardiaTest(dat1, cov = TRUE, qqplot = FALSE)

########## correlation matrix of 16 indicators ########## 
cor_acc <- Hmisc::rcorr(as.matrix(dat1), type="spearman") 
writexl::write_xlsx(cor_acc$r, "cor_acc_spearman_parcel.xlsx")

########## splithalf reliability of 16 indicators ########## 
cor_sh <- gdata::read.xls("acc_32parcel.xlsx",sheet= 1, header = T)
cor_acc_parcel_sh <- Hmisc::rcorr(as.matrix(cor_sh[,2:33]), type ="spearman")
writexl::write_xlsx(cor_acc_parcel_sh$r, "cor_acc_spearman_parcel_split.xlsx")

############### >>>>>>>>> ** 3. accuracy SEMs ** <<<<<<<<<<<<< ####################
setwd(working_dir)

load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

packages <- c("lavaan","semPlot","semTools","nonnest2","htmlTable","readxl")
load(packages)

datacc <- readxl::read_excel("sum_acc.xlsx")
datacc = as.data.frame(datacc)
dat01 <- datacc[1:153,-1] 
# 16 parceled accuracy
sixteen_ind = c("I_letter_acc1","I_letter_acc2","O_letter_acc1","O_letter_acc2","I_word_acc1","I_word_acc2","O_word_acc1","O_word_acc2","I_location_acc1","I_location_acc2","O_location_acc1","O_location_acc2","I_arrow_acc1","I_arrow_acc2","O_arrow_acc1","O_arrow_acc2")   
dat1 = dat01[,sixteen_ind]
# 8 accuracy measures
eight_ind <- c("I_letter_acc","O_letter_acc","I_word_acc","O_word_acc","I_location_acc","O_location_acc","I_arrow_acc","O_arrow_acc")
dat2 = dat01[,eight_ind]

####################### *** model 45: (2fac: v + nv) w/ stimulus factors -- Model 1 #######################################################
model45 <- ' 
verbal =~ I_letter_acc1 + I_letter_acc2 + I_word_acc1 + I_word_acc2 +O_letter_acc1 + O_letter_acc2 + O_word_acc1 + O_word_acc2 
nonverbal =~ I_location_acc1 + I_location_acc2 + I_arrow_acc1 + I_arrow_acc2 + O_location_acc1 + O_location_acc2 + O_arrow_acc1 + O_arrow_acc2 
letter =~ I_letter_acc1 + I_letter_acc2 + O_letter_acc1 + O_letter_acc2
word =~ I_word_acc1 + I_word_acc2 + O_word_acc1 + O_word_acc2 
loc =~ I_location_acc1 + I_location_acc2 + O_location_acc1 + O_location_acc2
arrow =~ I_arrow_acc1 + I_arrow_acc2 + O_arrow_acc1 + O_arrow_acc2 
verbal ~~ nonverbal
'
fit45 <- sem(model45, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=TRUE) 
summary(fit45,fit.measures=T)
standardizedSolution(fit45) # read path loadings from this output 
inspect(fit45,what="std")
#fitMeasures(fit45, c("chisq","srmr","rmsea"))
#fitMeasures(fit45, c("nfi" ,"tli","cfi","ifi","rfi","ECVI"))
#fitMeasures(fit45, c("AIC","BIC","SABIC"))
fitMeasures(fit45,c("chisq","srmr","rmsea","cfi","nfi","tli","AIC","BIC"))

resid(fit45, type="standardized") #(unstandardized) residuals of a fitted model -the difference between the observed and implied covariance matrix and mean vector
# If the estimator is maximum likelihood, it is also possible to obtain the normalized and the standardized residuals (note: NA values can be safely ignored)
cor_table <- residuals(fit45, type = "cor")$cor
cor_table[upper.tri(cor_table)] <- NA 
diag(cor_table) <- NA 
kable(cor_table, digits=2) 
# Keep an eye out for residual correlations larger than about .1.RMSEA is based on these residual correlations, so the deviations we’re seeing here are what’s driving the RMSEA value we saw above.
writexl::write_xlsx(cor_table,"resid_cor_model1.xlsx")

mi <- modificationIndices(fit45, sort.=TRUE, minimum.value=3)
writexl::write_xlsx(mi,"mi_model1.xlsx")

# CI for correlations btwn latent varibles (psi matrix)
# w/ or w/o NA*1st indicator to make sure it's a free parameter, parameter estimates get the same result
# as long as std.lv=T, default is to standardize the variance instead of fix the 1st indicator as the scale for that latent variable 
model45 <- ' 
verbal =~ NA*I_letter_acc1 + I_letter_acc2 + I_word_acc1 + I_word_acc2 +O_letter_acc1 + O_letter_acc2 + O_word_acc1 + O_word_acc2 
nonverbal =~ NA*I_location_acc1 + I_location_acc2 + I_arrow_acc1 + I_arrow_acc2 + O_location_acc1 + O_location_acc2 + O_arrow_acc1 + O_arrow_acc2 
letter =~ NA*I_letter_acc1 + I_letter_acc2 + O_letter_acc1 + O_letter_acc2
word =~ NA*I_word_acc1 + I_word_acc2 + O_word_acc1 + O_word_acc2 
loc =~ NA*I_location_acc1 + I_location_acc2 + O_location_acc1 + O_location_acc2
arrow =~ NA*I_arrow_acc1 + I_arrow_acc2 + O_arrow_acc1 + O_arrow_acc2 
verbal ~~ nonverbal
'
fit45 <- sem(model45, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=TRUE, std.lv=T) 
parameterEstimates(fit45, ci = TRUE, level = 0.95) # unstandardized parameter estimates, use std.lv=T to run the fit then get CI for correlations btwn latent varibles; for standardized factor loadings, still need to inspect(fit45,what="std")

################################### *** model 41: 35 (4fac: vi+vo+nvi+nvo) w/ stimulus factors domain-specific order -- Model2 ##################################
model41 <- ' 
verbal_I =~ I_letter_acc1 + I_letter_acc2 + I_word_acc1 + I_word_acc2 
verbal_O =~ O_letter_acc1 + O_letter_acc2 + O_word_acc1 + O_word_acc2 
nonverbal_I =~ I_location_acc1 + I_location_acc2 + I_arrow_acc1 + I_arrow_acc2 
nonverbal_O =~ O_location_acc1 + O_location_acc2 + O_arrow_acc1 + O_arrow_acc2 
letter =~ I_letter_acc1 + I_letter_acc2 + O_letter_acc1 + O_letter_acc2
word =~ I_word_acc1 + I_word_acc2 + O_word_acc1 + O_word_acc2 
loc =~ I_location_acc1 + I_location_acc2 + O_location_acc1 + O_location_acc2
arrow =~ I_arrow_acc1 + I_arrow_acc2 + O_arrow_acc1 + O_arrow_acc2 
verbal_I ~~ verbal_O
nonverbal_I ~~ nonverbal_O
verbal_I ~~ nonverbal_I
verbal_O ~~ nonverbal_O
verbal_I ~~ nonverbal_O
verbal_O ~~ nonverbal_I
'
fit41 <- sem(model41, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T)
eigen(inspect(fit41,"cov.lv")) #eigen value of latent covariance matrix

summary(fit41, fit.measures = TRUE)
fitMeasures(fit41, c("chisq","srmr","rmsea"))
fitMeasures(fit41, c("nfi" ,"tli","cfi","ifi","rfi","ECVI")) #tli = nnfi
fitMeasures(fit41, c("AIC","BIC","SABIC"))
fitMeasures(fit41,c("chisq","rmsea","srmr","cfi","nfi","tli","AIC","BIC")) #as Kane et al., 2004

inspect(fit41,what="std") # lambda (factor loading) and psi(cor btwn latent) matrix significance tests, theta is teh residual variance of manifest #this is where all reports are
standardizedSolution(fit41) #standardized parameter estimates #get residual from here
## model test baseline model for null/independent model
# return the lambda (factor loadings), theta (observed error covariance matrix), psi (latent covariance matrix), and beta (latent paths) matrices.

# latent var correlation CI
fit41 <- sem(model41, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T, std.lv=T)
parameterEstimates(fit41, ci = TRUE, level = 0.95) # unstandardized parameter estimates

# resid(fit41, type="standardized")
residuals(fit41, type = "cor")
cor_table <- residuals(fit41, type = "cor")$cor
cor_table[upper.tri(cor_table)] <- NA # erase the upper triangle
diag(cor_table) <- NA # erase the diagonal 0's
kable(cor_table, digits=2) # makes a nice table and rounds everyhing to 2 digits
# Keep an eye out for residual correlations larger than about .1.RMSEA is based on these residual correlations, so the deviations we’re seeing here are what’s driving the RMSEA value we saw above.
writexl::write_xlsx(cor_table,"resid_cor_model2.xlsx")

mi2 <- modificationIndices(fit41, sort.=TRUE, minimum.value=3)
writexl::write_xlsx(mi2,"mi_model2.xlsx")

#residual correlations
fit41 <- sem(model41, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T, std.lv = T)
parameterEstimates(fit41, ci = TRUE, level = 0.95) # unstandardized parameter estimates, use std.lv=T to run the fit then get CI for correlations btwn latent varibles
# use std.lv to standardize the output, then in parameter estimate we can get CI for correlations among latent variables
# if use std report w/o std.lv=T, we can get the correlation between lvs with inspect, but no CI estimate will be reported

# additional info
reliability(fit41)
nullRMSEA(fit41, scaled = FALSE, silent=FALSE)
inspect(fit41,what="cov.lv") #variance=0.007
fitted(fit41) # the model-implied (fitted) covariance matrix (and mean vector) of a fitted model
mi <- modindices(fit41,sort. = TRUE) # mi:modification index
#epc: the expected parameter change (EPC) values
#the standardized EPC values sepc.lv, sepc.all, sepc.nox
#sepc.lv: only standardizing the latent variables; 
#sepc.all: standardizing all variables; 
#sepc.nox: standardizing all but exogenous observed variables
mi[mi$op == "=~",] # modification index for factor loading
resid(fit41, type="standardized") #(unstandardized) residuals of a fitted model -the difference between the observed and implied covariance matrix and mean vector
# If the estimator is maximum likelihood, it is also possible to obtain the normalized and the standardized residuals (note: NA values can be safely ignored)
inspect(fit41, what="start") #the starting values of parameters in each model matrix
inspect(fit41, what="list") #lavaan internally represents a model
inspect(fit41) #free parameters are non-zero
coef(fit41)


################################### *** model 6: new 3fac (v+nvi+nvo) w/ stimulus factors -- Model4 ##################################
model6 <- ' 
verbal =~ I_letter_acc1 + I_letter_acc2 + I_word_acc1 + I_word_acc2 + O_letter_acc1 + O_letter_acc2 + O_word_acc1 + O_word_acc2 
nonverbal_I =~ I_location_acc1 + I_location_acc2 + I_arrow_acc1 + I_arrow_acc2 
nonverbal_O =~ O_location_acc1 + O_location_acc2 + O_arrow_acc1 + O_arrow_acc2 
letter =~ I_letter_acc1 + I_letter_acc2 + O_letter_acc1 + O_letter_acc2
word =~ I_word_acc1 + I_word_acc2 + O_word_acc1 + O_word_acc2 
loc =~ I_location_acc1 + I_location_acc2 + O_location_acc1 + O_location_acc2
arrow =~ I_arrow_acc1 + I_arrow_acc2 + O_arrow_acc1 + O_arrow_acc2 
nonverbal_I ~~ nonverbal_O
verbal ~~ nonverbal_I
verbal ~~ nonverbal_O
'
fit6 <- sem(model6, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T)
summary(fit6, fit.measures = TRUE)
fitMeasures(fit6,c("chisq","srmr","rmsea","cfi","nfi","tli","AIC","BIC")) #as Kane et al., 2004
standardizedSolution(fit6) #read loadings from this output

# latent var correlation CI
fit6 <- sem(model6, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T, std.lv=T)
parameterEstimates(fit6, ci = TRUE, level = 0.95) # unstandardized parameter estimates

####################### *** model 44: 39 (3fac domain-general: vi+nvi+o) w/ stimulus factors domain-general order (model3) #######################################################
model44 <- ' 
verbal_I =~ I_letter_acc1 + I_letter_acc2 + I_word_acc1 + I_word_acc2 
Order =~ O_letter_acc1 + O_letter_acc2 + O_word_acc1 + O_word_acc2 +O_location_acc1 + O_location_acc2 + O_arrow_acc1 + O_arrow_acc2 
nonverbal_I =~ I_location_acc1 + I_location_acc2 + I_arrow_acc1 + I_arrow_acc2 
letter =~ I_letter_acc1 + I_letter_acc2 + O_letter_acc1 + O_letter_acc2
word =~ I_word_acc1 + I_word_acc2 + O_word_acc1 + O_word_acc2 
loc =~ I_location_acc1 + I_location_acc2 + O_location_acc1 + O_location_acc2
arrow =~ I_arrow_acc1 + I_arrow_acc2 + O_arrow_acc1 + O_arrow_acc2 
verbal_I ~~ Order
nonverbal_I ~~ Order
verbal_I ~~ nonverbal_I
'
fit44 <- sem(model44, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=TRUE) 
summary(fit44,fit.measures=T)
fitMeasures(fit44, c("chisq","srmr","rmsea"))
fitMeasures(fit44, c("nfi" ,"tli","cfi","ifi","rfi","ECVI"))
fitMeasures(fit44, c("AIC","BIC","SABIC"))
fitMeasures(fit44,c("chisq","srmr","cfi","nfi","tli","AIC","BIC")) #as Kane et al., 2004

inspect(fit44,what="std") 
standardizedSolution(fit44)

resid(fit44, type="standardized") #(unstandardized) residuals of a fitted model -the difference between the observed and implied covariance matrix and mean vector
# If the estimator is maximum likelihood, it is also possible to obtain the normalized and the standardized residuals (note: NA values can be safely ignored)
cor_table <- residuals(fit44, type = "cor")$cor
cor_table[upper.tri(cor_table)] <- NA # erase the upper triangle
diag(cor_table) <- NA # erase the diagonal 0's
kable(cor_table, digits=2) # makes a nice table and rounds everyhing to 2 digits
# Keep an eye out for residual correlations larger than about .1.RMSEA is based on these residual correlations, so the deviations we’re seeing here are what’s driving the RMSEA value we saw above.
writexl::write_xlsx(cor_table,"resid_cor_model3.xlsx")

mi3 <- modificationIndices(fit44, sort.=TRUE, minimum.value=3)
writexl::write_xlsx(mi3,"mi_model3.xlsx")

# latent var correlation CI
fit44 <- sem(model44, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T, std.lv=T)
parameterEstimates(fit44, ci = TRUE, level = 0.95) # unstandardized parameter estimates

################################### *** S1 feature model 47: model0: item vs. order WM (no domain) ###########################################
model47 <- '
Item =~ I_letter_acc1 + I_letter_acc2 + I_word_acc1 + I_word_acc2+I_location_acc1 +I_location_acc2+ I_arrow_acc1 +I_arrow_acc2
Order =~ O_letter_acc1 +O_letter_acc2+  O_word_acc1 +O_word_acc2+ O_location_acc1 +O_location_acc2+ O_arrow_acc1+O_arrow_acc2
Item ~~ Order
letter =~ I_letter_acc1 + I_letter_acc2 + O_letter_acc1 + O_letter_acc2
word =~ I_word_acc1 + I_word_acc2 + O_word_acc1 + O_word_acc2 
loc =~ I_location_acc1 + I_location_acc2 + O_location_acc1 + O_location_acc2
arrow =~ I_arrow_acc1 + I_arrow_acc2 + O_arrow_acc1 + O_arrow_acc2 
'
fit47 <- sem(model47, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T) 
summary(fit47,fit.measures=T)
standardizedSolution(fit47)
fitMeasures(fit47, c("chisq","srmr","rmsea"))
fitMeasures(fit47, c("nfi" ,"tli","cfi","ifi","rfi","ECVI"))
fitMeasures(fit47, c("AIC","BIC","SABIC"))

fit47 <- sem(model47, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T,std.lv=T) 
parameterEstimates(fit47, ci = TRUE, level = 0.95) # unstandardized parameter estimates 


################################### *** model S0: one factor WM -- not converged ###########################################
model00 <- '
WM =~ I_letter_acc1+I_letter_acc2 + I_word_acc1 + I_word_acc2+ I_location_acc1 +I_location_acc2+ I_arrow_acc1 +I_arrow_acc2+ O_letter_acc1 +O_letter_acc2+  O_word_acc1 + O_word_acc2 + O_location_acc1 + O_location_acc2 + O_arrow_acc1+O_arrow_acc2
letter =~ I_letter_acc1 + I_letter_acc2 + O_letter_acc1 + O_letter_acc2
word =~ I_word_acc1 + I_word_acc2 + O_word_acc1 + O_word_acc2 
loc =~ I_location_acc1 + I_location_acc2 + O_location_acc1 + O_location_acc2
arrow =~ I_arrow_acc1 + I_arrow_acc2 + O_arrow_acc1 + O_arrow_acc2 
'
fit00 <- sem(model00, data = dat1,estimator="ML",likelihood = "wishart") #unbiased sample covariance; multiply chi-square by n-1 

fitMeasures(fit00, c("chisq","srmr","rmsea"))
fitMeasures(fit00, c("nfi" ,"tli","cfi","ifi","rfi","ECVI"))
fitMeasures(fit00, c("AIC","BIC","SABIC"))
fitMeasures(fit00,c("chisq","srmr","cfi","nfi","tli")) #as Kane et al., 2004

summary(fit00, fit.measures = TRUE)
## model test baseline model for null/independent model
# return the lambda (factor loadings), theta (observed error covariance matrix), psi (latent covariance matrix), and beta (latent paths) matrices.

standardizedSolution(fit00) #standardized parameter estimates

####### *** supplementary material comparison #######
anova(fit41,fit45)
anova(fit41,fit46)



################### >>>>>>>>>>>>> ** 4. SOM: d prime <<<<<<<<<<<<<<<<<<< ############################
setwd(working_dir)
datdp <- readxl::read_excel("sum_dp_sepY_alt.xlsx")
  datdp = as.data.frame(datdp)
dat01 <- datdp[1:153,-1]

####### >>> 8 indicators: descriptive & intercorrelation  ########
eight_dp_ind <- c("lt_i","lt_o","wd_i","wd_o","loc_i","loc_o","arw_i","arw_o")
dat1 = dat01[,eight_dp_ind]
descriptive_dp <- psych::describe(dat1)
writexl::write_xlsx(descriptive_dp, "descriptive_8dp.xlsx")

# visualization w/ boxplot
boxplot(dat1,ylim = c(-1.5,9.5),
        col=c("purple","orangered","purple","orangered","dodgerblue","orange","dodgerblue","orange"),
        names=c("Item Letter","Order Letter","Item Word","Order Word", "Item Location","Order Location", "Item Arrow","Order Arrow"))
axis(side=2,at=seq(-1,9,1),lwd=1)
title(ylab="d prime", line=2, cex.lab=1.2)
points(colMeans(dat1), pch=15)

# multivariate normality
library(MVN)
mardiaTest(dat1, cov = TRUE, qqplot = FALSE)

########## correlation matrix of 8 indicators ########## 
cor_dp <- Hmisc::rcorr(as.matrix(dat1), type="spearman") 
writexl::write_xlsx(cor_dp, "cor_8dp_spearman.xlsx")

####### splithalf reliability ####### 
# also 16 parcels' intercorrelation
sixteen_dp_ind = c("lt_i1","lt_i2","lt_o1","lt_o2","wd_i1","wd_i2","wd_o1","wd_o2","loc_i1","loc_i2","loc_o1", "loc_o2", "arw_i1", "arw_i2", "arw_o1", "arw_o2")
dat2 = datdp[,sixteen_dp_ind]

cor_dp_sh <- Hmisc::rcorr(as.matrix(dat2, method="spearman"))
writexl::write_xlsx(cor_dp_sh$r, "cor_8dp_spearman_splithalf.xlsx")

####### task difficulty w/ 8 d' indicators ####### 
####### rmANOVA w/ 4 tasks -- stim*I/O -- ######
# use wide-format data
Anv5 = lm(cbind(dat1$lt_i, dat1$lt_o,dat1$wd_i, dat1$wd_o, dat1$loc_i, dat1$loc_o,dat1$arw_i, dat1$arw_o)~1)
Dom = factor(c(rep("letter", 2), rep("word",2),rep("location", 2), rep("arrow",2)))
Typ = factor(rep(c("item", "order"), 4))
Anv6 = car::Anova(Anv5, idata=data.frame(Dom,Typ), idesign=~Dom*Typ)
summary(Anv6, multivariate=F) # HF = .90

####### rmANOVA w/ letter and location -- domain*I/O -- ######
# use wide-format data
Anv9 = lm(cbind(dat1$lt_i,dat1$lt_o,dat1$loc_i,dat1$loc_o)~1)
Dom = factor(c(rep("letter", 2), rep("location", 2)))
Typ = factor(rep(c("item", "order"), 2))
Anv10 = car::Anova(Anv9, idata=data.frame(Dom,Typ), idesign=~Dom*Typ)
summary(Anv10, multivariate=F)


####### >>> 16 d' indicators: descriptive & intercorrelation  ########
datdp <- readxl::read_excel("sum_dp_sepY_alt.xlsx", 1,header = TRUE)
sixteen_dp_ind = c("lt_i1","lt_i2","lt_o1","lt_o2","wd_i1","wd_i2","wd_o1","wd_o2","loc_i1","loc_i2","loc_o1", "loc_o2", "arw_i1", "arw_i2", "arw_o1", "arw_o2")
dat01 = datdp[,sixteen_dp_ind]

######## descriptives for 16 d' parcels ###########
descriptive_dp_parcel <- psych::describe(dat01)
writexl::write_xlsx(descriptive_dp_parcel, "descriptive_dp_16parcel.xlsx")

# visualize w/ boxplot
boxplot(dat01,cex.axis=0.8,yaxt="n",ylim=c(-1,9),
        col=c("purple","purple","orangered","orangered","purple","purple","orangered","orangered","dodgerblue","dodgerblue","orange","orange","dodgerblue","dodgerblue","orange","orange"),
        names=c("ItemLetter1","ItemLetter2","OrderLetter1","OrderLetter2","ItemWord1","ItemWord2","OrderWord1","OrderWord2", "ItemLocation1","ItemLocation2","OrderLocation1","OrderLocation2", "ItemArrow1","ItemArrow2","OrderArrow1","OrderArrow2"))
axis(side=2,at=seq(-1,9,1),lwd=1)
title(ylab="Parceled d'", line=2, cex.lab=1.2)
points(colMeans(dat01), pch=15)

# multivariate normality
library(MVN)
mardiaTest(dat1, cov = TRUE, qqplot = FALSE)

########## correlation matrix of 16 indicators ########## 
cor_dp <- Hmisc::rcorr(as.matrix(dat01), type="spearman") 
writexl::write_xlsx(cor_dp, "cor_16dp_spearman_splithalf.xlsx")

######## splithalf reliability for of 16 indicators #######
thirtytwo_dp_ind = names(datdp)[18:49]
cor_16dp_sh <- Hmisc::rcorr(as.matrix(datdp[,thirtytwo_dp_ind], method="spearman"))
writexl::write_xlsx(cor_16dp_sh$r, "cor_16dp_sh.xlsx")

################### >>>>>>>>>>>>> d' SEMs <<<<<<<<<<<<<< ##########
setwd(working_dir)
datdp <- readxl::read_excel("sum_dp_sepY_alt.xlsx")
  datdp = as.data.frame(datdp)
dat01 <- datdp[1:153,-1]

# 8 indicators dataframe
eight_dp_ind <- c("lt_i","lt_o","wd_i","wd_o","loc_i","loc_o","arw_i","arw_o")
dat2 = dat01[,eight_dp_ind]

################################### *** S4/model 41: 4-fac (vi-nvi-vo-nvo) w/ stimulus factors ##################################
model41 <- ' 
verbal_I =~ lt_i + wd_i 
verbal_O =~ lt_o + wd_o 
nonverbal_I =~ loc_i + arw_i 
nonverbal_O =~ loc_o + arw_o
verbal_I ~~ verbal_O
nonverbal_I ~~ nonverbal_O
verbal_I ~~ nonverbal_I
verbal_O ~~ nonverbal_O
verbal_I ~~ nonverbal_O
verbal_O ~~ nonverbal_I
letter =~ lt_i + lt_o 
word =~ wd_i + wd_o 
loc =~ loc_i + loc_o 
arrow =~ arw_i + arw_o
'
fit41 <- lavaan::sem(model41, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=T) # NOT identified for SD
eigen(inspect(fit41,"cov.lv")) #eigen value of latent covariance matrix

fitMeasures(fit41,c("chisq","rmsea","srmr","nfi","tli","cfi","AIC","BIC")) 
summary(fit41,fit.measures=T)
  # total dof = 8*7/2=28 (baseline model dof)
  # free parameter number = 30
  # dof = 6 in user model

standardizedSolution(fit41) # read path loadings from this output 
inspect(fit41,what="std")

####################### *** S5/model 44: 3-fac(vi-nvi-o) w/ stimulus factors #######################################################
model44 <- ' 
verbal_I =~ lt_i + wd_i 
Order =~ lt_o + wd_o + loc_o + arw_o 
nonverbal_I =~  loc_i + arw_i 
verbal_I ~~ Order
nonverbal_I ~~ Order
verbal_I ~~ nonverbal_I
letter =~ lt_i + lt_o 
word =~ wd_i + wd_o 
loc =~ loc_i + loc_o 
arrow =~ arw_i + arw_o
'
fit44 <- sem(model44, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=TRUE) # not identified for SD
summary(fit44,fit.measures=T)
standardizedSolution(fit44)
fitMeasures(fit44,c("chisq","rmsea","srmr","nfi","tli","cfi","AIC","BIC")) 

####################### *** S3/model 45: 2-fac (v+nv) w/ stimulus factors #######################################################
model45 <- ' 
verbal =~ lt_i + wd_i + lt_o + wd_o 
nonverbal =~ loc_i + arw_i + loc_o + arw_o 
verbal ~~ nonverbal
letter =~ lt_i + lt_o 
word =~ wd_i + wd_o 
loc =~ loc_i + loc_o 
arrow =~ arw_i + arw_o
'
fit45 <- sem(model45, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=TRUE) # not identified for SD
fitMeasures(fit45,c("chisq","rmsea","srmr","nfi","tli","cfi","AIC","BIC")) 
summary(fit45,fit.measures=T)
standardizedSolution(fit45)


################################### *** S6/model 6: new 3fac (v+nvi+nvo) w/ stimulus factors v & nvi & nvo (model4) ##################################
model6 <- ' 
verbal =~ lt_i + wd_i + lt_o + wd_o 
nonverbal_I =~ loc_i + arw_i 
nonverbal_O =~ loc_o + arw_o 
letter =~ lt_i + lt_o 
word =~ wd_i + wd_o 
loc =~ loc_i + loc_o 
arrow =~ arw_i + arw_o
nonverbal_I ~~ nonverbal_O
verbal ~~ nonverbal_I
verbal ~~ nonverbal_O
'
fit6 <- sem(model6, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=T) # not identified for SD
summary(fit6, fit.measures = TRUE)
fitMeasures(fit6,c("chisq","rmsea","srmr","nfi","tli","cfi","AIC","BIC")) 
standardizedSolution(fit6)

####################### *** S7/model 7: WM 1 fac w/ stimulus factors #######################################################
model7 <- ' 
WM =~ lt_i + wd_i + lt_o + wd_o + loc_i + arw_i + loc_o + arw_o 
letter =~ lt_i + lt_o 
word =~ wd_i + wd_o 
loc =~ loc_i + loc_o 
arrow =~ arw_i + arw_o
'
fit7 <- sem(model7, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=TRUE) # not identified for SD
fitMeasures(fit7,c("chisq","rmsea","srmr","nfi","tli","cfi","AIC","BIC")) 
summary(fit7, fit.measures = TRUE)
standardizedSolution(fit7)

################################### *** S8/S1 feature model 47: model0: item-order WM (no domain) ###########################################
model47 <- '
Item =~ lt_i + wd_i + loc_i + arw_i 
Order =~ lt_o + wd_o + loc_o + arw_o 
Item ~~ Order
letter =~ lt_i + lt_o 
word =~ wd_i + wd_o 
loc =~ loc_i + loc_o 
arrow =~ arw_i + arw_o
'
fit47 <- sem(model47, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=T) #no SE computed
summary(fit47,fit.measures=T)
standardizedSolution(fit47)
fitMeasures(fit47,c("chisq","rmsea","srmr","nfi","tli","cfi","AIC","BIC")) 

##### comparison #####
# extract aic
daic <- function(pp,qq){
  fitMeasures(pp,"AIC") - fitMeasures(qq,"AIC") 
}

# a vs b
anova(fit41,fit45) # p=.53 
daic(fit41,fit45)
# b vs c
anova(fit41,fit44) # 4-fac (b) vs. 3-fac (c) vi-nvi-o, domain-specific wins 
daic(fit41,fit44)
# b vs d
anova(fit41,fit6) #  p=.63
daic(fit41,fit6)
# a vs d
anova(fit6,fit45)  # p=.30
daic(fit6,fit45)
# e vs f
anova(fit7,fit47)
daic(fit7,fit47)
# f vs b
anova(fit41,fit47)
daic(fit41,fit47)
# e vs b
anova(fit7,fit41)
daic(fit41,fit7)
