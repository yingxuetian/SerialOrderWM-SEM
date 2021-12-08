working_dir = 'DATA'
outlier_dir = 'sub_outlier'

##################### >>>>>>>>> 1. outlier screening <<<<<<<<<<<<<<< #######################
####### (1) fast task-level outlier detection w/ stimulus_log10_RT #######################
# intra-individual trimming trial-outliers for each subject
setwd(outlier_dir)
acrosssubj <- readxl::read_excel("stimlogtrimRT_outlier.xlsx")
psych::describe(acrosssubj)  

# 3IQR as hinge     
# across subjects, set outlier bar for Q1-3IQR and Q3+3*IQR
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

####### (2) low task-level outlier detection w/ stimulus accuracy #########
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

#### low outlier summary 
Y$subj[Y$letterAcc <=0.575]
# 11  33  50  66  69  88 123 130 134
Y$subj[Y$wordAcc <=0.575]
# 5   7  11  18  32  69  88 134
Y$subj[Y$locationAcc <=0.575]
# 14  18  33  41  49  69  88  94  96 106 130 134 135
Y$subj[Y$arrowAcc <=0.575]
# 2  11  12  14  15  18  23  25  32  33  35  41  46  55  58  64  66  88  90  96 100 103 106 108 121 130 134 135 136 138 144 145 147 159

####### subject-level outlier summary #######
      # # fast outlier
        # word: subject 48
      # # low outlier
        # letter: 11  33  50  66  69  88 123 130 134
        # word: 5   7  11  18  32  69  88 134
        # location: 14  18  33  41  49  69  88  94  96 106 130 134 135
        # arrow: 2  11  12  14  15  18  23  25  32  33  35  41  46  55  58  64  66  88  90  96 100 103 106 108 121 130 134 135 136 138 144 145 147 159

##################### >>>>>>>>> 2. descriptives <<<<<<<<<<############################
setwd(working_dir)
datacc <- gdata::read.xls("sum_acc.xlsx", header = TRUE)
dat01 <- datacc[1:153,-1]

####### (1) 8 indicators: descriptive & intercorrelation  ########
descriptive_acc <- psych::describe(dat01)

eight_ind <- c("I_letter_acc","O_letter_acc","I_word_acc","O_word_acc","I_location_acc","O_location_acc","I_arrow_acc","O_arrow_acc")
dat1 = dat01[,eight_ind]

boxplot(dat1,ylim = c(0,1),
        col=c("purple","orangered","purple","orangered","dodgerblue","orange","dodgerblue","orange"),
        names=c("Item Letter","Order Letter","Item Word","Order Word", "Item Location","Order Location", "Item Arrow","Order Arrow"))
title(ylab="Accuracy", line=2, cex.lab=1.2)
points(colMeans(dat1), pch=15)

####### task difficulty ANOVA  ###########
colMeans(dat1)
#### factorial rm-ANOVA w/ 4 tasks -- domain*I/O
# use wide-format data -- all task difficulty levels for acc 
# 2-by-2 ANOVA (I/o-by-v/nv)
Anv10 = lm(cbind(dat1$I_letter_acc,dat1$O_letter_acc,dat1$I_word_acc,dat1$O_word_acc, dat1$I_location_acc, dat1$O_location_acc, dat1$I_arrow_acc, dat1$O_arrow_acc)~1)
Dom = factor(c(rep("verbal", 4), rep("nonverbal",4)))
Typ = factor(rep(c("item", "order"), 4))
Anv11 = car::Anova(Anv10, idata=data.frame(Dom, Typ), idesign=~Dom*Typ)
summary(Anv11, multivariate=F) # F(1,152) = 87.36
# omega^2 = (SS_effect - df_effect*ES_effect/den_df_effect) / (SS_effect+ES_effect+ ES_effect/den_df_effect)
omegasquared = (.58-1*1.0157/152)/(.58+1.0157+1.0157/152) = 0.3577909

####### correlation matrix of 8 indicators ####### 
cor_acc <- Hmisc::rcorr(as.matrix(dat1), type="spearman") 

####### splithalf reliability ####### 
cor_acc_sh <- Hmisc::rcorr(as.matrix(dat01[,13:36], method="spearman"))

####### (2) 16 indicators: descriptive & intercorrelation --- SOM  ########
datacc <- readxl::read_excel("sum_acc.xlsx", 1,header = TRUE)
dat01 <- datacc[1:153,-1] 
#16 parceled accuracy dataframe
sixteen_ind = c("I_letter_acc1","I_letter_acc2","O_letter_acc1","O_letter_acc2","I_word_acc1","I_word_acc2","O_word_acc1","O_word_acc2","I_location_acc1","I_location_acc2","O_location_acc1","O_location_acc2","I_arrow_acc1","I_arrow_acc2","O_arrow_acc1","O_arrow_acc2")   
dat1 = dat01[,sixteen_ind]

####### descriptives for parceled accuracy ###########
descriptive_acc_parcel <- psych::describe(dat01[,13:36])

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

####### correlation matrix of 16 indicators ########## 
cor_acc <- Hmisc::rcorr(as.matrix(dat1), type="spearman") 

####### splithalf reliability of 16 indicators ########## 
cor_sh <- gdata::read.xls("acc_32parcel.xlsx",sheet= 1, header = T)

##################### >>>>>>>>> 3. accuracy SEMs <<<<<<<<<<<<< ####################
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

####################### *** model 1: (2fac: v + nv) w/ stimulus factors -- Model 1 #######################################################
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

fitMeasures(fit45,c("chisq","rmsea","srmr","cfi","nfi","tli","AIC","BIC")) #as Kane et al., 2004

####################### *** model 2: (4fac: vi+vo+nvi+nvo) w/ stimulus factors domain-specific order -- Model2 ##################################
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
summary(fit41, fit.measures = TRUE)

fitMeasures(fit41,c("chisq","rmsea","srmr","cfi","nfi","tli","AIC","BIC")) 

####################### *** model 4: another 3fac (v+nvi+nvo) w/ stimulus factors -- Model4 ##################################
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
fitMeasures(fit6,c("chisq","srmr","rmsea","cfi","nfi","tli","AIC","BIC")) 
standardizedSolution(fit6) #read loadings from this output

# latent var correlation CI
fit6 <- sem(model6, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T, std.lv=T)
parameterEstimates(fit6, ci = TRUE, level = 0.95) # unstandardized parameter estimates

####################### *** model 3:  (3fac domain-general: vi+nvi+o) w/ stimulus factors domain-general order --- model3 #######################################################
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

fitMeasures(fit44,c("chisq","srmr","cfi","nfi","tli","AIC","BIC")) 

# latent var correlation CI
fit44 <- sem(model44, data = dat1,estimator="ML",likelihood = "wishart",orthogonal=T, std.lv=T)
parameterEstimates(fit44, ci = TRUE, level = 0.95) # unstandardized parameter estimates

####################### *** comparison #######
anova(fit41,fit45)
anova(fit41,fit6)
anova(fit41,fit44)
anova(fit6,fit44)

####################### *** S1 feature model: model0: item vs. order WM (no domain) --- SOM ###########################################
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

####################### *** model S0: one factor WM -- not converged --- SOM ###########################################
model00 <- '
WM =~ I_letter_acc1+I_letter_acc2 + I_word_acc1 + I_word_acc2+ I_location_acc1 +I_location_acc2+ I_arrow_acc1 +I_arrow_acc2+ O_letter_acc1 +O_letter_acc2+  O_word_acc1 + O_word_acc2 + O_location_acc1 + O_location_acc2 + O_arrow_acc1+O_arrow_acc2
letter =~ I_letter_acc1 + I_letter_acc2 + O_letter_acc1 + O_letter_acc2
word =~ I_word_acc1 + I_word_acc2 + O_word_acc1 + O_word_acc2 
loc =~ I_location_acc1 + I_location_acc2 + O_location_acc1 + O_location_acc2
arrow =~ I_arrow_acc1 + I_arrow_acc2 + O_arrow_acc1 + O_arrow_acc2 
'
fit00 <- sem(model00, data = dat1,estimator="ML",likelihood = "wishart") 
fitMeasures(fit00,c("chisq","srmr","cfi","nfi","tli")) #as Kane et al., 2004
standardizedSolution(fit00) #standardized parameter estimates

####################### *** model comparison --- SOM #######
anova(fit41,fit45)
anova(fit41,fit46)

##################### >>>>>>>>> 4. SOM: d prime <<<<<<<<<<<<<<<<<<< ############################
setwd(working_dir)
datdp <- readxl::read_excel("sum_dp_sepY_alt.xlsx")
  datdp = as.data.frame(datdp)
dat01 <- datdp[1:153,-1]

####### (1) 8 indicators: descriptive & intercorrelation  ########
eight_dp_ind <- c("lt_i","lt_o","wd_i","wd_o","loc_i","loc_o","arw_i","arw_o")
dat1 = dat01[,eight_dp_ind]
descriptive_dp <- psych::describe(dat1)

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

####### correlation matrix of 8 indicators ########## 
cor_dp <- Hmisc::rcorr(as.matrix(dat1), type="spearman") 

####### splithalf reliability ####### 
# also 16 parcels' intercorrelation
sixteen_dp_ind = c("lt_i1","lt_i2","lt_o1","lt_o2","wd_i1","wd_i2","wd_o1","wd_o2","loc_i1","loc_i2","loc_o1", "loc_o2", "arw_i1", "arw_i2", "arw_o1", "arw_o2")
dat2 = datdp[,sixteen_dp_ind]

cor_dp_sh <- Hmisc::rcorr(as.matrix(dat2, method="spearman"))

####### task difficulty w/ 8 d' indicators ####### 
####### rmANOVA w/ 4 tasks -- stim*I/O
# use wide-format data
Anv5 = lm(cbind(dat1$lt_i, dat1$lt_o,dat1$wd_i, dat1$wd_o, dat1$loc_i, dat1$loc_o,dat1$arw_i, dat1$arw_o)~1)
Dom = factor(c(rep("letter", 2), rep("word",2),rep("location", 2), rep("arrow",2)))
Typ = factor(rep(c("item", "order"), 4))
Anv6 = car::Anova(Anv5, idata=data.frame(Dom,Typ), idesign=~Dom*Typ)
summary(Anv6, multivariate=F) # HF = .90

####### rmANOVA w/ letter and location -- domain*I/O
# use wide-format data
Anv9 = lm(cbind(dat1$lt_i,dat1$lt_o,dat1$loc_i,dat1$loc_o)~1)
Dom = factor(c(rep("letter", 2), rep("location", 2)))
Typ = factor(rep(c("item", "order"), 2))
Anv10 = car::Anova(Anv9, idata=data.frame(Dom,Typ), idesign=~Dom*Typ)
summary(Anv10, multivariate=F)


####### (2) 16 d' indicators: descriptive & intercorrelation  ########
datdp <- readxl::read_excel("sum_dp_sepY_alt.xlsx", 1,header = TRUE)
sixteen_dp_ind = c("lt_i1","lt_i2","lt_o1","lt_o2","wd_i1","wd_i2","wd_o1","wd_o2","loc_i1","loc_i2","loc_o1", "loc_o2", "arw_i1", "arw_i2", "arw_o1", "arw_o2")
dat01 = datdp[,sixteen_dp_ind]

####### descriptives for 16 d' parcels ###########
descriptive_dp_parcel <- psych::describe(dat01)

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

####### correlation matrix of 16 indicators ########## 
cor_dp <- Hmisc::rcorr(as.matrix(dat01), type="spearman") 

####### splithalf reliability for of 16 indicators #######
thirtytwo_dp_ind = names(datdp)[18:49]
cor_16dp_sh <- Hmisc::rcorr(as.matrix(datdp[,thirtytwo_dp_ind], method="spearman"))

####### (3) d' SEMs <<<<<<<<<<<<<< ##########
setwd(working_dir)
datdp <- readxl::read_excel("sum_dp_sepY_alt.xlsx")
  datdp = as.data.frame(datdp)
dat01 <- datdp[1:153,-1]

# 8 indicators dataframe
eight_dp_ind <- c("lt_i","lt_o","wd_i","wd_o","loc_i","loc_o","arw_i","arw_o")
dat2 = dat01[,eight_dp_ind]

####################### *** S3: 2-fac (v+nv) w/ stimulus factors -- analogous to model 1 #######################################################
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

####################### *** S4: 4-fac (vi-nvi-vo-nvo) w/ stimulus factors -- analogous to model 2  ##################################
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

####################### *** S5: 3-fac(vi-nvi-o) w/ stimulus factors -- analogous to model 3  #######################################################
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

####################### *** S6: new 3fac (v+nvi+nvo) w/ stimulus factors v & nvi & nvo -- analogous to model 4 ##################################
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

####################### *** S7: WM 1 fac w/ stimulus factors -- analogous to model S0 #######################################################
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

####################### *** S8 feature model: model0: item-order WM (no domain) -- analogous to model S1 ###########################################
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

####################### *** comparison #####
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

##################### >>>>>>>>> 5. SOM: logRT descriptives <<<<<<<<<<<<<<<<<<< ############################
### ** descriptive for SOM
setwd(working_dir)
dat = as.data.frame(readxl::read_excel("sum_logtrimcorRT.xlsx"))
descriptive_logrt <- psych::describe(dat[1:153,-1])

# reliability
rt_r = c(cor(dat$i_lt_1,dat$i_lt_2,method="spearman"),
          cor(dat$o_lt_1,dat$o_lt_2,method="spearman"),
          cor(dat$y_lt_1,dat$y_lt_2,method="spearman"),
          cor(dat$i_wd_1,dat$i_wd_2,method="spearman"),
          cor(dat$o_wd_1,dat$o_wd_2,method="spearman"),
          cor(dat$y_wd_1,dat$y_wd_2,method="spearman"),
          cor(dat$i_loc_1,dat$i_loc_2,method="spearman"),
          cor(dat$o_loc_1,dat$o_loc_2,method="spearman"),
          cor(dat$y_loc_1,dat$y_loc_2,method="spearman",use = "complete.obs"), #sub 106 has only 1 valid yes trial
          cor(dat$i_arw_1,dat$i_arw_2,method="spearman"),
          cor(dat$o_arw_1,dat$o_arw_2,method="spearman"),
          cor(dat$y_arw_1,dat$y_arw_2,method="spearman"))
rt_r_rel = 2*rt_r/(1+rt_r)

##################### >>>>>>>>> 6. SOM: scale-based analysis <<<<<<<<<<<<<<<<<<< ############################
setwd(working_dir)
datacc <- gdata::read.xls("sum_acc.xlsx", header = TRUE)
dat01 <- datacc[1:153,-1]

eight_ind <- c("I_letter_acc","O_letter_acc","I_word_acc","O_word_acc","I_location_acc","O_location_acc","I_arrow_acc","O_arrow_acc")
dat2 = dat01[,eight_ind]

####################### set up measurement error adjustment #########
# Cole and Preacher 2014: loading = 1; error var = (1-rel)*var
sixteen_ind = c("I_letter_acc1","I_letter_acc2","O_letter_acc1","O_letter_acc2","I_word_acc1","I_word_acc2","O_word_acc1","O_word_acc2","I_location_acc1","I_location_acc2","O_location_acc1","O_location_acc2","I_arrow_acc1","I_arrow_acc2","O_arrow_acc1","O_arrow_acc2")
cor_acc_16parcel <- Hmisc::rcorr(as.matrix(dat01[,sixteen_ind]), type="spearman")$r

sp_corr = c(cor_acc_16parcel[1,2],cor_acc_16parcel[3,4],cor_acc_16parcel[5,6],cor_acc_16parcel[7,8],cor_acc_16parcel[9,10],cor_acc_16parcel[11,12],cor_acc_16parcel[13,14],cor_acc_16parcel[15,16])
rel_adj = sp_corr*2/(1+sp_corr)
names(rel_adj) = c("I_letter","O_letter","I_word","O_word","I_location","O_location","I_arrow","O_arrow")

rel_i_lt = as.numeric(rel_adj["I_letter"])
rel_o_lt = as.numeric(rel_adj["O_letter"])
rel_i_wd = as.numeric(rel_adj["I_word"])
rel_o_wd = as.numeric(rel_adj["O_word"])
rel_i_loc = as.numeric(rel_adj["I_location"])
rel_o_loc = as.numeric(rel_adj["O_location"])
rel_i_arw = as.numeric(rel_adj["I_arrow"])
rel_o_arw = as.numeric(rel_adj["O_arrow"])

####################### *** model 1  ###########
model1_scalecole <- ' 
verbal =~ 1*I_letter_acc + 1*O_letter_acc + 1*I_word_acc + 1*O_word_acc
nonverbal =~ 1*I_location_acc + 1*O_location_acc + 1*I_arrow_acc + 1*O_arrow_acc
letter =~ 1*I_letter_acc + 1*O_letter_acc
word =~ 1*I_word_acc + 1*O_word_acc
loc =~ 1*I_location_acc + 1*O_location_acc
arrow =~ 1*I_arrow_acc + 1*O_arrow_acc
verbal ~~ nonverbal
I_letter_acc ~~ (1-rel_i_lt)*I_letter_acc
O_letter_acc ~~ (1 - rel_o_lt)*O_letter_acc
I_word_acc ~~ (1 - rel_i_wd)*I_word_acc
O_word_acc ~~ (1 - rel_o_wd)*O_word_acc
I_location_acc~~ (1 - rel_i_loc)*I_location_acc
O_location_acc~~ (1 - rel_o_loc)*O_location_acc
I_arrow_acc ~~ (1 - rel_i_arw)*I_arrow_acc
O_arrow_acc ~~ (1 - rel_o_arw)*O_arrow_acc
'
fit1_scalecole <- sem(model1_scalecole, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=TRUE) 
inspect(fit1_scalecole,what="std")
inspect(fit1_scalecole,what="est")
summary(fit1_scalecole,fit.measures=T)

####################### *** model 4  #########
model4_scalecole <- ' 
verbal =~ 1*I_letter_acc + 1*O_letter_acc + 1*I_word_acc + 1*O_word_acc
nonverbal_I =~ 1*I_location_acc  + 1*I_arrow_acc
nonverbal_O =~ 1*O_location_acc + 1*O_arrow_acc
letter =~ 1*I_letter_acc + 1*O_letter_acc
word =~ 1*I_word_acc + 1*O_word_acc
loc =~ 1*I_location_acc + 1*O_location_acc
arrow =~ 1*I_arrow_acc + 1*O_arrow_acc
nonverbal_I ~~ nonverbal_O
verbal ~~ nonverbal_I
verbal ~~ nonverbal_O
I_letter_acc ~~ (1-rel_i_lt)*I_letter_acc
O_letter_acc ~~ (1 - rel_o_lt)*O_letter_acc 
I_word_acc ~~ (1 - rel_i_wd)*I_word_acc 
O_word_acc ~~ (1 - rel_o_wd)*O_word_acc
I_location_acc~~ (1 - rel_i_loc)*I_location_acc
O_location_acc~~ (1 - rel_o_loc)*O_location_acc
I_arrow_acc ~~ (1 - rel_i_arw)*I_arrow_acc
O_arrow_acc ~~ (1 - rel_o_arw)*O_arrow_acc
'
fit4_scalecole <- sem(model4_scalecole, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=TRUE) 
summary(fit4_scalecole,fit.measures=T)

####################### *** model 2  ##########
model2_scalecole <- ' 
verbal_I =~ 1*I_letter_acc + 1* I_word_acc
verbal_O =~ 1*O_letter_acc + 1*O_word_acc
nonverbal_I =~ 1*I_location_acc  + 1 *I_arrow_acc
nonverbal_O =~ 1*O_location_acc + 1*O_arrow_acc
letter =~ 1*I_letter_acc + 1*O_letter_acc
word =~ 1*I_word_acc + 1*O_word_acc
loc =~ 1*I_location_acc + 1*O_location_acc
arrow =~ 1*I_arrow_acc + 1*O_arrow_acc
nonverbal_I ~~ nonverbal_O
verbal_I ~~ verbal_O
verbal_I ~~ nonverbal_I
verbal_O ~~ nonverbal_O
verbal_I ~~ nonverbal_O
verbal_O ~~ nonverbal_I
I_letter_acc ~~ (1-rel_i_lt)*I_letter_acc
O_letter_acc ~~ (1 - rel_o_lt)*O_letter_acc 
I_word_acc ~~ (1 - rel_i_wd)*I_word_acc 
O_word_acc ~~ (1 - rel_o_wd)*O_word_acc
I_location_acc~~ (1 - rel_i_loc)*I_location_acc
O_location_acc~~ (1 - rel_o_loc)*O_location_acc
I_arrow_acc ~~ (1 - rel_i_arw)*I_arrow_acc
O_arrow_acc ~~ (1 - rel_o_arw)*O_arrow_acc
'
fit2_scalecole <- sem(model2_scalecole, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=TRUE) 
summary(fit2_scalecole,fit.measures=T)
inspect(fit2_scalecole)
standardizedSolution(fit2_scalecole) 

####################### *** model 3  ##########
model3_scalecole <- ' 
verbal_I =~ 1*I_letter_acc +  1*I_word_acc
Order=~ 1*O_letter_acc + 1*O_word_acc + 1*O_location_acc + 1*O_arrow_acc
nonverbal_I =~ 1*I_location_acc  + 1*I_arrow_acc
letter =~ 1*I_letter_acc + 1*O_letter_acc
word =~ 1*I_word_acc + 1*O_word_acc
loc =~ 1*I_location_acc + 1*O_location_acc
arrow =~ 1*I_arrow_acc + 1*O_arrow_acc
verbal_I ~~ Order
nonverbal_I ~~ Order
verbal_I ~~ nonverbal_I
I_letter_acc ~~ (1-rel_i_lt)*I_letter_acc
O_letter_acc ~~ (1 - rel_o_lt)*O_letter_acc 
I_word_acc ~~ (1 - rel_i_wd)*I_word_acc 
O_word_acc ~~ (1 - rel_o_wd)*O_word_acc
I_location_acc~~ (1 - rel_i_loc)*I_location_acc
O_location_acc~~ (1 - rel_o_loc)*O_location_acc
I_arrow_acc ~~ (1 - rel_i_arw)*I_arrow_acc
O_arrow_acc ~~ (1 - rel_o_arw)*O_arrow_acc
'
fit3_scalecole <- sem(model3_scalecole, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=TRUE) 
summary(fit3_scalecole,fit.measures=T)

####################### *** model S1 wmc ######
models1_scalecole <- ' 
wmc =~ 1*I_letter_acc +  1*I_word_acc + 1*O_letter_acc + 1*O_word_acc + 1*O_location_acc + 1*O_arrow_acc + 1*I_location_acc  + 1*I_arrow_acc
letter =~ 1*I_letter_acc + 1*O_letter_acc
word =~ 1*I_word_acc + 1*O_word_acc
loc =~ 1*I_location_acc + 1*O_location_acc
arrow =~ 1*I_arrow_acc + 1*O_arrow_acc
I_letter_acc ~~ (1-rel_i_lt)*I_letter_acc
O_letter_acc ~~ (1 - rel_o_lt)*O_letter_acc 
I_word_acc ~~ (1 - rel_i_wd)*I_word_acc 
O_word_acc ~~ (1 - rel_o_wd)*O_word_acc
I_location_acc~~ (1 - rel_i_loc)*I_location_acc
O_location_acc~~ (1 - rel_o_loc)*O_location_acc
I_arrow_acc ~~ (1 - rel_i_arw)*I_arrow_acc
O_arrow_acc ~~ (1 - rel_o_arw)*O_arrow_acc
'
fits1_scalecole <- sem(models1_scalecole, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=TRUE) 
inspect(fits1_scalecole)

####################### *** model S2 i+o #######
models2_scalecole <- ' 
Item =~ 1*I_letter_acc +  1*I_word_acc + 1*I_location_acc  + 1*I_arrow_acc
Order =~ 1*O_letter_acc + 1*O_word_acc + 1*O_location_acc + 1*O_arrow_acc
letter =~ 1*I_letter_acc + 1*O_letter_acc
word =~ 1*I_word_acc + 1*O_word_acc
loc =~ 1*I_location_acc + 1*O_location_acc
arrow =~ 1*I_arrow_acc + 1*O_arrow_acc
Item ~~ Order
I_letter_acc ~~ (1-rel_i_lt)*I_letter_acc
O_letter_acc ~~ (1 - rel_o_lt)*O_letter_acc 
I_word_acc ~~ (1 - rel_i_wd)*I_word_acc 
O_word_acc ~~ (1 - rel_o_wd)*O_word_acc
I_location_acc~~ (1 - rel_i_loc)*I_location_acc
O_location_acc~~ (1 - rel_o_loc)*O_location_acc
I_arrow_acc ~~ (1 - rel_i_arw)*I_arrow_acc
O_arrow_acc ~~ (1 - rel_o_arw)*O_arrow_acc
'
fits2_scalecole <- sem(models2_scalecole, data = dat2,estimator="ML",likelihood = "wishart",orthogonal=TRUE) 
inspect(fits2_scalecole)

####################### *** model comparison ######
# (1) check estimates being corrected numbered (shows the free parameters being estimated)
inspect(fit1_scalecole)
inspect(fit2_scalecole)
inspect(fit3_scalecole)
inspect(fit4_scalecole)
# (2) model comparison
anova(fit1_scalecole,fit2_scalecole)
# 1~2
anova(fit2_scalecole,fit3_scalecole)
# 2 better than 3
anova(fit2_scalecole,fit4_scalecole)
# wmc vs. i-o
anova(fits2_scalecole,fits1_scalecole)
# vi-vo-nvi-nvo better than i-o
anova(fits2_scalecole,fit2_scalecole)

# when sig, larger model preferred
# smaller AIC preferred

# (3) for fig and table: fit indices, factor corr
fitind_scalecole = rbind(lavaan::lavInspect(fit1_scalecole,"fit")[c('chisq','df',"pvalue","rmsea","srmr","nfi","nnfi","cfi","aic","bic")],
lavaan::lavInspect(fit2_scalecole,"fit")[c('chisq','df',"pvalue","rmsea","srmr","nfi","nnfi","cfi","aic","bic")],
lavaan::lavInspect(fit3_scalecole,"fit")[c('chisq','df',"pvalue","rmsea","srmr","nfi","nnfi","cfi","aic","bic")],
lavaan::lavInspect(fit4_scalecole,"fit")[c('chisq','df',"pvalue","rmsea","srmr","nfi","nnfi","cfi","aic","bic")],
lavaan::lavInspect(fits1_scalecole,"fit")[c('chisq','df',"pvalue","rmsea","srmr","nfi","nnfi","cfi","aic","bic")],
lavaan::lavInspect(fits2_scalecole,"fit")[c('chisq','df',"pvalue","rmsea","srmr","nfi","nnfi","cfi","aic","bic")])
fit_final_colescale = cbind.data.frame(c(1:4,"s1","s3"),fitind_scalecole)
names(fit_final_colescale)[1]="model"

# lat factor cor
inspect(fit1_scalecole,what="std")
inspect(fit2_scalecole,what="std")
inspect(fit3_scalecole,what="std")
inspect(fit4_scalecole,what="std")
inspect(fits1_scalecole,what="std")
inspect(fits2_scalecole,what="std")
