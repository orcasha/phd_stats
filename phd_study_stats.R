library('foreign')
library('tidyr')
library('nlme')
library('multcomp') #For post-hoc comparisons of the main effects.
library('phia')#When interaction terms are significant, this may be used (WITH MASSIVE CAVEATS - see paper).
data<-read.spss('/home/peter/Dropbox/phd/Results/Stats_Spreadsheet/phd_collection_all_vars_4.sav',to.data.frame=TRUE)
#OR to choose manually:  
#data<-file.choose()

data_culled<-data[-c(4,13,14,27,28),] #Remove rows (subs) 4,13,14,27,28
n=nrow(data_culled['no']) #Get number of rows
data_culled['no']<-1:n #Fix var 'no' for factor. 
#OR filter based on value between groups:
#data_culled<-data[!(data$Group=='Control' & data$DASS_D_1>=14 | data$Group=='Control' & data$DASS_A_1>=14 | data$Group=='Control' & data$DASS_S_1>=14) & !(data$Group=='Depressed' & data$DASS_D_1<=14 | data$Group=='Depressed' & data$DASS_A_1<=14 | data$Group=='Depressed' & data$DASS_S_1<=14),]

#Run chi-squared test on cross tabs (in this case, group x sex)
tbl=table(data_culled$Group, data_culled$Sex) #Table frequencies for vars.
tbl #Print table
chisq.test(tbl,simulate.p.value=TRUE,B=10000) #Perform chi-sq test on table.

#Setup group variables (easier for t-tests etc)
#con=data_culled[data_culled$Group=='Control',]
#dep=data_culled[data_culled$Group=='Depressed',]
#t.test(con['DASS_A_1'],dep['DASS_A_1'])

#OR specify grouped variables in command:
#func(data[data$Groupvar=='G1',]['var'],data[data$Groupvar=='G2',]['var'],data[data$Groupvar=='Gx',]['var'])
#eg:
#t.test(data_culled[data_culled$Group=='Control',]['DASS_A_1'],data_culled[data_culled$Group=='Depressed',]['DASS_A_1'])

#Do t-tests here

#Gather data for ACC mixed anova
#data_culled_long<-gather(data_culled,cond,value,MRI_nback_Hap_Acc:MRI_nback_Chk_Acc) #Create longform dataset
data_culled_long<-gather(data_culled,cond,value,MRI_nback_Hap_Acc:MRI_nback_Chk_Acc) #Create longform dataset
data_culled_long$no<-factor(data_culled_long$no) #NFI why the syntax needs to change. Using data['no'] leads to error.
data_culled_long$cond<-factor(data_culled_long$cond) #Make cond into factor

acc.anova<-lme(value~Group*cond,random=~1|no,data=data_culled_long) #Make ANOVA model - value explained by factors (A+B = Main effects, A*B = interactions)
anova(acc.anova)
summary(acc.anova)
acc.means<-interactionMeans(acc.anova)
plot(acc.means)

#ACC INTERACTIONS
data_culled_long$GroupCond=interaction(data_culled_long$Group,data_culled_long$cond)
acc_anova2=lme(value~GroupCond,random=~1|no,data=data_culled_long)
summary(glht(acc_anova2,linfct=mcp(GroupCond='Tukey')))

#Gather data for RT mixed anova
#data_culled_long<-gather(data_culled,cond,value,MRI_nback_Hap_RT:MRI_nback_Chk_RT) #Create longform dataset
data_culled_long<-gather(data_culled,cond,value,MRI_nback_Hap_RT:MRI_nback_Chk_RT) #Create longform dataset
data_culled_long$no<-factor(data_culled_long$no) #NFI why the syntax needs to change. Using data['no'] leads to error.
data_culled_long$cond<-factor(data_culled_long$cond) #Make cond into factor

rt.anova<-lme(value~Group*cond,random=~1|no,data=data_culled_long) #Make ANOVA model - value explained by factors (A+B = Main effects, A*B = interactions)
anova(rt.anova)
summary(rt.anova)
rt.means<-interactionMeans(rt.anova)
plot(rt.means)

#RT INTERACTIONS
data_culled_long$GroupCond=interaction(data_culled_long$Group,data_culled_long$cond)
rt_anova2=lme(value~GroupCond,random=~1|no,data=data_culled_long)
summary(glht(rt_anova2,linfct=mcp(GroupCond='Tukey')))


###Correlations###
library('psych')
data_corr=read.csv('/home/orcasha/Dropbox/phd/manuscripts_for_submission/nback-fmri/v2/betas_min52_names.csv',sep=",")
data_corr_culled<-data_corr[-c(4,13,14,27,28),]



