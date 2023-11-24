## Load libraries
library(dplyr)
library(openxlsx)
library(truncreg)
library(VGAM)

## Read and process dataset
scdata <- read.xlsx(
  xlsxFile = "data/Giri Dataset.xlsx",
  sheet = 1,
  detectDates = TRUE
)

#scdata$inf_prior_vacc <-as.factor(scdata$inf_prior_vacc)
#scdata$bmi_class <-as.factor(scdata$bmi_class)
#scdata$vacc_status <-as.factor(scdata$vacc_status)
#scdata$comorbid <-as.factor(scdata$comorbid)
#scdata$time_after_vaccine <-as.factor(scdata$time_after_vaccine)



# scdata <- scdata%>%
#   select(-Name,-Surname)

x1<-scdata%>%
  select(D:N28Results)%>%
  mutate(time_after_vaccine="month 1")%>%
  rename(
    id=D, 
    #school=School,
    sex=Gender,
    comorbid=BirthComor,
    bmi=BMI.measure,
    bmi_class=BMI,
    vacc_status=Vacstat,
    vacc_date1=Date.1,
    vacc_date2=Date.2,
    age=Age,
    class=Class,
    inf_prior_vacc=Infected.prior.to.vaccination,
    ab_after_vacc=Quantivac.at.28.days,
    ab_after_vacc_class=Q.28.Result,
    ab_after_vacc_od=ODQ28Days,
    ab_after_inf=NCPat28.days,
    ab_after_inf_class=N28Results
  )

x2<-scdata%>%
  select(c(D:Infected.prior.to.vaccination,Quantivacat3.months:N3Result))%>%
  mutate(time_after_vaccine="month 4")%>%
  rename(
    id=D, 
    #school=School,
    sex=Gender,
    comorbid=BirthComor,
    bmi=BMI.measure,
    bmi_class=BMI,
    vacc_status=Vacstat,
    vacc_date1=Date.1,
    vacc_date2=Date.2,
    age=Age,
    class=Class,
    inf_prior_vacc=Infected.prior.to.vaccination,
    ab_after_vacc=Quantivacat3.months,
    ab_after_vacc_class=Q3.Result,
    ab_after_vacc_od=ODQ4months,
    ab_after_inf=NCPat3months,
    ab_after_inf_class=N3Result
  )

x3<-scdata%>%
  select(c(D:Infected.prior.to.vaccination,Quantivacat7months:N7Result))%>%
  mutate(time_after_vaccine="month 7")%>%
  rename(
    id=D, 
    #school=School,
    sex=Gender,
    comorbid=BirthComor,
    bmi=BMI.measure,
    bmi_class=BMI,
    vacc_status=Vacstat,
    vacc_date1=Date.1,
    vacc_date2=Date.2,
    age=Age,
    class=Class,
    inf_prior_vacc=Infected.prior.to.vaccination,
    ab_after_vacc=Quantivacat7months,
    ab_after_vacc_class=Q7Result,
    ab_after_vacc_od=ODQ7months,
    ab_after_inf=NCPat7months,
    ab_after_inf_class=N7Result 
  )

scdata <- rbind(x1,x2,x3)

vacc_data<-scdata%>%
  filter(vacc_status=="Vaccinated")

unvacc_data<-scdata%>%
  filter(vacc_status=="Non Vaccinated")

## Create summary description
scdata%>%
  group_by(vacc_status)%>%
  summarise(
    mean_age=mean(age,na.rm=TRUE),
    sd_age=sd(age,na.rm = TRUE),
    mean_bmi=mean(bmi,na.rm=TRUE),
    sd_bmi=sd(bmi,na.rm=TRUE)
  )

scdata%>% 
  filter(time_after_vaccine=="month 1")%>%
  count(vacc_status,comorbid)%>%
  mutate(
    total_vacc=nrow(scdata[scdata$vacc_status=="Vaccinated"&scdata$time_after_vaccine=="month 1",]),
    total_unvacc=nrow(scdata[scdata$vacc_status=="Non Vaccinated"&scdata$time_after_vaccine=="month 1",]),
    prop_comorbid_vacc=n/total_vacc*100,
    prop_comorbid_unvacc=n/total_unvacc*100
  )


scdata%>% 
  filter(time_after_vaccine=="month 1")%>%
  count(vacc_status,inf_prior_vacc)%>%
  mutate(
    total_vacc=nrow(scdata[scdata$vacc_status=="Vaccinated"&scdata$time_after_vaccine=="month 1",]),
    total_unvacc=nrow(scdata[scdata$vacc_status=="Non Vaccinated"&scdata$time_after_vaccine=="month 1",]),
    prop_inf_vacc=n/total_vacc*100,
    prop_inf_unvacc=n/total_unvacc*100
  )


###Mean Ab after vacc optical density
mean_vacc_od<-mean(vacc_data$ab_after_vacc_od)
sd_vacc_od<-sd(vacc_data$ab_after_vacc_od)
ci_vacc_od<- c(mean_vacc_od-1.96*sd_vacc_od,mean_vacc_od+1.96*sd_vacc_od)

mean_vacc_od_df<-scdata%>%
  group_by(vacc_status,time_after_vaccine)%>%
  summarise(
    mean_od=mean(ab_after_vacc_od),
    sd_od=sd(ab_after_vacc_od),
    mean_lcl=mean_od-1.96*sd_od,
    mean_ucl=mean_od+1.96*sd_od
  )

boxplot(ab_after_vacc_od~vacc_status+time_after_vaccine,data=scdata)

png(filename = "output/boxplot_ab.png", width = 10,height = 8,units = "in",res = 150,pointsize = 16)
par(mfrow=c(1,3))
boxplot(
  ab_after_vacc_od~vacc_status,
  data=scdata[scdata$time_after_vaccine=="month 1",],
  ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 1"
)
abline(h=3.5,lty=2,lwd=2,col="red")
text(x=1.5,y=3.7,labels="censored point",col="red")
axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))


boxplot(
  ab_after_vacc_od~vacc_status,
  data=scdata[scdata$time_after_vaccine=="month 4",],
  ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 4"
)
abline(h=3.5,lty=2,lwd=2,col="red")
text(x=1.5,y=3.7,labels="censored point",col="red")
axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))

boxplot(
  ab_after_vacc_od~vacc_status,
  data=scdata[scdata$time_after_vaccine=="month 7",],
  ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 7"
)
abline(h=3.5,lty=2,lwd=2,col="red")
text(x=1.5,y=3.7,labels="censored point",col="red")
axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))
dev.off()



##Statistical test for mean difference of outcome measure 

mean_m1<-t.test(ab_after_vacc_od~vacc_status,data=scdata[scdata$time_after_vaccine=="month 1",])
r1<-c(mean_m1$estimate[2],mean_m1$estimate[1],mean_m1$estimate[2]-mean_m1$estimate[1],mean_m1$conf.int,mean_m1$p.value)

mean_m2<-t.test(ab_after_vacc_od~vacc_status,data=scdata[scdata$time_after_vaccine=="month 4",])
r2<-c(mean_m2$estimate[2],mean_m2$estimate[1],mean_m2$estimate[2]-mean_m2$estimate[1],mean_m2$conf.int,mean_m2$p.value)

mean_m3<-t.test(ab_after_vacc_od~vacc_status,data=scdata[scdata$time_after_vaccine=="month 7",])
r3<-c(mean_m3$estimate[2],mean_m3$estimate[1],mean_m3$estimate[2]-mean_m3$estimate[1],mean_m3$conf.int,mean_m3$p.value)

mean_diff_od<- data.frame(rbind(r1,r2,r3))
names(mean_diff_od)<-c("mean vaccinated","mean unvaccinated", "mean difference", "95% LCL", "95% UCL", "p-value")
mean_diff_od_pretty <-data.frame(round(mean_diff_od[,1:5],digits = 2),round(mean_diff_od[,6],digits = 12))
 

####Compare ab optical density over time for vaccinted
png(filename = "output/boxplot_ab_vacc.png", width = 10,height = 8,units = "in",res = 150,pointsize = 16)
boxplot(ab_after_vacc_od~time_after_vaccine,data = vacc_data,xlab=NULL,ylab = "Optical Density of S1 Ab",ylim=c(0,4.0))
abline(h=3.5,lty=2,lwd=2,col="red")
text(x=1.5,y=3.7,labels="censored point",col="red")
axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))
dev.off()


mean_vacc_m1<-t.test(ab_after_vacc_od~time_after_vaccine,data=vacc_data[vacc_data$time_after_vaccine%in%c("month 1", "month 4"),], alternative ="less", paired=TRUE)
vacc_r1<-c(mean_vacc_m1$estimate[2],mean_vacc_m1$estimate[1],mean_vacc_m1$estimate[2]-mean_vacc_m1$estimate[1],mean_vacc_m1$conf.int,mean_vacc_m1$p.value)

mean_vacc_m2<-t.test(ab_after_vacc_od~time_after_vaccine,data=vacc_data[vacc_data$time_after_vaccine%in%c("month 1", "month 7"),])
vacc_r2<-c(mean_vacc_m2$estimate[2],mean_vacc_m2$estimate[1],mean_vacc_m2$estimate[2]-mean_vacc_m2$estimate[1],mean_vacc_m2$conf.int,mean_vacc_m2$p.value)

mean_vacc_m3<-t.test(ab_after_vacc_od~time_after_vaccine,data=vacc_data[vacc_data$time_after_vaccine%in%c("month 4", "month 7"),],alternative ="less", paired=TRUE)
vacc_r3<-c(mean_vacc_m3$estimate[2],mean_vacc_m3$estimate[1],mean_vacc_m3$estimate[2]-mean_vacc_m3$estimate[1],mean_vacc_m3$conf.int,mean_vacc_m3$p.value)

mean_diff_vacc_od<-data.frame(comparison=c("month 4 - month 1", "month 7 - month 1", "month 7 - month 4"),rbind(vacc_r1,vacc_r2,vacc_r3))

names(mean_diff_vacc_od)<-c("comparison","mean first time point","mean second time point", "mean difference", "95% LCL", "95% UCL", "p-value")
mean_diff_vacc_od_pretty <-data.frame(mean_diff_vacc_od[,"comparison"],round(mean_diff_vacc_od[,2:5],digits = 2),round(mean_diff_vacc_od[,6],digits = 12))

###Boxplot vaccinated vs unvaccinated by infection status

png(filename = "output/boxplot_ab_inf.png", width = 10,height = 8,units = "in",res = 150,pointsize = 16)
par(mfrow=c(1,3))
boxplot(
  ab_after_vacc_od~ab_after_inf_class,
  data=vacc_data[vacc_data$time_after_vaccine=="month 1",],
  ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 1",names=c("negtaive","positive")
)
abline(h=3.5,lty=2,lwd=2,col="red")
text(x=1.5,y=3.7,labels="censored point",col="red")
axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))

boxplot(
  ab_after_vacc_od~ab_after_inf_class,
  data=vacc_data[vacc_data$time_after_vaccine=="month 4",],
  ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 4",names=c("negtaive","positive")
)
abline(h=3.5,lty=2,lwd=2,col="red")
text(x=1.5,y=3.7,labels="censored point",col="red")
axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))

boxplot(
  ab_after_vacc_od~ab_after_inf_class,
  data=vacc_data[vacc_data$time_after_vaccine=="month 7",],
  ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 7",names=c("negtaive","positive")
)
abline(h=3.5,lty=2,lwd=2,col="red")
text(x=1.5,y=3.7,labels="censored point",col="red")
axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))
dev.off()



# ###Boxplot unvaccinated by infection status
# 
# png(filename = "output/boxplot_ab_unvacc.png", width = 10,height = 8,units = "in",res = 150,pointsize = 16)
# par(mfrow=c(1,3))
# boxplot(
#   ab_after_vacc_od~ab_after_inf_class,
#   data=unvacc_data[unvacc_data$time_after_vaccine=="month 1",],
#   ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 1",names=c("negtaive","positive")
# )
# abline(h=3.5,lty=2,lwd=2,col="red")
# text(x=1.5,y=3.7,labels="truncation point",col="red")
# axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))
# 
# boxplot(
#   ab_after_vacc_od~ab_after_inf_class,
#   data=unvacc_data[unvacc_data$time_after_vaccine=="month 4",],
#   ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 4",names=c("negtaive","positive")
# )
# abline(h=3.5,lty=2,lwd=2,col="red")
# text(x=1.5,y=3.7,labels="truncation point",col="red")
# axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))
# 
# boxplot(
#   ab_after_vacc_od~ab_after_inf_class,
#   data=unvacc_data[unvacc_data$time_after_vaccine=="month 7",],
#   ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 7",names=c("negtaive","positive")
# )
# abline(h=3.5,lty=2,lwd=2,col="red")
# text(x=1.5,y=3.7,labels="truncation point",col="red")
# axis(side = 2,at=seq(from=0,to=4.0, by=0.5),labels =seq(from=0,to=4.0, by=0.5))
# dev.off()




## Regression analysis
##vacc_data<-scdata%>%
##filter(vacc_status=="Vaccinated")
 
vacc_data$inf_prior_vacc <-as.factor(vacc_data$inf_prior_vacc)
vacc_data$bmi_class <-as.factor(vacc_data$bmi_class)
vacc_data$vacc_status <-as.factor(vacc_data$vacc_status)
vacc_data$comorbid <-as.factor(vacc_data$comorbid)
vacc_data$time_after_vaccine <-as.factor(vacc_data$time_after_vaccine)

# # ## m <- truncreg(ab_after_vacc_od~ age + factor(time_after_vaccine),data = vacc_data, 
# #               point = 4.0, 
# #               direction = "right"
# # )
# 
# summary(m)



##Univariable vaccinated

m1 <- vglm(ab_after_vacc_od ~ age, tobit(Upper =3.5), data = vacc_data)
summary (m1)
b_age <- coef(m1)
se_age <- sqrt(diag(vcov(m1)))
ci_age<-cbind(LL = b_age - qnorm(0.975) * se_age, UL = b_age+ qnorm(0.975) * se_age)
ci_age


m2 <- vglm(ab_after_vacc_od ~ sex, tobit(Upper =3.5), data = vacc_data)
summary(m2)
b_sex <- coef(m2)
se_sex <- sqrt(diag(vcov(m2)))
ci_sex<-cbind(LL = b_sex - qnorm(0.975) * se_sex, UL = b_sex + qnorm(0.975) * se_sex)
ci_sex



m3 <- vglm(ab_after_vacc_od ~ bmi, tobit(Upper =3.5), data = vacc_data)
summary(m3)
b_bmi <- coef(m3)
se_bmi <- sqrt(diag(vcov(m3)))
ci_bmi<-cbind(LL = b_bmi - qnorm(0.975) * se_bmi, UL = b_bmi + qnorm(0.975) * se_bmi)
ci_bmi

m4 <- vglm(ab_after_vacc_od ~ inf_prior_vacc, tobit(Upper =3.5), data = vacc_data)
summary(m4)
b_inf_prior_vacc <- coef(m4)
se_inf_prior_vacc <- sqrt(diag(vcov(m4)))
ci_inf_prior_vacc<-cbind(LL = b_inf_prior_vacc - qnorm(0.975) * se_inf_prior_vacc, UL = b_inf_prior_vacc + qnorm(0.975) * se_inf_prior_vacc)
ci_inf_prior_vacc


m5 <- vglm(ab_after_vacc_od ~ time_after_vaccine, tobit(Upper =3.5), data = vacc_data)
summary(m5)
b_time_after_vacc <- coef(m5)
se_time_after_vacc <- sqrt(diag(vcov(m5)))
ci_time_after_vacc <-cbind(LL = b_time_after_vacc  - qnorm(0.975) * se_time_after_vacc , UL = b_time_after_vacc  + qnorm(0.975) * se_time_after_vacc)
ci_time_after_vacc 

m6 <- vglm(ab_after_vacc_od ~ comorbid, tobit(Upper =3.5), data = vacc_data)
summary(m6)
b_comorbid <- coef(m6)
se_comorbid <- sqrt(diag(vcov(m6)))
ci_comorbid<-cbind(LL = b_comorbid - qnorm(0.975) * se_comorbid, UL = b_comorbid + qnorm(0.975) * se_comorbid)
ci_comorbid

m7 <- vglm(ab_after_vacc_od ~ ab_after_inf_class, tobit(Upper =3.5), data = vacc_data)
summary(m7)
b_ab_after_inf_class <- coef(m7)
se_ab_after_inf_class <- sqrt(diag(vcov(m7)))
ci_ab_after_inf_class<-cbind(LL = b_ab_after_inf_class - qnorm(0.975) * se_ab_after_inf_class, UL = b_ab_after_inf_class + qnorm(0.975) * se_ab_after_inf_class)
ci_ab_after_inf_class



m8 <- vglm(ab_after_vacc_od ~ ab_after_inf_class +time_after_vaccine, tobit(Upper =3.5), data = vacc_data)
b_inf_time <- coef(m8)
se_inf_time <- sqrt(diag(vcov(m8)))
ci_inf_time<-cbind(LL = b_inf_time - qnorm(0.975) * se_inf_time, UL = b_inf_time + qnorm(0.975) * se_inf_time)
ci_inf_time


##Multivariable vaccinated

multi_vac <- vglm(ab_after_vacc_od ~ age+
                    sex+ab_after_inf_class+
                    bmi+
                    time_after_vaccine + 
                    comorbid,tobit(Upper =3.5), data = vacc_data)
summary(multi_vac)
b_multi_vac<- coef(multi_vac)
se_multi_vac <- sqrt(diag(vcov(multi_vac)))
ci_multi_vac<-cbind(LL = b_multi_vac - qnorm(0.975) * se_multi_vac, UL = b_multi_vac + qnorm(0.975) * se_multi_vac)
ci_multi_vac

### Regression plots

yhat <- fitted(multi_vac)[,1]
rr <- resid(multi_vac, type = "response")
rp <- resid(multi_vac, type = "pearson")[,1]


  plot(yhat, rr, main = "Fitted vs Residuals")
  qqnorm(rr)
  plot(yhat, rp, main = "Fitted vs Pearson Residuals")
  qqnorm(rp)
 
  plot(yhat, rr, main = "Fitted vs Residuals")
  qqnorm(rr)
  plot(yhat, rp, main = "Fitted vs Pearson Residuals")
  qqnorm(rp)
   
  qqnorm(yhat)
  qqline(yhat)
  

unvacc_data$inf_prior_vacc <-as.factor(unvacc_data$inf_prior_vacc)
unvacc_data$bmi_class <-as.factor(unvacc_data$bmi_class)
unvacc_data$vacc_status <-as.factor(unvacc_data$vacc_status)
unvacc_data$comorbid <-as.factor(unvacc_data$comorbid)
unvacc_data$time_after_vaccine <-as.factor(unvacc_data$time_after_vaccine)

# ##Univarible unvaccinated
# summary(muv1 <- vglm(ab_after_vacc_od ~ age, tobit(Upper =3.5), data = unvacc_data))
# 
# 
# summary(muv2 <- vglm(ab_after_vacc_od ~ bmi, tobit(Upper =3.5), data = unvacc_data))
# 
# summary(muv3 <- vglm(ab_after_vacc_od ~ comorbid, tobit(Upper =3.5), data = unvacc_data))
# 
# summary(muv4 <- vglm(ab_after_vacc_od ~ inf_prior_vacc, tobit(Upper =3.5), data = unvacc_data))
# 
# summary(muv5 <- vglm(ab_after_vacc_od ~ time_after_vaccine, tobit(Upper =3.5), data = unvacc_data))
# 
# summary(muv6 <- vglm(ab_after_vacc_od ~ ab_after_inf_class, tobit(Upper =3.5), data = unvacc_data))
# 
# summary(muv7 <- vglm(ab_after_vacc_od ~ ab_after_inf_class+ time_after_vaccine, tobit(Upper =3.5), data = unvacc_data))
# 
# 
# ###Multivarible regression Unvaccinated
# summary(multiuv1 <- vglm(ab_after_vacc_od ~ age+sex+bmi+ comorbid,tobit(Upper =3.5), data = unvacc_data))
# 
# summary(multuv2 <- vglm(ab_after_vacc_od ~ age+sex+bmi+relevel(inf_prior_vacc, ref ="N") +time_after_vaccine + comorbid,tobit(Upper =3.5), data = unvacc_data))
# 
# 
# 
# 
# 
# #####others
# ###Interaction
# 
# 
# 
# summary(m1 <- vglm(ab_after_vacc_od ~ ab_after_inf, tobit(Upper =3.5), data = vacc_data))
# 
# 
# 
# summary(multi_vac <- vglm(ab_after_vacc_od ~ age+sex+bmi_class+relevel(inf_prior_vacc, ref ="N") +time_after_vaccine + relevel(comorbid,ref="N"), tobit(Upper =3.5), data = vacc_data))
# 
# summary(multi_vac <- vglm(ab_after_vacc_od ~ age+sex+bmi_class+relevel(inf_prior_vacc, ref ="N") +time_after_vaccine + relevel(comorbid,ref="N"),+ab_after_inf_class, tobit(Upper =3.5), data = vacc_data))
# 
# summary(m1 <- vglm(ab_after_vacc_od ~ ab_after_inf_class+time_after_vaccine, tobit(Upper =3.5), data = vacc_data))
# 
# 
# 
# summary(muv6 <- vglm(ab_after_vacc_od ~ time_after_vaccine+ab_after_inf_class, tobit(Upper =3.5), data = unvacc_data))
# 
# 
# 
# 
# 
# # ######testing
# # boxplot(ab_after_vacc_od~vacc_status+ab_after_inf_class+time_after_vaccine,data=vacc_data)
# # table(vacc_data$ab_after_inf_class,vacc_data$time_after_vaccine)
# # boxplot(ab_after_vacc_od~vacc_status+ab_after_inf_class+time_after_vaccine,data=unvacc_data)
# # 
# # png(filename = "output/boxplot_ab.png", width = 10,height = 8,units = "in",res = 150,pointsize = 16)
# # par(mfrow=c(1,3))
# # boxplot(
# #   ab_after_vacc_od~vacc_status,
# #   data=scdata[scdata$time_after_vaccine=="month 1",],
# #   ylim=c(0,4.0),ylab = "Optical Density of S1 Ab",xlab = "Month 1"
# 
# 



# ####wilcoxson test
# mean_wx1 <- wilcox.test(
#   ab_after_vacc_od ~ vacc_status,
#   data = scdata[scdata$time_after_vaccine=="month 1",],
#   conf.int = TRUE
# )
# 
# wx1 <- c(
#   mean(scdata$ab_after_vacc_od[scdata$vacc_status == "Vaccinated"]),
#   mean(scdata$ab_after_vacc_od[scdata$vacc_status == "Non Vaccinated"]),
#   mean_wx1$estimate,
#   mean_wx1$conf.int,
#   mean_wx1$p.value
# )


###Kruskal Wallis Test

kw1 <- kruskal.test(
  ab_after_vacc_od~vacc_status,
  data=scdata[scdata$time_after_vaccine=="month 1",]
)


kw2 <- kruskal.test(
  ab_after_vacc_od~vacc_status,
  data=scdata[scdata$time_after_vaccine=="month 4",]
)


kw3 <- kruskal.test(
  ab_after_vacc_od~vacc_status,
  data=scdata[scdata$time_after_vaccine=="month 7",]
)



### Wilcoxson Test
wx1 <-wilcox.test(
  ab_after_vacc_od~time_after_vaccine,
  data=vacc_data%>% filter(time_after_vaccine %in% c("month 1", "month 4")),
  alternative="less",
  paired=TRUE,
  conf.int=TRUE
)


wx2 <-wilcox.test(
  ab_after_vacc_od~time_after_vaccine,
  data=vacc_data%>% filter(time_after_vaccine %in% c("month 1", "month 7")),
  alternative="less",
  paired=TRUE,
  conf.int=TRUE
)

wx3 <-wilcox.test(
  ab_after_vacc_od~time_after_vaccine,
  data=vacc_data%>% filter(time_after_vaccine %in% c("month 4", "month 7")),
  alternative="less",
  paired=TRUE,
  conf.int=TRUE
)


##ShapiroTest
shapiro.test(scdata$ab_after_vacc_od)



###scaterplot
plot(x=vacc_data$bmi, y=vacc_data$ab_after_vacc_od)


## kruskal wallis for vacc data
kruskal.test(
ab_after_vacc_od~ab_after_inf_class,
data=vacc_data %>%filter(time_after_vaccine== "month 1")
)



## kruskal wallis for vacc data

kruskal.test(
  ab_after_vacc_od~ab_after_inf_class,
  data=vacc_data %>%filter(time_after_vaccine== "month 1")
)




kruskal.test(
  ab_after_vacc_od~ab_after_inf_class,
  data=vacc_data %>%filter(time_after_vaccine== "month 4")
)


kruskal.test(
  ab_after_vacc_od~ab_after_inf_class,
  data=vacc_data %>%filter(time_after_vaccine== "month 7")
)



######testing
xxy <- wilcox.test(
  ab_after_vacc_od~vacc_status,
  data=scdata[scdata$time_after_vaccine=="month 1",]
  )

