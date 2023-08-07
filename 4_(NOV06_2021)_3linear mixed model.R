library("lme4")
library("ggplot2")
library("pbkrtest")
library(lmerTest)
data_finish<-read.csv("data_finish.csv")
##centering
data_finish$T_EF<-data_finish$T_EF-mean(data_finish$T_EF)
data_finish$T_CR<-data_finish$T_CR-mean(data_finish$T_CR)
factor(data_finish$INT)
factor(data_finish$CRE)
data_finish$INT<-factor(data_finish$INT,levels=c("H","L"))
data_finish$INT<-relevel(data_finish$INT,ref="L")
data_finish$CRE<-factor(data_finish$CRE,levels=c("H","L"))
data_finish$CRE<-relevel(data_finish$CRE,ref="L")




#Teaching Method_Lecture
modelL1<- lmer(TM_L ~ (CRE|run_id)+(INT|run_id)+(1|run_id),data_finish,REML=FALSE)

Ls1<-summary(modelL1)
write.table(round(Ls1$coefficients,3),"clipboard",sep="\t")

modelL2<-update(modelL1,.~.+T_EF)
KRmodcomp(modelL1,modelL2)
modelL3<-update(modelL2,.~.+CRE*T_EF)
KRmodcomp(modelL2, modelL3)
modelL4<-update(modelL2,.~.+INT*T_EF)
KRmodcomp(modelL2, modelL4)
modelL5<-update(modelL2,.~.+T_CR)
KRmodcomp(modelL2, modelL5)
modelL6<-update(modelL5,.~.+T_CR*CRE)
KRmodcomp(modelL5,modelL6)
modelL7<-update(modelL5,.~.+T_CR*INT)
KRmodcomp(modelL5,modelL7)
modelL5<-update(modelL4,.~.+T_CR*INT)
modelL6<-update(modelL5,.~.+T_CR*CRE)
KRmodcomp(modelL5,modelL6)
summary(modelL5)
anova(modelL1, modelL2, modelL3,modelL4,modelL5,modelL6,modelL7)[,1:4]
summary(model_finish)

#Teaching Method_Cooperative Learning
modelC1<- lmer(TM_C ~ CRE+INT+(1|run_id),data_finish,REML=FALSE)
Cs1<-summary(modelC1)
summary(modelC1)
modelC2<-update(modelC1,.~.+T_EF)
KRmodcomp(modelC1, modelC2)
modelC3<-update(modelC2,.~.+CRE*T_EF)
KRmodcomp(modelC2, modelC3)
modelC4<-update(modelC2,.~.+INT*T_EF)
KRmodcomp(modelC2, modelC4)
modelC5<-update(modelC1,.~.+T_CR)
KRmodcomp(modelC1, modelC5)
modelC6<-update(modelC5,.~.+CRE*T_CR)
KRmodcomp(modelC5, modelC6)
modelC7<-update(modelC5,.~.+INT*T_CR)
KRmodcomp(modelC5, modelC7)

anova(modelC1, modelC2, modelC3,modelC4,modelC5,modelC6,modelC7)[,1:4]

model_finish_C<- modelC1
summary(model_finish_C)

#Teaching Method_Inquiry learning


modelI1<- lmer(TM_I ~ CRE+INT+(1|run_id),data_finish,REML=FALSE)
summary(modelI1)


Is1<-summary(modelI1)
write.table(round(Is1$coefficients,3),"clipboard",sep="\t")


modelI2<-update(modelI1,.~.+T_EF)
KRmodcomp(modelI1, modelI2)
modelI3<-update(modelI2,.~.+CRE*T_EF)
KRmodcomp(modelI2, modelI3)
modelI4<-update(modelI2,.~.+INT*T_EF)
KRmodcomp(modelI2, modelI4)
modelI5<-update(modelI4,.~.+T_CR)
KRmodcomp(modelI4, modelI5)
modelI6<-update(modelI5,.~.+T_CR*CRE)
KRmodcomp(modelI5, modelI6)
modelI7<-update(modelI6,.~.+T_CR*INT)
KRmodcomp(modelI6, modelI7)

anova(modelI1, modelI2, modelI3,modelI4,modelI5,modelI6,modelI7)[,1:4]


model_finish_I<- modelI7
summary(model_finish_I)


#fixed effect(model_finish)
stdCoef.merMod(modelL5) 
stdCoef.merMod(modelC1)
stdCoef.merMod(modelI6)

#Assumption
model_finish<- lmer(TM_C ~ INT + CRE + T_EF + T_CR + (1 | run_id) + INT:T_EF + INT:T_CR + T_EF:T_CR + INT:T_EF:T_CR, data_finish, REML=FALSE)
ggqqplot(residuals(model_finish))
ggqqplot(residuals(modelC1))
qqnorm(residuals(modelL5))
qqnorm(residuals(modelC1))
qqnorm(residuals(model_finish))
qqnorm(residuals(modelI6))

plot(fitted(modelL5),residuals(modelL5),xlab="Fitted",ylab="Residuals")
plot(fitted(modelC1),residuals(modelC1),xlab="Fitted",ylab="Residuals")
plot(fitted(modelI6),residuals(modelI6),xlab="Fitted",ylab="Residuals")


#clipboard
Ls2<-summary(modelL7)
write.table(round(Ls2$coefficients,3),"clipboard",sep="\t")
Cs2<-summary(modelC1)
write.table(round(Cs2$coefficients,3),"clipboard",sep="\t")
Is2<-summary(modelI6)
write.table(round(Is2$coefficients,3),"clipboard",sep="\t")
