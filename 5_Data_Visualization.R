library("ggplot2")
library("dplyr")
library("tidyr")
library('showtext')
data<-read.csv("data_finish.csv")

str(data)
data$INT<-as.factor(data$INT)
data$CRE<-as.factor(data$CRE)

data_finish %>%mutate(T_CR_S= ifelse(T_CR>quantile(T_CR,probs=.5, na.rm=T), yes="H",
                                     no=ifelse(T_CR>quantile(T_CR,probs=.5, na.rm=T),"M","L"))) -> data_finish 
data_finish %>%mutate(T_EF_S= ifelse(T_EF>quantile(T_EF,probs=.5, na.rm=T), yes="H",
                                     no=ifelse(T_EF>quantile(T_EF,probs=.5, na.rm=T),"M","L"))) -> data_finish 



std<-data %>%
  group_by(INT,CRE)%>%
  summarise(TM_L_group=mean(TM_L),TM_I_group=mean(TM_I),TM_C_group=mean(TM_C))

std
pivot_longer(std, cols=3:5, names_to = "TM",
             values_to = "value", values_drop_na = FALSE)->std

###INTEREST
std
std %>% 
  ggplot()+
  aes(x=interaction(CRE,INT), y=value, group=TM, color=TM, shape=TM)+
  stat_summary(fun = "mean",geom="point")+stat_summary(fun = "mean",geom="line")+
  xlab("Std. Creativity And Task Interest")+
  ylab("Teaching Method Intention")+
  scale_color_discrete(name ="Teaching Method", labels = c("Lecture", "Cooperative", "Inquiry"), breaks=c("TM_L_group","TM_C_group","TM_I_group"))+
  scale_shape_discrete(name ="Teaching Method", labels = c("Lecture", "Cooperative", "Inquiry"), breaks=c("TM_L_group","TM_C_group","TM_I_group"))+
  scale_x_discrete(labels = c('Cre. Low X Int. Low','Cre.High X Int. Low','Cre. Low X Int. High','Cre. High X Int. High'))+
  theme(text = element_text(size=20))+geom_point(size=3)+ylim(3, 5)+
  theme(legend.position="bottom", legend.box = "horizontal", legend.title=element_blank())



std2<-data_finish %>%
  group_by(INT,CRE,T_EF_S)%>%
  summarise(TM_I_group=mean(TM_I),TM_C_group=mean(TM_C),TM_L_group=mean(TM_L))
std2
pivot_longer(std2, cols=4:6, names_to = "TM",
             values_to = "value", values_drop_na = FALSE)->std2

std2

###Student CRE, INT X Teacher Efficacy

std2$TM<-as.factor(std2$TM)
std2$T_EF_S<-as.factor(std2$T_EF_S)

std2
std2 %>%
  group_by(CRE,INT,T_EF_S,T_CR_S,TM) %>%
  summarise_at(vars(value), list(value = mean))->std2
std2
std2<-std2[std2$T_EF_S!="M",]
std2<-std2[std2$T_CR_S!="M",]
std2
std2$T_EF_S<-as.factor(std2$T_EF_S)
  std2%>%ggplot()+
  aes(x=interaction(CRE,INT), y=value, group=interaction(TM,T_EF_S), color=TM, shape=T_EF_S, linetype=T_EF_S)+
    stat_summary(fun = "mean",geom="point",size=3)+stat_summary(fun = "mean",geom="line")+
    xlab("Std. Creativity And Task Interest")+
  ylab("Teaching Method Intention")+
    scale_color_discrete(name ="Teaching Method", labels = c("Lecture", "Cooperative", "Inquiry"), breaks=c("TM_L_group","TM_C_group","TM_I_group"))+
    scale_x_discrete(labels = c('Cre. Low X Int. Low','Cre.High X Int. Low','Cre. Low X Int. High','Cre. High X Int. High'))+
    scale_shape_discrete(name="Teacher Efficacy",labels = c('SD +1','SD -1'))+
    theme(text = element_text(size=20))+
    scale_linetype_manual(values=c("twodash", "solid"))+
    guides(linetype=FALSE)+
    scale_y_continuous(breaks=seq(3, 5, 0.5))
  
  ###Student CRE, INT X Teacher Creativity
  
  
  std2<-data_finish %>%
    group_by(INT,CRE,T_CR_S)%>%
    summarise(TM_I_group=mean(TM_I),TM_C_group=mean(TM_C),TM_L_group=mean(TM_L))
  std2
  pivot_longer(std2, cols=4:6, names_to = "TM",
               values_to = "value", values_drop_na = FALSE)->std2
  std2$T_CR_S<-as.factor(std2$T_CR_S)
std2
std2<-std2[std2$T_CR_S!="M",]

std2%>%ggplot()+
  aes(x=interaction(CRE,INT), y=value, group=interaction(TM,T_CR_S), color=TM, shape=T_CR_S, linetype=T_CR_S)+
  stat_summary(fun = "mean",geom="point",size=3)+stat_summary(fun = "mean",geom="line")+
  xlab("Std. Creativity And Task Interest")+
  ylab("Teaching Method Intention")+
  scale_color_discrete(name ="Teaching Method", labels = c("Lecture", "Cooperative", "Inquiry"), breaks=c("TM_L_group","TM_C_group","TM_I_group"))+
    scale_x_discrete(labels = c('Cre. Low X Int. Low','Cre.High X Int. Low','Cre. Low X Int. High','Cre. High X Int. High'))+
  scale_shape_discrete(name="Teacher Creativity",labels = c('SD +1','SD  -1'))+
  theme(text = element_text(size=20))+
  scale_linetype_manual(values=c("twodash", "solid"))+
  guides(linetype=FALSE)+
  scale_y_continuous(breaks=seq(3, 5, 0.5))

