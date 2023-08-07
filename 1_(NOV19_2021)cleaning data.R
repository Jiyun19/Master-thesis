remove(list=ls())
library("dplyr")
library("haven")
library("psych")
library("tidyverse")
library("car")
library("rstatix")
library("rvest")
library("MANOVA.RM")
library(matrixStats)

data_raw<-read.csv("data.csv")
data_raw %>% select(run_id,response,st_num) -> data_new
head(data_new)

#cleaning data

data_split<-as.data.frame(str_split_fixed(data_new$response, ",",12))
bind_cols(data_new, data_split)->data_combine
data_long <- gather(data_combine, variable, measurement, V1:V12, factor_key=TRUE)
data_long2<-as.data.frame(str_split_fixed(data_long$measurement, ":",2))

data_long2<-bind_cols(data_long, data_long2)
data_long2$V1<-gsub('"',"",as.character(data_long2$V1))
data_long2$V1<-gsub('\\{',"",as.character(data_long2$V1))
data_long2$V1<-gsub('\\}',"",as.character(data_long2$V1))
data_long2$V2<-gsub('"',"",as.character(data_long2$V2))
data_long2$V2<-gsub('\\{',"",as.character(data_long2$V2))
data_long2$V2<-gsub('\\}',"",as.character(data_long2$V2))
data_finish<-data_long2 %>%select(run_id,st_num,var=V1,measurement=V2)


data_finish<-data_finish[!data_finish$measurement=="",]
data_finish<-data_finish %>% distinct()

data_finish<-data_finish%>%
  pivot_wider(names_from =var, values_from = measurement)


data_finish<-select(data_finish, -starts_with(c("age", "exp","T_","gender","region","gift","accept","mobile")))

data_finish<-left_join(data_finish,T_demo,by="run_id")
data_finish <-data_finish %>% mutate_at(c("exp_y","exp_m", "giftexp_y","giftexp_m","school","region"), ~replace(., is.na(.), 0))
data_finish<- data_finish[complete.cases(data_finish), ]

##CREATIVITY INTEREST CATEGORIZE
data_finish<-data_finish %>%mutate(CRE=ifelse(st_num =="s1"|st_num =="s3",yes="H",no="L"),CRE=ordered(CRE,levels=c("L","H")))
data_finish<-data_finish %>%mutate(INT=ifelse(st_num =="s1"|st_num =="s2",yes="H",no="L"),INT=ordered(INT,levels=c("L","H")))
head(data_finish)                      

data_finish%>%select(run_id,mobile)%>%distinct() %>% group_by(mobile)%>%filter(n()>1)


##data as.factor
data_finish$st_num<-as.factor(data_finish$st_num)
data_finish$run_id<-as.factor(data_finish$run_id)

data_finish %>%mutate_at(vars(starts_with(c("TM_","ST_","exp","giftexp","age","T_"))), funs(as.numeric))->data_finish
###Teaching Method SCORE
data_finish<-data_finish %>%mutate(TM_I=(TM_I_1+TM_I_2+TM_I_3+TM_I_4)/4+1)
data_finish<-data_finish %>%mutate(TM_L=(TM_L_1+TM_L_2+TM_L_3+TM_L_4)/4+1)
data_finish<-data_finish %>%mutate(TM_C=(TM_C_1+TM_C_2+TM_C_3+TM_C_4)/4+1)

##Teacher Efficiency,Teacher Creativity score


data_finish<-data_finish %>%mutate(T_EF=T_EF_1 +T_EF_2+T_EF_4+T_EF_5+5)
data_finish<-data_finish %>%mutate(T_CR=T_CR_1+T_CR_2+T_CR_3+T_CR_4+T_CR_5+5) 

##Manipulation categorize
data_finish$ST_INT<-data_finish$ST_INT+1
data_finish$ST_CRE<-data_finish$ST_CRE+1

##Teacher experience

data_finish<-data_finish%>%mutate(year=as.numeric(exp_y)+as.numeric(exp_m)*(1/12),giftyear=as.numeric(giftexp_y)+as.numeric(giftexp_m)*(1/12))
data_finish%>%distinct(run_id,.keep_all = TRUE)%>%describe()

##INTERST,CREATIVE, catagorize

data_finish<-mutate_at(data_finish, c("INT", "CRE"), funs(ifelse(. == "H", 1,0)))



##Cleaned Data Save

write.csv(data_finish, "data_finish.csv")

data_finish%>%select(CRE,INT,TM_L,TM_I,TM_C,T_EF,T_CR) %>%describe()%>%t()%>%round(3)%>%write.table("clipboard",sep="\t")


data_finish%>%select(run_id, gift)%>%distinct()%>%select(gift)%>%table()
data_finish%>%select(run_id, gender)%>%distinct()%>%select(gender)%>%table()

data_finish$T_EF<-data_finish$T_EF-mean(data_finish$T_EF)
data_finish$T_CR<-data_finish$T_CR-mean(data_finish$T_CR)

data_finish%>%select(run_id, T_EF,T_CR)%>%distinct()%>%arrange(run_id)->teacher
data_finish%>%select(run_id, CRE,INT,TM_L, TM_C, TM_I)%>%distinct()%>%arrange(run_id,CRE,INT)->student

write_sav(student, "student.sav")
write_sav(teacher, "teacher.sav")
