
airplane<-read.csv("C:\\Users\\USER\\Downloads\\범자 B팀\\범자비행기만족도.csv")
str(airplane)

#결측치 확인

table(is.na(airplane))

table(is.na(airplane$Gender))

table(is.na(airplane$Age))

table(is.na(airplane$Type.of.Travel))

table(is.na(airplane$Class))

table(is.na(airplane$Flight.Distance))

table(is.na(airplane$Inflight.wifi.service))

table(is.na(airplane$Departure.Arrival.time.convenient))

table(is.na(airplane$Gate.location))

table(is.na(airplane$Food.and.drink))

table(is.na(airplane$Seat.comfort))

table(is.na(airplane$Inflight.entertainment))

table(is.na(airplane$On.board.service))

table(is.na(airplane$Leg.room.service))

table(is.na(airplane$Baggage.handling))

table(is.na(airplane$Checkin.service))

table(is.na(airplane$Inflight.service))

table(is.na(airplane$Cleanliness))

table(is.na(airplane$Departure.Delay.in.Minutes))

table(is.na(airplane$Arrival.Delay.in.Minutes))

#원래 자료에서 결측치 제거

air<-na.omit(airplane)



#500개 랜덤추출

airplane1<-airplane[sample(nrow(airplane),500), ]

getwd()

write.csv(airplane1,file="airplane1.csv",row.names=FALSE)

airplane1

airplane2<-na.omit(airplane1)

write.csv(airplane2,file="airplane2.csv",row.names=FALSE)

airplane2<-read.csv("airplane2.csv")

x11();

boxplot(air)

x11();

boxplot(airplane2)

summary(air)

summary(airplane2)

#EDA

#EDA->barplot으로 시각화

color<-c("Light Gray","Pink2","Lemon Chiffon 1","Light Blue 2","Dark Olive Green 1","Khaki 2")

color2<-c("Light Gray","Pink2","Lemon Chiffon 1","Light Blue 2","Khaki 2")

#성별

barplot(table(airplane2$Gender), main="성별",col=c("Pink2","Lemon Chiffon 1"),ylim=c(0,310),legend=c("남성", "여성"))

#여행 유형

barplot(table(airplane2$Type.of.Travel), main="여행유형",col=c("Pink2","Lemon Chiffon 1"),ylim=c(0,400),legend=c("출장", "개인 여행"))

#등급별

barplot(table(airplane2$Class), main="등급",col=c("Pink2","Light Gray","Lemon Chiffon 1"),ylim=c(0,350),legend=c("Eco", "Eco Plus", "Business"))

#와이파이 만족도

barplot(table(airplane2$Inflight.wifi.service), main="와이파이 만족도",col=color,ylim=c(0,170),legend=c(“적용불가”,"매우 불만족", "불만족", "보통","만족", "매우 만족"))

#출발/도착 시간의 편리성

barplot(table(airplane2$Departure.Arrival.time.convenient), main="출발/도착 시간의 편리성",ylim=c(0,200),col=color,legend=c(“무응답”,"매우 불만족", "불만족", "보통","만족", "매우 만족"))

#게이트 위치

barplot(table(airplane2$Gate.location), main="게이트 위치의 편리성",ylim=c(0,160),col=color2,legend=c("매우 불만족", "불만족", "보통","만족", "매우 만족"))

#식음료

barplot(table(airplane2$Food.and.drink), main="식음료에 대한 만족도",ylim=c(0,200),col=color,legend=c(“무응답”,"매우 불만족", "불만족", "보통","만족", "매우 만족"))

#시트(좌석 편리성)

barplot(table(airplane2$Seat.comfort), main="시트에 대한 만족도",ylim=c(0,250),col=color2,legend=c("매우 불만족", "불만족", "보통","만족", "매우 만족"))

#기내 엔터테인먼트 만족도

barplot(table(airplane2$Inflight.entertainment),ylim=c(0,220), main="기내 엔터테인먼트에 대한 만족도",col=color2,legend=c("매우 불만족", "불만족", "보통","만족", "매우 만족"))

#기내 서비스 만족도

barplot(table(airplane2$On.board.service),ylim=c(0,230), main="기내 서비스에 대한 만족도",col=color2,legend=c("매우 불만족", "불만족", "보통","만족", "매우 만족"))

#좌석 간 거리에 대한 만족도

barplot(table(airplane2$Leg.room.service),ylim=c(0,200), main="좌석 간 거리에 대한 만족도",col=color,legend=c(“무응답”,"매우 불만족", "불만족", "보통","만족", "매우 만족"))

#수하물 취급 만족도

barplot(table(airplane2$Baggage.handling),ylim=c(0,250), main="수하물 취급에 대한 만족도",col=color2,legend=c("매우 불만족", "불만족", "보통","만족", "매우 만족"))

#체크인 서비스에 대한 만족도

barplot(table(airplane2$Checkin.service),ylim=c(0,225), main="체크인 서비스에 대한 만족도",col=color2,legend=c("매우 불만족", "불만족", "보통","만족", "매우 만족"))

#공항 내 항공사 서비스 만족도

barplot(table(airplane2$Inflight.service),ylim=c(0,240), main="공항 내 항공사 서비스에 대한 만족도",col=color2,legend=c("매우 불만족", "불만족", "보통","만족", "매우 만족"))

#청결도에 대한 만족도

barplot(table(airplane2$Cleanliness), main="청결도에 대한 만족도",ylim=c(0,200),col=color2,legend=c("매우 불만족", "불만족", "보통","만족", "매우 만족"))

#나이

a<-airplane2$Age

aa<-cut(a,c(0,10,20,30,40,50,60,70,80,90),right=F,include.lowset=T)

table(aa)

levels(aa)<-c("10세 미만","10대","20대","30대","40대","50대","60대","70대","80세 이상")

table(aa)

barplot(table(aa), main="연령대",ylim=c(0,130),col=color,legend=c("10세 미만","10대","20대","30대","40대","50대","60대","70대","80세 이상"))

#비행거리

b<-airplane2$Flight.Distance

bb<-cut(b,c(0,500,1000,2000,3000,4000),right=F,include.lowset=T)

table(bb)

levels(bb)<-c("0~499","500~999","1000~1999","2000~2999","3000~4000")

table(bb)

barplot(table(bb), main="비행거리",ylim=c(0,160),col=color2,legend=c("0~499","500~999","1000~1999","2000~2999","3000~4000"))

#만족도(종속)

barplot(table(airplane2$satisfaction), main="만족",col=unique(airplane2$satisfaction),legend=c("불만족", "만족"))

#범주형변수 독립성검정

chisq.test(airplane2$satisfaction, airplane2$Gender)

chisq.test(airplane2$satisfaction, airplane2$Type.of.Travel)

#순서형변수 독립성검정

install.packages("vcdExtra")

library(vcdExtra)

airplane2$Class

class<-xtabs(~Class+satisfaction,data=airplane2)

CMHtest(class,recores=c(1,2,3,4,5))

airplane2$Inflight.wifi.service

inflight.wifi.service<-xtabs(~Inflight.wifi.service+satisfaction,data=airplane2)

CMHtest(inflight.wifi.service,recores=c(1,2,3,4,5))



airplane2$Departure.Arrival.time.convenient

Departure<-xtabs(~Departure.Arrival.time.convenient+satisfaction,data=airplane2)

CMHtest(Departure,recores=c(1,2,3,4,5))



airplane2$Gate.location  

Gate<-xtabs(~Gate.location+satisfaction,data=airplane2)

CMHtest(Gate,recores=c(1,2,3,4,5))

airplane2$Food.and.drink

Food<-xtabs(~Food.and.drink+satisfaction,data=airplane2)

CMHtest(Food,recores=c(1,2,3,4,5))

airplane2$Seat.comfort 

Seat<-xtabs(~Seat.comfort +satisfaction,data=airplane2)

CMHtest(Seat,recores=c(1,2,3,4,5))

airplane2$Inflight.entertainment

Inflight<-xtabs(~Inflight.entertainment+satisfaction,data=airplane2)

CMHtest(Inflight,recores=c(1,2,3,4,5))

airplane2$On.board.service

On.board<-xtabs(~On.board.service+satisfaction,data=airplane2)

CMHtest(On.board,recores=c(1,2,3,4,5))

airplane2$Leg.room.service 

Leg<-xtabs(~Leg.room.service+satisfaction,data=airplane2)

CMHtest(Leg,recores=c(1,2,3,4,5))

airplane2$Baggage.handling 

Baggage<-xtabs(~Baggage.handling+satisfaction,data=airplane2)

CMHtest(Baggage,recores=c(1,2,3,4,5))

airplane2$Checkin.service

Checkin<-xtabs(~Checkin.service+satisfaction,data=airplane2)

CMHtest(Checkin,recores=c(1,2,3,4,5))

airplane2$Inflight.service   

service<-xtabs(~Inflight.service+satisfaction,data=airplane2)

CMHtest(service,recores=c(1,2,3,4,5))

airplane2$Cleanliness   

clean<-xtabs(~Cleanliness+satisfaction,data=airplane2)

CMHtest(clean,recores=c(1,2,3,4,5))

##연속형 변수(다중공산성)

aa<-cor(airplane2[,c(2,5,18,19)])

aa

install.packages("corrplot")

library(corrplot)

corrplot.mixed(aa,lower="ellipse", upper="number", tl.pos = c("lt"),bg="grey")

#stepwise

fit<-glm(satisfaction ~ Age + factor(Type.of.Travel) + Class +  Flight.Distance + Inflight.wifi.service + 
           
           Seat.comfort + Inflight.entertainment + On.board.service + Leg.room.service + 
           
           Baggage.handling + Inflight.service + Cleanliness + Departure.Delay.in.Minutes, family = binomial, data = airplane2)

summary(fit)

library(MASS)

stepAIC(fit, direction = "both") 

fit1<-glm(formula = satisfaction ~ Age + factor(Type.of.Travel) + Class + 
            
            Flight.Distance + Inflight.wifi.service + On.board.service + 
            
            Baggage.handling + Cleanliness, family = binomial, data = airplane2)

summary(fit1)

install.packages("car")

library(carData)

anova(fit1,fit)

AIC(fit,fit1)

#회귀분석

fit2<-glm(formula = satisfaction ~ Age + factor(Type.of.Travel) + Class + 
            
            Flight.Distance + Inflight.wifi.service + On.board.service + 
            
            Baggage.handling + Cleanliness, family = binomial("logit"), data = airplane2)

summary(fit2)



#deviance 값을 통해 모형의 적합성 형가

1-pchisq(661.18-373.55,496-488)

#ROC curve

install.packages("pROC")

library(pROC)

rocplot<-roc(satisfaction~fitted(fit2),data=airplane2)

plot.roc(rocplot,legacy.axes=TRUE)

auc(rocplot)

#odds

exp(0.0362441)

exp(-2.2094334)

exp(0.7616152)

exp(0.0003161)

exp(0.5917370)

exp(0.4857308)

exp(0.2713777)

exp(0.4782986)

