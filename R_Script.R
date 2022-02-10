#Eisagogi ton dedomenon
attach(Hobbit_values)

#Eisagogi tis vivliothikis Hmisc
library(Hmisc)
describe(income)

#Proto erotima:
library(plotrix)
lbls <- c("<$15,000", "$15,000  to $24,999", "$25,000 to $49,999", "$50,000 to $74,999", "$75,000 \n to $99,999","$100,000 \n to $149,999","$150,000+")
pct <- round(count/sum(count)*100)
lbls <- paste(lbls, pct,sep="\n") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie3D(count,labels=lbls,explode=0.1,
      main="Tax household income")



#Deutero erotima
boxplot(avprice~income,main="Entree meal according to income")
summary(avprice~income,main="Entree meal according to income")
cor.test(income,avprice)


#Trito erotima
Hmisc::describe(simple)
Hmisc::describe(elegant)

count <-table(gender,simple)
barplot(count,beside=T, xlab = 'Simple Decor', ylab='Count')
Gender <- c('M','F')
legend("topleft",Gender, fill= gray.colors(4))

count <-table(gender,elegant)
barplot(count,beside=T, xlab = 'Elegant Decor', ylab='Count')
Gender <- c('M','F')
legend("topright",Gender, fill= gray.colors(4))


#4o erotima
Hmisc::describe(zipcode)

count <-table(income,zipcode)
par("mar")
par(mar=c(1,1,1,1))

barplot(count)
barplot(count,beside=T, xlab = 'ZipCode', ylab='Count', col=rainbow(7))
Money <- c('<$15,000','<$24,999','<$49,999','<$74,999','<$99,999','<$149,999','$150,000+')
legend("topright",Money, fill= rainbow(7))
psych::describeBy(income,group = zipcode)



#5o erotima
count <-table(zipcode,likely)
describeBy(likely, group= zipcode)
par("mar")
par(mar=c(1,1,1,1))

barplot(count,beside=T, xlab = 'Patronize', ylab='Count', col=rainbow(4), names.arg=c("Very Unlikely","Somewhat Unlikely","Neither Likely Nor Unlikely","Somewhat Likely", "Very Likely"))
ZipCode <- c('A','B','C','D')
legend(title="Zip Code", "topright",ZipCode, fill= rainbow(4))



#6o erotima
Tabletotspent <-prop.table(table(TotspentCut))
count <-table(TotspentCut,waitstaf)
barplot(count,beside=T, xlab = 'waitstaf', ylab='Count', col=rainbow(6),names.arg=c('Very Strongly Not Prefer','Somewhat Not Prefer','Neither Prefer Nor Not Prefer','Somewhat Prefer','Very Strongly Prefer'))
WaitStaf <- c("<50$","<100$","<150$","<200$","<250$","+250$")
legend("topright",WaitStaf, fill= rainbow(6))
TotspentCut <- cut(totspent, breaks=c(0,50,100,150,200,250,300), labels=c("<50","<100","<150","<200","<250","+250"))



#Epipleon tests

var.test(totspent,reincome)

t.test(reincome,totspent)


plot(totspent,avprice,pch=16)
abline(lm(avprice~totspent))
summary(lm(avprice~totspent))


#Kataskuei modelon

model<-lm(avprice~totspent)
summary(model)


ZipcodeCut <- cut(zipcode, breaks=c(0,1,2,3,4), labels=c("A","B","C","D"))
table(ZipcodeCut)
model5<-lm(reincome~ZipcodeCut)

summary(model5)
VarietyCut <- cut(variety, breaks=c(0,1,2,3,4,5), labels=c("Very Strongly Not Prefer","Somewhat Not Prefer","Neither Prefer Nor Not Prefer","Somewhat Prefer","Very Strongly Prefer"))
table(VarietyCut)
UnusualCut <- cut(unusal, breaks=c(0,1,2,3,4,5), labels=c("Very Strongly Not Prefer","Somewhat Not Prefer","Neither Prefer Nor Not Prefer","Somewhat Prefer","Very Strongly Prefer"))
table(UnusualCut)


model7<-lm(avprice~UnusualCut+VarietyCut)
summary(model7)
rm(VarietyCut)

VarietyCut<- factor(VarietyCut,order=T)
levels(VarietyCut)

income<-factor(income, order=T)
levels(income=="2")

zipcode<- factor(zipcode,order=T)
levels(zipcode)

model8<-lm(avprice~totspent+zipcode + income+zipcode:income)
summary(model8)


educa<-factor(educa, order=T)
levels(educa)

model9<-lm(reincome~totspent+educa+zipcode+avprice+I(totspent^2)+I(avprice^2)+zipcode:avprice)
summary(model9)

model10<-update(model9,~.-educa)
summary(model10)

model11<-update(model10,~.-I(totspent^2))
summary(model11)


model12<-update(model11,~.-I(avprice^2))
summary(model12)


model13<-lm(reincome~totspent+zipcode+avprice+zipcode:avprice,subset=(1:length(reincome)!=65))
summary(model13)


par(mfrow=c(2,2))

plot(model13)

