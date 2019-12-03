library(dplyr)

cros =read.csv("C:\\Users\\Pavan\\Desktop\\Jigsaw\\Financials Analytics\\3. Mod 6\\UniversalBank.csv",header=4,na.strings=c(""," ","  "))

#In order to create a cross sell grid table as shown below, we first have to create indicator variables to code only the incidence of the products as 1 else 0.
#Create a cross- sell grid/penetration using the product fields of  Education_b,
#Personal.Loan_b, Securities.Account_b, CD.Account,Online_b

cros$Education_b=ifelse(cros$Education>=1,1,0)
cros$Personal.Loan_b=ifelse(cros$Personal.Loan>=1,1,0)
cros$Securities.Account_b=ifelse(cros$Securities.Account>=1,1,0)
cros$CD.Account_b=ifelse(cros$CD.Account>=1,1,0)
cros$Online_b=ifelse(cros$Online>=1,1,0)

#cross- sell grid/penetration
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union

cros%>%filter(Education_b==1)%>%summarise(psum=sum(Personal.Loan_b),ssum=sum(Securities.Account_b),cdsum=sum(CD.Account_b),osum=sum(Online_b))-> esum
esum$esum<-""
esum
cros%>%filter(Personal.Loan_b==1)%>%summarise(esum=sum(Education_b),ssum=sum(Securities.Account_b),cdsum=sum(CD.Account_b),osum=sum(Online_b))-> psum
psum$psum<-""
psum
cros%>%filter(Securities.Account_b==1)%>%summarise(esum=sum(Education_b),psum=sum(Personal.Loan_b),cdsum=sum(CD.Account_b),osum=sum(Online_b))-> ssum
ssum$ssum<-""
ssum
cros%>%filter(CD.Account_b==1)%>%summarise(esum=sum(Education_b),psum=sum(Personal.Loan_b),osum=sum(Online_b),ssum=sum(Securities.Account_b))-> cdsum
cdsum$cdsum<-""
cdsum
cros%>%filter(Online_b==1)%>%summarise(esum=sum(Education_b),psum=sum(Personal.Loan_b),cdsum=sum(CD.Account_b),ssum=sum(Securities.Account_b))-> osum
osum$osum<-""
osum

#creating the dataframe from the above arrays of output

crtab=rbind(esum,osum,psum,ssum,cdsum)
class(crtab)
colnames(crtab)<-c("Personal.Loan","Securities.Account","CD.Account","Online","Education")
rownames(crtab)<-c("Education","Online","Personal.Loan","Securities.Account","CD.Account")

#arranage columns and rows
crtab=crtab[,c(names(cros)[c(8,10:13)])]
crtab%>%arrange(match(rownames(crtab),c(names(ub)[c(8,10:13)])))->crtab
crtab=sapply(crtab,as.numeric)
crtab=data.frame(crtab)
rownames(crtab)<-c(names(ub)[c(8,10:13)])

#Total Customers with Product in Row
#Education
Edu<-rowSums(crtab[1,],na.rm=TRUE)+ub%>%filter(Education_b==1)%>%nrow() 
PL<-rowSums(crtab[2,],na.rm=TRUE)+ub%>%filter(Education_b==0,Personal.Loan_b==1,Securities.Account_b==0,CD.Account_b==0,Online_b==0)%>%nrow()
SA<-rowSums(crtab[3,c(1,4,5)])
CD<-rowSums(crtab[4,c(1,5)])
On<-crtab[5,1]
crtab$TC_PRow=c(Edu,PL,SA,CD,On)
crtab

crtab.prop=crtab[,-6]
crtab.prop[is.na(crtab.prop)]<-0
rownames(crtab.prop)<-NULL
#colnames(crtab.prop)<-NULL
crtab.prop=rbind(crtab.prop[1,]/Edu,crtab.prop[2,]/PL,crtab.prop[3,]/SA,crtab.prop[4,]/CD,crtab.prop[5,]/On)
rownames(crtab.prop)<-c(row.names(crtab))

percent<-function(x){
  paste0(round(x,3)*100,"%")
}

crtab.prop=sapply(as.data.frame(crtab.prop),percent)
rownames(crtab.prop)<-c(row.names(crtab))
print.data.frame(data.frame(crtab.prop),quote=FALSE)

#Where the proportion is above 48% then the Bank's cross sell efforts are effective - Education Loan to Personal Loan/Securities/CD Account.

#Where the proportion is less than 6.5% are the areas where the Bank could be doing better. Personal Loan to Education Loan /Securities; Securities to Education Loan/Personal Loan; CD Account to Education Loan.

