# adaboost for variable importance
library(ada)
setwd("C:/Users/Wesle/Documents/GitHub/social-capital-task/lib")
Census_data <-read.csv("../data/Census data with variables 2016.csv",header = T,sep=",",as.is = T)
rate_2014   <-read.table("../output/rate_2014.tsv",header=T) # first row seems to be the description of the variable
var_des     <-Census_data[1,] # store description
Census_data <-Census_data[-1,-1] # rest of the data,drop the first column and first row





rate_2014$id          <- as.integer(rate_2014$id) # we need to convert to integer for combining data
Census_data$GEO.id2   <- as.integer(Census_data$GEO.id2)
names(rate_2014)      <- c("GEO.id2","SK14") # rename the label

states<-regmatches(Census_data$GEO.display.label,regexpr(pattern = ", [A-Z]+[a-z]+",text=Census_data$GEO.display.label))
states<-as.factor(substr(states,3,nchar(states)))
Census_data$GEO.display.label<-states
county<-gsub(",.*$", "", Census_data$GEO.display.label)
Census_data$county<-county
newdata<-rate_2014 %>%
  left_join(Census_data,by="GEO.id2") #combine two table 

colnames(newdata)
var_y<-"SK14"
var_X<-strsplit("HC01_VC03
HC03_VC07
HC03_VC23
HC03_VC50
HC03_VC69
HC03_VC101
HC03_VC131
HC03_VC132
HC03_VC134
HC03_VC164",split="\n")[[1]]

filter_data<-newdata[,c(var_y,"county",var_X)]
X<-apply(filter_data[,var_X],2,as.numeric)
filter_data<-cbind(filter_data[,c(var_y,"county")],X)

# need to classify each class
class<-ifelse(filter_data$SK14>0,1,-1)
summary(class)
filter_data$class<-class



insurance_var<-strsplit("HC03_VC131 HC03_VC132 HC03_VC134",split=" ")[[1]]
modified_insurance<-(filter_data[,insurance_var]/filter_data$HC01_VC03)


filter_data[,c("HC03_VC131_m","HC03_VC132_m","HC03_VC134_m")]<-modified_insurance
write.csv(filter_data,"Interesting_vars_based_on_lasso.csv")


filter_data$county<-as.factor(filter_data$county)

data_forada<-filter_data[,!colnames(filter_data)%in% c(insurance_var,"SK14","HC01_VC03")]

names(data_forada)
names(data_forada)<-c("states","Unemployment_Rate","Both_Parents_working","Hunting_fishing_industry","Self-employed","FoodStamp",
                      "Poverty_level","class","Public_insurance_coverage_modified","private_insurance_coveraged_modified","no_health_insurance")


thisada<-ada(class~.,data=data_forada,iter=20,nu=1,type="discrete")
varplot(thisada)

plot(thisada,T,T)


head(filter_data)
str(filter_data)

data(iris)
##drop setosa
iris[iris$Species!="setosa",]->iris
##set up testing and training data (60% for training)
n<-dim(iris)[1]
trind<-sample(1:n,floor(.6*n),FALSE)
teind<-setdiff(1:n,trind)
iris[,5]<- as.factor((levels(iris[,5])[2:3])[as.numeric(iris[,5])-1])
##fit 8-split trees
gdis<-ada(Species~.,data=iris[trind,],iter=20,nu=1,type="discrete")
##add testing data set
gdis=addtest(gdis,iris[teind,-5],iris[teind,5])
##plot gdis
plot(gdis,TRUE,TRUE)
##variable selection plot
varplot(gdis)
##pairwise plot
pairs(gdis,iris[trind,-5],maxvar=2)
head(iris)