
#Importing Data
store_trai = read.csv("store_train.csv",stringsAsFactors = F)
store_test = read.csv("store_test.csv",stringsAsFactors = F)


#Creating new column for target variable in test data,and data column to both the data sets
setdiff(colnames(store_trai),colnames(store_test))
store_test$store = NA
store_trai$data = 'train'
store_test$data = 'test'

#Binding the testing and training data
store_all=rbind(store_trai,store_test)

#Viewing data
library(dplyr)
glimpse(store_all)

#Data Preparation
#Cleaning for variable 'countyname'
length(unique(table(store_all$countyname)))
sum(unique(table(store_all$countyname)))

#Since it has lot of unique values, we will kick it off
store_all = store_all %>% 
  select(-countyname)

#Cleaning for variable 'Areaname'
length(unique(table(store_all$Areaname)))
sum(unique(table(store_all$Areaname)))

#Since it has lot of unique values, we will kick it off
store_all = store_all %>% 
  select(-Areaname)

#Cleaning for variable 'countytownname'
length(unique(table(store_all$countytownname)))
sum(unique(table(store_all$countytownname)))

#Since it has lot of unique values, we will kick it off
store_all = store_all %>% 
  select(-countytownname)

#Cleaning for variable 'state_alpha'
length(unique(table(store_all$state_alpha)))
sum(unique(table(store_all$state_alpha)))

#Since it has lot of unique values, we will also kick off this variable
store_all = store_all %>% 
  select(-state_alpha)

str(store_all)

#Checking for columns which have NA values
sapply(store_all,function(x) sum(is.na(x)))

#Removing NA values from country and population
store_all$population[is.na(store_all$population)]=round(mean(store_all$population,na.rm=T),0)
store_all$country[is.na(store_all$country)]=round(mean(store_all$country,na.rm=T),0)

#Convert char columns into categorical columns
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

char_logical=sapply(store_all,is.character)
cat_cols=names(store_all)[char_logical]
cat_cols=cat_cols[!(cat_cols %in% c('data','store'))]
cat_cols

#Create Dummies
for(col in cat_cols){
  store_all = CreateDummies(store_all,col,50)
}

glimpse(store_all)

# Remove remaining NA values from all other columns
store_all=store_all[!((is.na(store_all$store)) & store_all$data=='train'), ]
for(col in names(store_all)){
  if(sum(is.na(store_all[,col]))>0 & !(col %in% c("data","store"))){
    store_all[is.na(store_all[,col]),col]=mean(store_all[store_all$data=='train',col],na.rm=T)
  }
}

any(is.na(store_all))
sum(is.na(store_all))

colSums(is.na(store_all))
#It is for target variable

#Data Splitting
store_trai=store_all %>% filter(data=='train') %>% select(-data)
store_test=store_all %>% filter(data=='test') %>% select(-data)

#Building Model
set.seed(2)
s = sample(1:nrow(store_trai),0.80*nrow(store_trai))
train_80 = store_trai[s,]
train_20 = store_trai[-s,]

library(car)
for_vif=lm(store~.,data=train_80)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id,data=train_80)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id -sales0,data=train_80)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id -sales0 - sales2,data=train_80)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id -sales0 - sales2,data=train_80)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id -sales0 - sales2 -sales3,data=train_80)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id -sales0 - sales2 -sales3 -sales1,data=train_80)
sort(vif(for_vif),decreasing = T)[1:3]

summary(for_vif)

# Logistic Model
fit = glm(store~.-Id -sales0 - sales2 -sales3 - sales1,data=train_80, family = "binomial")
step(fit)

summary(fit)
formula(fit)
fit = glm(store ~ country + sales4 +
            State + CouSub + population + storecode_METRO12620N23019 + 
            storecode_NCNTY23003N23003 + storecode_METRO14460MM1120 + 
            store_Type_SupermarketType3 + store_Type_GroceryStore + store_Type_SupermarketType1,
          data = train_80,
          family = "binomial"
          )

library(pROC)
score = predict(fit,newdata = train_20,type = "response")
roccurve=roc(train_20$store,score) 
auc(roccurve)

# Decision Tree
library(tree)
DT= tree(as.factor(store)~.-Id,data=train_80)
DTscore=predict(DT,newdata=train_20,type="vector")[,2]
auc(roc(train_20$store,DTscore))

# Random Forests
library(randomForest)
rf.model3= randomForest(as.factor(store)~.-Id,data=train_80)
test.score3=predict(rf.model3,newdata=train_20,type="prob")[,2]
auc(roc(train_20$store,test.score3))

# Parameter Tunning
library(cvTools)
store_trai$store=as.factor(store_trai$store)
glimpse(store_trai)

## 1076    3   800      500        5
#Use full train data because here we are doing CV

#Parameter value we want to try out
#mtry: There will be upperlimit.Upperlimit means no of predictor in the data. Good idea is to start with 4 or 5 then go to no of variables in the data
#ntree:This is number of trees in the forest.There is no limit on it as such , a good starting point is 10 to 500 and you can try out values as large as 1000,5000. Although very high number of trees make sense when the data is huge as well. Default value is 500.
#maxnodes:start with 5 there is, there is no limiton this as such but good range to try can be between 1 to 20. Default value is 1.
#nodesize:There is no limit on this as such but good range to try can be between 1 to 20. Default value is 1.If values comes at edge then try to expand

param=list(mtry=c(3,4,6,8,10),
           ntree=c(50,100,200,500,700,800,900), 
           maxnodes=c(5,10,15,20,30,50,100,300,500,600,700),
           nodesize=c(1,2,5,10,20,30,40)       
)



mycost_auc=function(store,yhat){  #Real #Predicted
  roccurve=pROC::roc(store,yhat)
  score=pROC::auc(roccurve)
  return(score)
}  


#We are looking at 5*7*11*7 combination. Hence it will took an hour to run

## Function for selecting random subset of params


subset_paras=function(full_list_para,n=10){  #n=10 is default, you can give higher value
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trial=100
my_params=subset_paras(param,num_trial)
my_params

muauc = 0
for(i in 1:num_trial){  
  #print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,
             store~.-Id, 
             data =store_trai,
             tuning =params,
             folds = cvFolds(nrow(store_trai), K=15, type ="random"),
             cost =mycost_auc, 
             seed =2,
             predictArgs = list(type="prob"))
  
  score.this=k$cv[,2]
  
  ## It took almost hours to run because we are trying 2695 combinations 
  
  
  if(score.this>myauc){
    #print(params)
    #uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    #uncomment the line above to keep track of progress
    #print(myauc)
    best_params=params
  }
  #print('DONE')
}


myauc

best_params
# Best params at 1487-> 4, 500, 600, 10

ci.rf.final=randomForest(store~.-Id,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=store_trai
)

test.score_final=predict(ci.rf.final,newdata=store_test, type="prob")[,2]
write.csv(test.score_final,'Shreya_gupta_P2_part2.csv',row.names = F)


