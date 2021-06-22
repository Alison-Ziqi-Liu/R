bankChurners = read.csv("P:/BankChurners.csv")
attach(bankChurners)

bankChurners$Attrition_Flag = ifelse(Attrition_Flag=='Attrited Customer',1,0)
bankChurners$Attrition_Flag = as.factor(bankChurners$Attrition_Flag)
attach(bankChurners)


########################## Exploratory Data Analysis
###numerical variables - stacked histogram
library(ggplot2)

ggplot(bankChurners, aes(Customer_Age, fill = Attrition_Flag)) +geom_histogram(binwidth = 3)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Dependent_count, fill = Attrition_Flag)) +geom_histogram(binwidth = 1)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Months_on_book, fill = Attrition_Flag)) +geom_histogram(binwidth = 3)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Total_Relationship_Count, fill = Attrition_Flag)) +geom_histogram(binwidth =1)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Months_Inactive_12_mon, fill = Attrition_Flag)) +geom_histogram(binwidth = 1)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Contacts_Count_12_mon, fill = Attrition_Flag)) +geom_histogram(binwidth = 1)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Credit_Limit, fill = Attrition_Flag)) +geom_histogram(binwidth = 1000)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Total_Revolving_Bal, fill = Attrition_Flag)) +geom_histogram(binwidth = 100)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Avg_Open_To_Buy, fill = Attrition_Flag)) +geom_histogram(binwidth = 1000)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Total_Amt_Chng_Q4_Q1, fill = Attrition_Flag)) +geom_histogram(binwidth = 0.2)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Total_Trans_Amt, fill = Attrition_Flag)) +geom_histogram(binwidth = 500)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Total_Trans_Ct, fill = Attrition_Flag)) +geom_histogram(binwidth = 10)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Total_Ct_Chng_Q4_Q1, fill = Attrition_Flag)) +geom_histogram(binwidth = 0.2)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
ggplot(bankChurners, aes(Avg_Utilization_Ratio, fill = Attrition_Flag)) +geom_histogram(binwidth = 0.1)+scale_fill_manual(values=c("#5899F7","#F9CA55"))

#EDA-numerical example
my_theme = theme_bw() + theme(axis.title.x = element_text(size = 14),
                 axis.text.x = element_text(size = 10),
                 axis.title.y = element_text(size = 14),
                 axis.text.y = element_text(size = 10))

p_EDA_n = ggplot(bankChurners, aes(Total_Trans_Ct, fill = Attrition_Flag)) +geom_histogram(binwidth = 10)+scale_fill_manual(values=c("#5899F7","#F9CA55"))
p_EDA_n + labs(x='Total Transaction Count (1yr)',y='Number of Clients')+my_theme

library(e1071)
skewness(Customer_Age)
skewness(Dependent_count)
skewness(Months_on_book)
skewness(Total_Relationship_Count)
skewness(Months_Inactive_12_mon)
skewness(Contacts_Count_12_mon)
skewness(Credit_Limit)
skewness(Total_Revolving_Bal)
skewness(Avg_Open_To_Buy)
skewness(Total_Amt_Chng_Q4_Q1)
skewness(Total_Trans_Amt)
skewness(Total_Trans_Ct)
skewness(Total_Ct_Chng_Q4_Q1)
skewness(Avg_Utilization_Ratio)

###categorical variables - stacked bar plot
library(gridExtra)
library(grid)

p1 = ggplot(bankChurners,aes(x=factor(Gender),fill=Attrition_Flag))+geom_bar()+
  geom_text(aes(label=scales::percent(accuracy=0.1,..count../sum(..count..))),stat="count",position=position_stack())+
  scale_fill_manual(values=c("#5899F7","#F9CA55"))

p2 = ggplot(bankChurners,aes(x=factor(Education_Level),fill=Attrition_Flag))+geom_bar()+
  geom_text(aes(label=scales::percent(accuracy=0.1,..count../sum(..count..))),stat="count",position=position_stack())+
  scale_fill_manual(values=c("#5899F7","#F9CA55"))

p3 = ggplot(bankChurners,aes(x=factor(Marital_Status),fill=Attrition_Flag))+geom_bar()+
  geom_text(aes(label=scales::percent(accuracy=0.1,..count../sum(..count..))),stat="count",position=position_stack())+
  scale_fill_manual(values=c("#5899F7","#F9CA55"))

p4 = ggplot(bankChurners,aes(x=factor(Income_Category),fill=Attrition_Flag))+geom_bar()+
  geom_text(aes(label=scales::percent(accuracy=0.1,..count../sum(..count..))),stat="count",position=position_stack())+
  scale_fill_manual(values=c("#5899F7","#F9CA55"))

p5 = ggplot(bankChurners,aes(x=factor(Card_Category),fill=Attrition_Flag))+geom_bar()+
  geom_text(aes(label=scales::percent(accuracy=0.1,..count../sum(..count..))),stat="count",position=position_stack())+
  scale_fill_manual(values=c("#5899F7","#F9CA55"))

grid.arrange(p1,p2,p3,p4,p5,nrow = 2,top = "EDA - Categorical Variables")

#EDA-cat example
p_EDA_c = p3 
p_EDA_c + labs(x='Martial Status',y='Number of Clients')+my_theme


#dummify significant features
bankChurners$Gender = ifelse(Gender=='M',1,0)
bankChurners$Edu_Doc = ifelse(Education_Level=='Doctorate',1,0)
bankChurners$Income_6_8 = ifelse(Income_Category=='$60K - $80K',1,0)
attach(bankChurners)



########################## Feature Selection

##NUMERICAL
bankChurners_vars = bankChurners[,c(2,3,5,10,11,12,13,14,15,16,17,18,19,20,21)]
bankChurners_vars$Attrition_Flag = as.numeric(bankChurners_vars$Attrition_Flag)
colnames(bankChurners_vars)= c('y','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13','x14')
attach(bankChurners_vars)

###correlation map
install.packages("corrplot")
library(corrplot)
bankChurners_corre = cor(bankChurners_vars)

col_corre = colorRampPalette(c("#F96E03", "#F98919", "#F99D19", "#F9D119",
                               "#FDDBA7", "#FFFFFF", "#58D7F7", "#58C1F7",
                               "#58B4F7", "#5899F7", "#407FEB"))
c("#F9CA55","#5899F7")
corrplot(bankChurners_corre, method="pie",type='lower',tl.col='black',tl.cex = 1,col= col_corre(50))

###PCA
install.packages('clusterSim')
#this might take several minutes
library(clusterSim)
bankChurners_vars_std = data.Normalization(bankChurners_vars,type="n1",normalization="column")

install.packages('ggfortify') 
library(ggplot2) 
library(ggfortify)

pca=prcomp(bankChurners_vars_std, scale=TRUE) 
pca_map = autoplot(pca, data = bankChurners_vars_std, loadings = TRUE, loadings.label = TRUE,shape = 20,labelsize = 16,col=ifelse(bankChurners_vars$y==1,"#5899F7","#F9CA55"))

pca_map$layers[[2]]$aes_params$size = 0.7
pca_map$layers[[3]]$aes_params$size = 6
pca_map + my_theme

#principal components importance 
pve=(pca$sdev^2)/sum(pca$sdev^2) 
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1)) 
plot(cumsum(pve), ylim=c(0,1))

###numeric variable - simple logistic regression
bankChurners_vars$y = as.factor(bankChurners_vars$y)

lg_x1 =glm(y ~ x1, data = bankChurners_vars, family = 'binomial')
lg_x2 =glm(y ~ x2, data = bankChurners_vars, family = 'binomial')
lg_x3 =glm(y ~ x3, data = bankChurners_vars, family = 'binomial')
lg_x4 =glm(y ~ x4, data = bankChurners_vars, family = 'binomial')
lg_x5 =glm(y ~ x5, data = bankChurners_vars, family = 'binomial')
lg_x6 =glm(y ~ x6, data = bankChurners_vars, family = 'binomial')
lg_x7 =glm(y ~ x7, data = bankChurners_vars, family = 'binomial')
lg_x8 =glm(y ~ x8, data = bankChurners_vars, family = 'binomial')
lg_x9 =glm(y ~ x9, data = bankChurners_vars, family = 'binomial')
lg_x10 =glm(y ~ x10, data = bankChurners_vars, family = 'binomial')
lg_x11 =glm(y ~ x11, data = bankChurners_vars, family = 'binomial')
lg_x12=glm(y ~ x12, data = bankChurners_vars, family = 'binomial')
lg_x13 =glm(y ~ x13, data = bankChurners_vars, family = 'binomial')
lg_x14 =glm(y ~ x14, data = bankChurners_vars, family = 'binomial')

summary(lg_x1)$coefficients
summary(lg_x2)$coefficients
summary(lg_x3)$coefficients
summary(lg_x4)$coefficients
summary(lg_x5)$coefficients
summary(lg_x6)$coefficients
summary(lg_x7)$coefficients
summary(lg_x8)$coefficients
summary(lg_x9)$coefficients
summary(lg_x10)$coefficients
summary(lg_x11)$coefficients
summary(lg_x12)$coefficients
summary(lg_x13)$coefficients
summary(lg_x14)$coefficients

##Categorical
lg_c1 =glm(Attrition_Flag ~ Gender, data = bankChurners, family = 'binomial')
lg_c2 =glm(Attrition_Flag ~ Edu_Doc, data = bankChurners, family = 'binomial')
lg_c3 =glm(Attrition_Flag ~ Income_6_8, data = bankChurners, family = 'binomial')
summary(lg_c1)
summary(lg_c2)
summary(lg_c3)

##variables selected dataset
#categorical feature variable creation: line85
bankChurners_selected = bankChurners[,c(2,3,11,12,13,14,15,19,20,22,23)]
attach(bankChurners_selected)
colnames(bankChurners_selected)= c('y','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10')
attach(bankChurners_selected)




########################## Model Selection

#logistic regression

logit = glm(y~x1+x2+x3+x4+x6+x7+x9+x10,data=bankChurners_selected, family = 'binomial')
logit

library(caret)
pred_logit = predict(logit,bankChurners_selected)

confusionMatrix(as.factor(as.numeric(pred_logit>0.5)),reference = y)

library(boot)
mse=cv.glm(bankChurners_selected, logit, K=10)$delta[1]
mse

#lda
install.packages('MASS') 
install.packages('klaR') 
library(MASS) 
library(klaR)

lda = lda(y~x1+x2+x3+x4+x6+x7+x9+x10,data=bankChurners_selected, family = 'binomial')
pred_lda = predict(lda,bankChurners_selected)
table(y,pred_lda$class)
mean(pred_lda$class==bankChurners_selected$y)

qda = qda(y~x1+x2+x3+x4+x6+x7+x9+x10,data=bankChurners_selected, family = 'binomial')
pred_qda = predict(qda,bankChurners_selected)
table(y,pred_qda$class)
mean(pred_qda$class==bankChurners_selected$y)

#regression tree
library(tree)
library(rpart) 
library(rpart.plot)

tree = rpart(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10,control=rpart.control(cp=0.01))
plotcp(tree)
opt_cp = tree$cptable[which.min(tree$cptable[,'xerror']),'CP']
opt_cp

pred_tree = predict(tree, type='class')
confusionMatrix(pred_tree,reference = y)

#random forest
library(randomForest)
forest = randomForest(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, ntree=500, data=bankChurners_selected, importance=TRUE, na.action = na.omit, do.trace=50)

importance(forest)
varImpPlot(forest)

pred_forest = predict(forest,type='response')
confusionMatrix(pred_forest,reference = y)

#boosting
install.packages('gbm') 

library(gbm) 
set.seed(1)
boosted = gbm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10,distribution= "gaussian",n.trees=10000, interaction.depth=4)
    #using school's computer: R sesion abort when using distribution = 'bernoulli', code used as follow
    # bankChurners_selected$y = as.numeric(bankChurners_selected$y)
    # boosted = gbm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data=bankChurners_selected, distribution = 'bernoulli', cv.folds=5, n.trees=3000, verbose=F)
    # chose 'gaussian' and MSE instead

summary(boosted)

pred_gbm = predict(boosted,n.trees=1000,type='response')
mean((pred_gbm-as.numeric(y))^2)
1-0.03569332

##try remove less important variables
  #model No.7
boosted_1 = gbm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,distribution= "gaussian",n.trees=10000, interaction.depth=4)
pred_gbm_1 = predict(boosted_1,n.trees=1000,type='response')
mean((pred_gbm_1 -as.numeric(y))^2)
#MSE = 0.0356277
1- 0.0356277

boosted_2 = gbm(y~x1+x2+x3+x4+x5+x6+x7+x8,distribution= "gaussian",n.trees=10000, interaction.depth=4)
pred_gbm_2 = predict(boosted_2,n.trees=1000,type='response')
mean((pred_gbm_2 -as.numeric(y))^2)
#MSE = 0.03581183
1- 0.03581183

boosted_3 = gbm(y~x1+x2+x3+x4+x5+x6+x7,distribution= "gaussian",n.trees=10000, interaction.depth=4)
pred_gbm_3 = predict(boosted_3,n.trees=1000,type='response')
mean((pred_gbm_3 -as.numeric(y))^2)
#MSE = 0.04050957
1- 0.04050957


forest_1 = randomForest(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, ntree=500, data=bankChurners_selected, importance=TRUE, na.action = na.omit, do.trace=50)
pred_forest_1 = predict(forest_1,type='response')
confusionMatrix(pred_forest_1,reference = y)

forest_2 = randomForest(y~x1+x2+x3+x4+x5+x6+x7+x8, ntree=500, data=bankChurners_selected, importance=TRUE, na.action = na.omit, do.trace=50)
pred_forest_2 = predict(forest_2,type='response')
confusionMatrix(pred_forest_2,reference = y)


final_model = boosted_2
summary(final_model)

#important influencers review
my_theme_new =theme_bw() 
p6 = ggplot(bankChurners, aes(Total_Relationship_Count, fill = Attrition_Flag)) +geom_histogram(binwidth =1)+scale_fill_manual(values=c("#5899F7","#F9CA55"))+my_theme_new
p7 = ggplot(bankChurners, aes(Months_Inactive_12_mon, fill = Attrition_Flag)) +geom_histogram(binwidth =1)+scale_fill_manual(values=c("#5899F7","#F9CA55"))+my_theme_new
p8 = ggplot(bankChurners, aes(Credit_Limit, fill = Attrition_Flag)) +geom_histogram(binwidth =1000)+scale_fill_manual(values=c("#5899F7","#F9CA55"))+my_theme_new
p9 = ggplot(bankChurners, aes(Contacts_Count_12_mon, fill = Attrition_Flag)) +geom_histogram(binwidth =1)+scale_fill_manual(values=c("#5899F7","#F9CA55"))+my_theme_new

install.packages("ggpubr")
library(ggpubr)

ggarrange(p6,p7,p8,p9,nrow = 2,ncol=2,common.legend = TRUE,legend = 'right')










###### See Exhibit Extra - Violinplot in Python
if(FALSE) {
  import pandas as pd
  import seaborn as sns 
  
  bankChurners = pd.read_csv("P:/BankChurners.csv")
  
  bankChurners['Attrition_Flag'] = pd.get_dummies(bankChurners['Attrition_Flag']) #col32
  
  y = bankChurners['Attrition_Flag']
  x = bankChurners.iloc[:,[2,4,9,10,11,12,13,14,15,16,17,18,19,20]]
  data_n_2 = (x - x.mean()) / (x.std())              # standardization
  
  data = pd.concat([y,data_n_2.iloc[:,0:14]],axis=1)
  variable_list = ["x_" + str(i) for i in range(1, 15)]
  data = pd.melt(data,id_vars="Attrition_Flag",var_name='features',value_name='value')
  
  sns.set(rc={'figure.figsize':(9.7,6.27)})
  sns.set_style("whitegrid")
  colors = ["#5899F7","#F9CA55"]
  sns.set_palette(sns.color_palette(colors))
  
  
  sns.violinplot(x='features', y="value", hue="Attrition_Flag", data=data,split=True, inner="quart").set_xticklabels(variable_list)
  
}









