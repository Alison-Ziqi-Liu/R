films=read.csv("C:/Users/zliu91/Downloads/films_fall_2020.csv", header=TRUE)
films=films[-c(1:3)]
View(films)
attach(films)
require(psych)
library(car)
library(ggplot2)
require(olsrr)
require(lmtest)
require(plm)
library(boot)
library(splines)
library(plyr)
library(stargazer)


#distribution of variables - boxplots
boxplot(imdb_score,main="boxplot of IMDb score",col='#5899F7')
summary(imdb_score)
hist(imdb_score,col='#5899F7')

boxplot(budget_in_millions,main="boxplot of budgets",col='#5899F7')
summary(budget_in_millions)
boxplot.stats(budget_in_millions)$out

boxplot(year_of_release,main="boxplot of year of release",col='#5899F7')
summary(year_of_release)
boxplot.stats(year_of_release)$out

boxplot(month_of_release,main="boxplot of year of release",col='#5899F7')
summary(month_of_release)
boxplot.stats(month_of_release)$out

boxplot(duration_in_hours,main="boxplot of duration")
summary(duration_in_hours)
boxplot.stats(duration_in_hours)$out

boxplot(total_number_languages,main="boxplot of number of languages")
summary(total_number_languages)
boxplot.stats(total_number_languages)$out

boxplot(total_number_of_directors,main="boxplot of number of directors")
summary(total_number_of_directors)
boxplot.stats(total_number_of_directors)$out

boxplot(total_number_of_actors,main="boxplot of number of actors")
summary(total_number_of_actors)
boxplot.stats(total_number_of_actors)$out

boxplot(total_number_of_producers,main="boxplot of number of producers")
summary(total_number_of_producers)
boxplot.stats(total_number_of_producers)$out

boxplot(total_number_of_production_companies,main="boxplot of number of production companies")
summary(total_number_of_production_companies)
boxplot.stats(total_number_of_production_companies)$out

boxplot(total_number_of_production_countries,main="boxplot of number of production countries")
summary(total_number_of_production_countries)
boxplot.stats(total_number_of_production_countries)$out

#distribution of variables - histograms
hist(budget_in_millions, breaks=40, col="red", xlab="budget in millions")
hist(duration_in_hours, breaks=40, col="red", xlab="duration(in hours)")
hist(month_of_release, breaks=40, col="red", xlab="month of release")
hist(year_of_release, breaks=40, col="red", xlab="year of release")
hist(total_number_languages, breaks=40, col="red", xlab="total number of languages")
hist(total_number_of_production_companies, breaks=40, col="red", xlab="total number of production companies")
hist(total_number_of_actors,breaks=40,col="red",xlab="total number of actors")
hist(total_number_of_directors, breaks=40, col="red", xlab="number of directors")
hist(total_number_of_producers, breaks=40, col="red", xlab="number of producers")
hist(total_number_of_production_countries, breaks=40, col="red", xlab="number of production countries")

#calculate skewness
library(e1071)
skewness(budget_in_millions)
skewness(duration_in_hours)
skewness(month_of_release)
skewness(year_of_release)
skewness(total_number_languages)
skewness(total_number_of_directors)
skewness(total_number_of_actors)
skewness(total_number_of_producers)
skewness(total_number_of_production_companies)
skewness(total_number_of_production_countries)


#overview of categorical independent variables
count(main_director_name)
count(main_production_company)
count(editor_name)
count(main_production_country)
table(main_lang)
table(main_actor1_is_female)
table(main_actor2_is_female)
table(main_actor3_is_female)
table(main_director_name)
table(main_producer_name)
table(main_production_company)
table(main_production_country)

#scatter plot
plot(budget_in_millions,imdb_score)
plot(month_of_release,imdb_score)
plot(year_of_release,imdb_score)
plot(genre_action,imdb_score)
plot(genre_adventure,imdb_score)
plot(genre_animation,imdb_score)
plot(genre_biography,imdb_score)
plot(genre_comedy,imdb_score)
plot(genre_crime,imdb_score)
plot(genre_documentary,imdb_score)
plot(genre_drama,imdb_score)
plot(genre_family,imdb_score)
#total_number_of_actors
plot(total_number_of_actors,imdb_score,col='#5899F7')
#total_number_of_directors
plot(total_number_of_directors,imdb_score,col='#5899F7')
#total_number_of_producers
plot(total_number_of_producers,imdb_score)
#total_number_of_production_companies
plot(total_number_of_production_companies,imdb_score)
#total_number_of_production_countries
plot(total_number_of_production_countries,imdb_score)

#correlation matrix for all continuous variables
quantv1=films[,c(2,5,7,38,41,43,46,48)]
pairs.panels(quantv1)

#outlier test
lmbudget=lm(imdb_score~budget_in_millions)
outlierTest(lmbudget)
lmduration=lm(imdb_score~duration_in_hours)
outlierTest(lmduration)
lmlanguage=lm(imdb_score~total_number_languages)
outlierTest(lmlanguage)
lmactor=lm(imdb_score~total_number_of_actors)
outlierTest(lmactor)
lmdirector=lm(imdb_score~total_number_of_directors)
outlierTest(lmdirector)
lmproducer=lm(imdb_score~total_number_of_producers)
outlierTest(lmproducer)
lmcompany=lm(imdb_score~total_number_of_production_companies)
outlierTest(lmcompany)
lmcountry=lm(imdb_score~total_number_of_production_countries)
outlierTest(lmcountry)

#remove outliers
films=films[-c(582,641,907,2071),]
attach(films)

#correlation between imdb_score and each independent variable
cor(budget_in_millions,imdb_score,method="pearson")
cor(month_of_release,imdb_score,method="pearson")
cor(year_of_release,imdb_score,method="pearson")
cor(duration_in_hours,imdb_score,method="pearson")
cor(total_number_languages,imdb_score,method="pearson")
cor(genre_action,imdb_score,method="pearson")
cor(genre_adventure,imdb_score,method="pearson")
cor(genre_animation,imdb_score,method="pearson")
cor(genre_biography,imdb_score,method="pearson")
cor(genre_comedy,imdb_score,method="pearson")
cor(genre_crime,imdb_score,method="pearson")
cor(genre_documentary,imdb_score,method="pearson")
cor(genre_drama,imdb_score,method="pearson")
cor(genre_family,imdb_score,method="pearson")
cor(genre_fantasy,imdb_score,method="pearson")
cor(genre_filmnoir,imdb_score,method="pearson")
cor(genre_history,imdb_score,method="pearson")
cor(genre_horror,imdb_score,method="pearson")
cor(genre_music,imdb_score,method="pearson")
cor(genre_musical,imdb_score,method="pearson")
cor(genre_mystery,imdb_score,method="pearson")
cor(genre_realitytv,imdb_score,method="pearson")
cor(genre_romance,imdb_score,method="pearson")
cor(genre_scifi,imdb_score,method="pearson")
cor(genre_shortfilm,imdb_score,method="pearson")
cor(genre_sport,imdb_score,method="pearson")
cor(genre_thriller,imdb_score,method="pearson")
cor(genre_war,imdb_score,method="pearson")
cor(genre_western,imdb_score,method="pearson")
cor(main_actor1_is_female,imdb_score,method="pearson")
cor(main_actor2_is_female,imdb_score,method="pearson")
cor(main_actor3_is_female,imdb_score,method="pearson")
cor(total_number_of_actors,imdb_score,method="pearson")
cor(main_director_is_female,imdb_score,method="pearson")
cor(total_number_of_directors,imdb_score,method="pearson")
cor(total_number_of_producers,imdb_score,method="pearson")
cor(total_number_of_production_companies,imdb_score,method="pearson")
cor(total_number_of_production_countries,imdb_score,method="pearson")

#simple linear regression & linearity test
reg1=lm(imdb_score~month_of_release+year_of_release+duration_in_hours+total_number_of_actors
        +genre_action+genre_biography+genre_comedy+genre_drama+genre_history+genre_horror)
residualPlots(reg1)

#building & testing model
reg2=lm(imdb_score~month_of_release+year_of_release+poly(duration_in_hours,3)+total_number_of_actors
        +genre_action+genre_biography+genre_comedy+genre_drama+genre_history+genre_horror)
reg3=lm(imdb_score~month_of_release+year_of_release+poly(duration_in_hours,4)+poly(total_number_of_actors,2)
        +genre_action+genre_biography+genre_comedy+genre_drama+genre_history+genre_horror)

#splines: year_of_release
plot=ggplot(films,aes(y=imdb_score,x=year_of_release))+geom_point()
plot

linsplinreg=lm(imdb_score~bs(year_of_release,knots=c(1980),degree=2))
summary(linsplinreg)

plot=ggplot(films,aes(y=imdb_score,x=year_of_release))+geom_point()
plot+geom_smooth(method="lm",formula=y~bs(x,knots=c(1980),degree=2))

#final chosen model
reg4=lm(imdb_score~month_of_release+bs(year_of_release,knots=c(1980),degree=2)+poly(duration_in_hours,4)+poly(total_number_of_actors,2)
        +genre_action+genre_comedy+genre_drama+genre_horror+genre_history+genre_biography,data=films)
summary(reg4)
stargazer(reg4,type="html",single.row=TRUE)

#correlation matrix for variables in final model
quantv2=films[,c(3,4,5,8,11,12,15,19,20,38)]
pairs.panels(quantv2)

#determine optimal degrees
anova(reg1,reg2,reg3,reg4)

#double-check our chosen degrees
mse_list=rep()
for (a in 1:5) {
  for (b in 1:5){
    for (c in 1:5){
      fit=glm(imdb_score~month_of_release+bs(year_of_release,knots=c(1980),degree=a)+poly(duration_in_hours,b)+poly(total_number_of_actors,c)
                         +genre_action+genre_comedy+genre_drama+genre_horror+genre_history+genre_biography, data=films)
    mse=cv.glm(films,fit,K=10)$delta[1]
    mse_list=append(mse_list,mse)
    }
  }
}
mse_list
min(mse_list)


fit=glm(imdb_score~month_of_release+bs(year_of_release,knots=c(1980),degree=2)+poly(duration_in_hours,4)+poly(total_number_of_actors,2)
        +genre_action+genre_comedy+genre_drama+genre_horror+genre_history+genre_biography,data=films)
mse=rep(NA,10)
for (i in 1:10) {
  mse[i]=cv.glm(films,fit,K=10)$delta[1]
}
mse
mean(mse)

#correct heteroskedasticity
coeftest(reg4,vcov=vcovHC(reg4,type="HC1"))
summary(fit)

#non-constant-variance (ncv) test
plot(predict(reg),residuals(reg),col="blue")
abline(0,0,lty=2)
ncvTest(reg)















