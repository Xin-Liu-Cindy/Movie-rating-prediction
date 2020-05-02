###### movie rating prediction ######


# packages
library(data.table)
library("tidyverse")
library(dplyr)
library(stringr)
library(ggpubr)
library(GGally)
library(caTools)
library(randomForest)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)


# import data
d <- fread('https://raw.githubusercontent.com/sundeepblue/movie_rating_prediction/master/movie_metadata.csv', stringsAsFactors = T)
dim(d)
str(d)
head(d)
summary(d)


# data processing
#### check missing value ####
table(is.na(d))
colSums(is.na(d))


#### imdb_score ####
summary(d$imdb_score)
hist(d$imdb_score)

imdb_rating <- function(imdb_score) {
  if (imdb_score >= 1 & imdb_score < 2.1) {
    return('1-2')
  } else if (imdb_score >= 2.1 & imdb_score < 3.1) {
    return('2-3')
  } else if (imdb_score >= 3.1 & imdb_score < 4.1) {
    return('3-4')
  } else if (imdb_score >= 4.1 & imdb_score < 5.1) {
    return('4-5')
  } else if (imdb_score >= 5.1 & imdb_score < 6.1) {
    return('5-6')
  } else if (imdb_score >= 6.1 & imdb_score < 7.1) {
    return('6-7')
  } else if (imdb_score >= 7.1 & imdb_score < 8.1) {
    return('7-8')
  } else if (imdb_score >= 8.1 & imdb_score < 9.1) {
    return('8-9')
  } else {
    return('9-10')
  }
}
d$imdb_rating <- factor(sapply(d$imdb_score, imdb_rating))
d %>%
  ggplot(aes(imdb_rating)) + 
  geom_bar(fill = 'dark grey') +
  ggtitle("Distribution of IMDB_score")


#### color ####
summary(d$color)

d$color = factor(ifelse(d$color == '', 'N/A', as.character(d$color)))
d %>%
  ggplot(aes(color)) + 
  geom_bar(fill = 'dark grey')


#### genres ####
summary(d$genres)
genres_new <- function(genres) {
  if (genres == 'Drama') {
    return('Drama')
  } else if (genres == 'Comedy') {
    return('Comedy')
  } else if (genres == 'Comedy|Drama') {
    return('Comedy|Drama')
  } else if (genres == 'Comedy|Drama|Romance') {
    return('Comedy|Drama|Romance')
  } else if (genres == 'Comedy|Romance') {
    return('Comedy|Romance')
  } else if (genres == 'Drama|Romance') {
    return('Drama|Romance')
  } else if (genres == 'Crime|Drama|Thriller') {
    return('Crime|Drama|Thriller')
  } else if (genres == 'Horror') {
    return('Horror')
  } else return('other')
}
d$genres_new <- factor(sapply(d$genres, genres_new))


#### language ####
summary(d$language)
d$language = factor(ifelse(d$language == '', 'N/A', as.character(d$language)))


#### country ####
summary(d$country)
d$country = factor(ifelse(d$country == '', 'N/A', as.character(d$country)))
country_new <- function(country) {
  if (country == 'Cameroon') {
    return('Africa')
  } else if (country == 'Egypt') {
    return('Africa')
  } else if (country == 'Kenya') {
    return('Africa')
  } else if (country == 'Libya') {
    return('Africa')
  } else if (country == 'Libya') {
    return('Africa')
  } else if (country == 'Nigeria') {
    return('Africa') 
  } else if (country == 'South Africa') {
    return('Africa') 
  } else if (country == 'Australia') {
    return('Oceania')   
  } else if (country == 'New Zealand') {
    return('Oceania')   
  } else if (country == 'China') {
    return('Asia')  
  } else if (country == 'Hong Kong') {
    return('Asia')  
  } else if (country == 'Japna') {
    return('Asia')   
  } else if (country == 'South Korea') {
    return('Asia') 
  } else if (country == 'Taiwan') {
    return('Asia') 
  } else if (country == 'Afghanistan') {
    return('Asia')    
  } else if (country == 'Kyrgyzstan') {
    return('Asia')  
  } else if (country == 'India') {
    return('Asia')  
  } else if (country == 'Parkistan') {
    return('Asia')  
  } else if (country == 'Philippines') {
    return('Asia')  
  } else if (country == 'Iran') {
    return('Asia')  
  } else if (country == 'Israel') {
    return('Asia')  
  } else if (country == 'United Arab Emirates') {
    return('Asia')  
  } else if (country == 'Cambodia') {
    return('Asia')  
  } else if (country == 'Indonesia') {
    return('Asia')  
  } else if (country == 'Thailand') {
    return('Asia')  
  } else if (country == 'Italy') {
    return('Europe') 
  } else if (country == 'Czech Republic') {
    return('Europe') 
  } else if (country == 'Hungary') {
    return('Europe') 
  } else if (country == 'Poland') {
    return('Europe') 
  } else if (country == 'Slovakia') {
    return('Europe') 
  } else if (country == 'Slovenia') {
    return('Europe')   
  } else if (country == 'Belgium') {
    return('Europe')   
  } else if (country == 'France') {
    return('Europe')    
  } else if (country == 'Germany') {
    return('Europe')
  } else if (country == 'Netherlands') {
    return('Europe') 
  } else if (country == 'Norway') {
    return('Europe')   
  } else if (country == 'Spain') {
    return('Europe')  
  } else if (country == 'Switzerland') {
    return('Europe')    
  } else if (country == 'UK') {
    return('Europe')    
  } else if (country == 'West Germany') {
    return('Europe')   
  } else if (country == 'Bulgaria') {
    return('Europe') 
  } else if (country == 'Georgia') {
    return('Europe') 
  } else if (country == 'Greece') {
    return('Europe')  
  } else if (country == 'Romania') {
    return('Europe')  
  } else if (country == 'Soviet Union') {
    return('Europe') 
  } else if (country == 'Denmark') {
    return('Europe') 
  } else if (country == 'Finland') {
    return('Europe')
  } else if (country == 'Iceland') {
    return('Europe')
  } else if (country == 'Ireland') {
    return('Europe')
  } else if (country == 'Russia') {
    return('Europe')
  } else if (country == 'Sweden') {
    return('Europe')
  } else if (country == 'Turkey') {
    return('Europe')
  } else if (country == 'Argentina') {
    return('America')
  } else if (country == 'Aruba') {
    return('America')
  } else if (country == 'Brazil') {
    return('America')
  } else if (country == 'Chile') {
    return('America')
  } else if (country == 'Colombia') {
    return('America')
  } else if (country == 'Bahamas') {
    return('America')
  } else if (country == 'Canada') {
    return('America')
  } else if (country == 'Dominican Republic') {
    return('America')
  } else if (country == 'Mexico') {
    return('America')
  } else if (country == 'Panama') {
    return('America')
  } else if (country == 'USA') {
    return('America')
  } else return('other')
}    
d$country_new <- factor(sapply(d$country, country_new))
d %>%
  ggplot(aes(country_new)) + 
  geom_bar(fill = 'dark grey')


#### int&num variables contain NA ####
d <- na.omit(d)


# EDA
#### imdb_rating vs color ####
summary(d$color)
g1 = d %>%
  count(color, imdb_rating) %>%
  filter(color != 'N/A') %>%
  ungroup %>%
  group_by(color)%>%
  mutate(pct = round(n/sum(n), 2))
g1 %>%
  ggplot(aes(x = color, y = pct)) +
  geom_bar(stat = 'identity', aes(fill = color)) +
  facet_grid(.~imdb_rating) +
  geom_text(aes(label = round(pct, 2), y = pct + 0.03)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_brewer()


#### imdb_rating vs genres ####
summary(d$genres_new)
g2 = d %>%
  count(genres_new, imdb_rating) %>%
  filter(genres_new != 'other') %>%
  ungroup %>%
  group_by(genres_new) %>%
  mutate(pct = round(n/sum(n), 2))
g2 %>%
  ggplot(aes(x= genres_new, y = pct)) +
  geom_bar(stat = 'identity', aes(fill = genres_new)) +
  facet_grid(.~imdb_rating) +
  geom_text(aes(label = round(pct, 2), y = pct + 0.03)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_brewer(palette = 'Set2')


#### imdb_rating vs country ####
summary(d$country_new)
g3 = d %>%
  count(country_new, imdb_rating) %>%
  filter(country_new != 'other') %>%
  ungroup %>%
  group_by(country_new) %>%
  mutate(pct = round(n/sum(n), 2))
g3 %>%
  ggplot(aes(x= country_new, y = pct)) +
  geom_bar(stat = 'identity', aes(fill = country_new)) +
  facet_grid(.~imdb_rating) +
  geom_text(aes(label = round(pct, 2), y = pct + 0.03)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_brewer(palette = 'Set3')


#### imdb_scores vs actor_facebook_likes ####
a1 <- d %>%
  ggplot(aes(x = imdb_score,
             y = actor_1_facebook_likes)) + 
  geom_point() + 
  scale_y_continuous(limits = c(-1, 640000)) +
  xlab('imdb_score') + 
  ylab('actor_1_facebook_likes')
a1
a2 <- d %>%
  ggplot(aes(x = imdb_score,
             y = actor_2_facebook_likes)) + 
  geom_point() +
  scale_y_continuous(limits = c(-1, 640000)) +
  xlab('imdb_score') + 
  ylab('actor_2_facebook_likes')
a2
a3 <- d %>%
  ggplot(aes(x = imdb_score,
             y = actor_3_facebook_likes)) + 
  geom_point() + 
  scale_y_continuous(limits = c(-1, 640000)) +
  xlab('imdb_score') + 
  ylab('actor_3_facebook_likes')
a3
ggarrange(a1, a2, a3, ncol=3, nrow=1)


###### imdb_scores vs gross & budget ####
b1 <- d %>%
  ggplot(aes(x = imdb_score,
             y = gross)) + 
  geom_point(color = 'tan') + 
  xlab('imdb_score') + 
  ylab('gross')
b1
b2 <- d %>%
  ggplot(aes(x = imdb_score,
             y = budget)) + 
  geom_point(color = 'dark grey') + 
  scale_y_continuous(limits = c(-1, 12215500000)) +
  xlab('imdb_score') + 
  ylab('budget')
b2
ggarrange(b1, b2, ncol=2, nrow=1)


#### imdb_score vs director & movie_facebook_likes ######
c1 <- d %>%
  ggplot(aes(x = imdb_score,
             y = director_facebook_likes)) + 
  geom_point(color = ' sky blue') + 
  xlab('imdb_score') + 
  ylab('director_facebook_likes')
c1
c2 <- d %>%
  ggplot(aes(x = imdb_score,
             y = movie_facebook_likes)) + 
  geom_point(color = 'sky blue') + 
  xlab('imdb_score') + 
  ylab('movie_facebook_likes')
c2
ggarrange(c1, c2, ncol=2, nrow=1)


#### imdb_score vs facenumber_in_poster & title_year ####
d1 <- d %>%
  ggplot(aes(x = imdb_score,
             y = facenumber_in_poster)) + 
  geom_point(color = 'yellow green') + 
  xlab('imdb_score') + 
  ylab('facenumber_in_poster')
d1
d2 <- d %>%
  ggplot(aes(x = title_year,
             y = imdb_score)) + 
  geom_point(color = 'violet') + 
  scale_x_continuous(limits = c(1916, 2016)) +
  xlab('title_year') + 
  ylab('imdb_score')
d2
ggarrange(d1, d2, ncol=2, nrow=1)


#### correlation matrix ####
str(d)
d$num_voted_users <- as.numeric(d$num_voted_users)
d$cast_total_facebook_likes <- as.numeric(d$cast_total_facebook_likes)
d$budget <- as.numeric(d$budget)
d$movie_facebook_likes <- as.numeric(d$movie_facebook_likes)
names(d)
d_cor <- select(d, 3,4,5,6,8,9,13,14,16,19,23,24,25,27,28,26)
cor <- cor(d_cor)
ggcorr(d_cor, label = TRUE, label_alpha = TRUE)


# Prediction: Random Forest
#### create training & testing dataset ####
df <- select(d, 4,5,6,8,16,23,25,29)
set.seed(123)
split = sample.split(df$imdb_rating, SplitRatio = 0.75)
training = subset(df, split == TRUE)
testing = subset(df, split == FALSE)


#### train model ####
set.seed(123)
model <- randomForest(imdb_rating~.,
                      data = training,
                      ntree = 3000,
                      importance = TRUE)
plot(model)


#### prediction ####
y_pred = predict(model, newdata = testing, type = 'response')
y_pred


#### confusion matrix ####
table = table(Predicted = y_pred, Actual = testing$imdb_rating)
table
print(paste('Random Forest Accuracy:', sum(diag(table)/sum(table))))


#### feature importance ####
varImpPlot(model, sort = TRUE, n.var = 7, main = "Feature impartance")


# Prediction: SVM
#### train model ####
classifier = svm(formula = imdb_rating~., 
                  data = training, 
                  type = 'C-classification', 
                  kernel = 'radial')
classifier


#### prediction ####
test_pred = predict(classifier, 
                     type = 'response', 
                     newdata = testing)
test_pred


#### confusion matrix ####
table2 = table(Predicted = test_pred, Actual = testing$imdb_rating)
table2
print(paste('SVM Accuracy:', sum(diag(table2)/sum(table2))))


# Prediction: Decision Tree
#### train model ####
tree <- rpart(imdb_rating~.,
                 data=training,
                 method = 'class')
print(tree)
summary(tree)
tree$variable.importance
rpart.plot(tree, 
           box.col=c("pink", "light blue"))


#### prediction ####
prediction <- predict(tree,
                       newdata=testing,
                       type = 'class')
prediction


#### confusion matrix ####
table3 = table(Predicted = prediction, Actual = testing$imdb_rating)
table3
print(paste('Decision Tree Accuracy:', sum(diag(table3)/sum(table3))))
