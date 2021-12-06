############################### Questions on Recommender System ############################### 
# 1.Implementing User-Based Recommender System: a.Load the 'MovieLense' dataset which is a part of recommenderlab Package
library(recommenderlab)
library(ggplot2)

data("MovieLense")
MovieLense@data
class(MovieLense@data)

vector_ratings = as.vector(MovieLense@data)
vector_ratings
unique(vector_ratings)

vector_ratings2 = vector_ratings[vector_ratings!=0]
vector_ratings2
unique(vector_ratings2)

table_ratings = table(vector_ratings)
table_ratings 

table_ratings2 = table(vector_ratings2)
table_ratings2

?qplot
qplot(vector_ratings)
qplot(vector_ratings2)

views_per_movie = colCounts(MovieLense)
views_per_movie
head(views_per_movie, 14)

table_views = data.frame(MovieName=names(views_per_movie),Views=views_per_movie)
table_views
head(table_views,4)

library(dplyr)

table_views2 = table_views %>% arrange(desc(Views))
table_views2
head(table_views2)

ggplot(table_views2[1:9,],aes(x=MovieName,y=Views))+geom_bar(stat = "identity")
ggplot(table_views2[1:9,],aes(x=MovieName,y=Views, fill=MovieName))+geom_bar(stat = "identity")
ggplot(table_views2[1:9,],aes(x=MovieName,y=Views, fill=Views))+geom_bar(stat = "identity")


average_rating = colMeans(MovieLense)
head(average_rating,14)

qplot(average_rating, col = "azure", bins=60)+ggtitle("Distribution of average rating")
qplot(average_rating, col = "azure", bins=200)+ggtitle("Distribution of average rating")

# b.From the 'MovieLense' dataset, extract only those observations where the users have seen atleast 100 movies & each movie has been seen atleast 120 times. Store the result in 'sample_movie'
sample_movie = MovieLense[rowCounts(MovieLense)>100, colCounts(MovieLense)>120]  

# c.Divide 'sample_movie' into train & test sets. The split ratio needs to be 70:30
split = sample(x=c(T,F),size=nrow(sample_movie),replace = T, prob = c(0.7,0.3))
train = sample_movie[split,]
test = sample_movie[!split,]

# d.Build the User-Based Collaborative Filtering model on train set & store the result in 'ubcf_model'
ubcf_model = Recommender(data = train,method="UBCF")

# e.Predict the values on the test set. The number of movies to be recommended is 10.
n_recommended_ubcf = 10
predicted_ubcf = predict(object=ubcf_model,newdata=test,n=n_recommended_ubcf)
predicted_ubcf
                                                  #Or#
predicted_ubcf2 = predict(object=ubcf_model,newdata=test,n=10)
predicted_ubcf2

# f.Recommend movies for user-3 & user-5
user3_movie_numbers = predicted_ubcf@items[[3]]
user3_movie_numbers

predicted_ubcf@itemLabels[user3_movie_numbers]

user5_movie_numbers = predicted_ubcf@items[[5]]
user5_movie_numbers

predicted_ubcf@itemLabels[user5_movie_numbers]


user3_movie_numbers2 = predicted_ubcf2@items[[3]]
user3_movie_numbers2

predicted_ubcf2@itemLabels[user3_movie_numbers2]

user5_movie_numbers2 = predicted_ubcf2@items[[5]]
user5_movie_numbers2

predicted_ubcf2@itemLabels[user5_movie_numbers2]




# 2.Implementing Item-Based Recommender System: a.On the same train set, build the Item-Based Collaborative Filtering Model & store the result in 'ibcf_model'
ibcf_model = Recommender(data = train,method="IBCF")

# b.Predict the values on test. The number of movies to be recommended is 12
n_recommended_ibcf = 12
predicted_ibcf = predict(object=ibcf_model,newdata=test,n=n_recommended_ibcf)

# c.Recommend movies for user-2 & user-4
user2_movie_numbers = predicted_ibcf@items[[2]]
user2_movie_numbers
predicted_ibcf@itemLabels[user2_movie_numbers]

user4_movie_numbers = predicted_ibcf@items[[4]]
user4_movie_numbers
predicted_ibcf@itemLabels[user4_movie_numbers]

