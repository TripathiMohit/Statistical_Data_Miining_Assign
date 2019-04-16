install.packages("recommenderlab")


library(recommenderlab)

rm(list = ls())

citation("recommenderlab")
ls("package:recommenderlab")


data("MovieLense")


####### part a.)
dat <- MovieLense
head(dat)
dim(dat) ### 943 users and 1664 movies


##create a realRatingMatrix
R <- as(dat, "realRatingMatrix")
class(R)


### get the rating matrix
dim(getRatingMatrix(R))
getRatingMatrix(R)[1:3,1:15]



### normalise the rating matrix
R_normalise <- normalize(R)
R_normalise


##### visualize the ratings and normalize ratings
quartz()
image(R_normalise[1:100,1:100], main= "Normalised Ratings")

quartz()
image(R[1:100,1:100], main= "Raw Ratings")


### get the normalize rating matrix
getRatingMatrix(R_normalise)[1:3,1:15]

### denormalize
R_denormalize <- denormalize(R_normalise)
getRatingMatrix(R_denormalize)[1:3,1:15]

### create a recommender system
#?recommenderRegistry
recommenderRegistry$get_entries(dataType="realRatingMatrix")
##Recommender of type `UBCF' for `realRatingMatrix' he parameters are UBCF 
##and Cosine similarity. We take 50 nearest neighbours
recommender_user <- Recommender(R, method = "UBCF", param=list(method="Cosine",nn=50)) 
names(getModel(recommender_user))


#####Obtain top 5 recommendations for first 3 users entry in dataset
Top_5_pred = predict(recommender_user, R[1:3], n=5)

#Convert the recommendations to a list
Top_5_List = as(Top_5_pred, "list")
Top_5_List

#### predicts ratings for first 3 users.we can get the complete rating matrix which includes the original
### ratings by the user.
user_ratings <- predict(recommender_user, R[1:3], type="ratingMatrix" )
user_ratings

as(user_ratings, "matrix")[,1:15]




####### part b.)


## evaluation
scheme <- evaluationScheme(R, method="cross-validation", k=5, given=3, goodRating=3)
scheme

results <- evaluate(scheme, method="UBCF", type = "ratings", n=c(1,3,5,10,15,20))
results


getConfusionMatrix(results)



avg(results) ### Average RMSE 1.16





