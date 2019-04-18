install.packages("recommenderlab")

library(recommenderlab)


rm(list = ls())

### to create the given matrix
ratings = matrix(c(5,4,NA,7,1,6,NA,3,4,NA,7,3,4,3,3,4,NA,1,6,2,3,5,1,NA,2,NA,4,NA,4,5), nrow = 5)

###create a realRatingMatrix
R = as(ratings, "realRatingMatrix")



## A.)

# UBCF (Pearson Coefficient)

recommender_user = Recommender(R, method = 'UBCF', parameter = list(normalize = "center", method = "pearson"))
pred_ratings <- predict(recommender_user, R, type = "ratings")

getRatingMatrix(user_ratings)

#### we can predict the complete rating matrix which includes the original
### ratings by the user.
user_ratings <- predict(recommender_user, R, type="ratingMatrix" )
user_ratings

as(user_ratings, "matrix")






## B.)   IBCF (Cosine Similarity)

recommender_item = Recommender(R, method = "IBCF", parameter = list(method = "cosine"))
rating_item = predict(recommender_item, R, type = 'ratings')
getRatingMatrix(rating_item)


#### we can predict the complete rating matrix which includes the original
### ratings by the user.
item_ratings <- predict(recommender_item, R, type="ratingMatrix" )
item_ratings

as(item_ratings, "matrix")

