# 
# Analysis Phase for Ergonomics Research
# Josh Avery & Kathryn Perry
# Updated 4/23/2022
#

############################### LOAD DATA ######################################

# Load MASS after running dplyr 'select' statements since MASS masks 'select'
library(MASS)
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)

setwd("C:/Users/josha/Downloads/txst senior/Research")

# Load in the data
adj_ratings <- read_csv(file = "finalDataAdjustedAngles.csv")
colnames(adj_ratings) <- c("Subject","Time","LKnee_Angle","Clip","MoCap","Rater1","Rater2","Rater3","Rater4")
og_ratings <- read_csv(file = "finalDataOriginalAngles.csv")
colnames(og_ratings) <- c("Subject","Time","LKnee_Angle","Clip","MoCap","Rater1","Rater2","Rater3","Rater4")


# MoCap Ratings are off, fix them here:
og_ratings %>% 
  mutate(checkMoCap = 0) -> og_ratings

for(i in 1:dim(og_ratings)[[1]]){
  # pred angle
  if (og_ratings[i,3] > 100){
    # Pred rating
    og_ratings[i,10] <- 1
  } else if(og_ratings[i,3] > 93.33){
    og_ratings[i,10] <- 2
  } else if(og_ratings[i,3] > 86.66){
    og_ratings[i,10] <- 3
  } else if(og_ratings[i,3] > 80){
    og_ratings[i,10] <- 4
  } else{
    og_ratings[i,10] <- 5
  }
}

# Make Columns for raw rater error
og_ratings %>% 
  mutate(ER1 = abs(checkMoCap - Rater1), 
         ER2 = abs(checkMoCap - Rater2), 
         ER3 = abs(checkMoCap - Rater3), 
         ER4 = abs(checkMoCap - Rater4)) -> og_ratings

# Finalize Dataframe
og_ratings %>% 
  mutate(MoCap = checkMoCap,
         Rater1 = factor(Rater1, levels = c(1,2,3,4,5)),
         Rater2 = factor(Rater2, levels = c(1,2,3,4,5)),
         Rater3 = factor(Rater3, levels = c(1,2,3,4,5)),
         Rater4 = factor(Rater4, levels = c(1,2,3,4,5)),
         adjAngle = adj_ratings$LKnee_Angle) %>% 
  select(-checkMoCap) -> ratings


########################## ORDINAL LOGISTIC REGRESSION ######################### 

######## Predicting the Rater's Ratings w/ Angle

#### Rater 1

m1 <- polr(Rater1 ~ LKnee_Angle , data = ratings, Hess = TRUE)
summary(m1)

# Add p-values
ctable <- round(coef(summary(m1)), 6)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
(ctable <- cbind(ctable, "p value" = round(p, 4)))
ctable

# Predicted ratings
predict(m1)


# Add a column for predicted ratings
ratings %>% 
  mutate(R1PRED = 0) -> ratings

num <- 1
for(i in 1:54){
  if(is.na(ratings[i,"Rater1"])){
    ratings[i,"R1PRED"] <- NA
  } else{
    ratings[i,"R1PRED"] <- as.numeric(predict(m1))[num]
    num <- num + 1
  }
}



#### Rater 2

m2 <- polr(Rater2 ~ LKnee_Angle, data = ratings, Hess = TRUE)
summary(m2)

# Add p-values
ctable <- round(coef(summary(m2)), 6)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
(ctable <- cbind(ctable, "p value" = round(p, 4)))


# Predicted ratings
predict(m2)

# Add a column for predicted ratings
ratings %>% 
  mutate(R2PRED = 0) -> ratings

num <- 1
for(i in 1:54){
  if(is.na(ratings[i,"Rater2"])){
    ratings[i,"R2PRED"] <- NA
  } else{
    ratings[i,"R2PRED"] <- as.numeric(predict(m2))[num]
    num <- num + 1
  }
}



############## Rater 3


m3 <- polr(Rater3 ~ LKnee_Angle , data = ratings, Hess = TRUE)
summary(m3)

# Add p-values
ctable <- round(coef(summary(m3)), 6)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
(ctable <- cbind(ctable, "p value" = round(p, 4)))

# Add a column for predicted ratings
ratings %>% 
  mutate(R3PRED = 0) -> ratings

num <- 1
for(i in 1:54){
  if(is.na(ratings[i,"Rater3"])){
    ratings[i,"R3PRED"] <- NA
  } else{
    ratings[i,"R3PRED"] <- as.numeric(predict(m3))[num]
    num <- num + 1
  }
}



### Calcuate Errors
ratings %>% 
  mutate(R1PREDERR = as.numeric(Rater1) - R1PRED,
         R2PREDERR = as.numeric(Rater2) - R2PRED,
         R3PREDERR = as.numeric(Rater3) - R3PRED) -> ratings

# Basic Histograms of Prediction Errors
# Rater 1
ratings %>% 
  ggplot(aes(x = R1PREDERR)) +
  geom_histogram(bins = 7) +
  xlim(c(-3,3)) +
  ylim(c(0,17))

# Rater 2
ratings %>% 
  ggplot(aes(x = R2PREDERR)) +
  geom_histogram(bins = 7) +
  xlim(c(-3,3)) +
  ylim(c(0,17))

# Rater 3
ratings %>% 
  ggplot(aes(x = R3PREDERR)) +
  geom_histogram(bins = 7) +
  xlim(c(-3,3)) +
  ylim(c(0,17))

# Df for every angle in range & correct ratings to go with them
# for visualization purposes
AnglesAndPreds <- tibble(LKnee_Angle = 60:160,
                         Correct = c(rep.int(5,times = 21),
                                     rep.int(4,times = 6),
                                     rep.int(3,times = 7),
                                     rep.int(2,times = 6),
                                     rep.int(1,times = 61)))

# Add predictions for every angle & rater to the df
AnglesAndPreds %>% 
  mutate(pred1 = predict(m1, newdata = AnglesAndPreds),
         pred2 = predict(m2, newdata = AnglesAndPreds),
         pred3 = predict(m3, newdata = AnglesAndPreds)) -> AnglesAndPreds

# basic plots of the preditctions and the angles
AnglesAndPreds %>% 
  ggplot(aes(x = LKnee_Angle, y = pred1)) +
  geom_line()

AnglesAndPreds %>% 
  ggplot(aes(x = LKnee_Angle, y = pred2)) +
  geom_line()

AnglesAndPreds %>% 
  ggplot(aes(x = LKnee_Angle, y = pred3)) +
  geom_line()


# Make a combined df to plot everything
ratings %>% 
  select(LKnee_Angle, Rater1,Rater2,Rater3) %>% 
  mutate(Rater1 = as.numeric(Rater1),
         Rater2 = as.numeric(Rater2),
         Rater3 = as.numeric(Rater3))-> temp 
temp %>% 
  full_join(AnglesAndPreds, by = c("LKnee_Angle" = "LKnee_Angle"), keep = TRUE) %>% 
  arrange(LKnee_Angle.y) %>% 
  mutate(pred1 = as.numeric(pred1),
         pred2 = as.numeric(pred2),
         pred3 = as.numeric(pred3))-> plotting_df



# Plot of Rater 3's Ordinal regression against the defined correct ratings
plotting_df %>% 
  ggplot(aes()) +
  ggtitle("Rater 3")+
  geom_line(aes(x = LKnee_Angle.y, y = as.numeric(pred3)), color = "#00AFBB", size = .8,alpha = .9) +
  geom_point(aes(x = LKnee_Angle.x, y = Rater3), shape = 17, size = 2.2, color = "#FC4E07", alpha = .7)+
  geom_line(aes(x = LKnee_Angle.y, y = Correct), linetype = "dashed", size = .8, alpha = .5) +
  ylim(c(1,5))+
  labs(x = "Knee Angle", y = "Rating", color = "Legend") ->p3


  
# Plot of Rater 2's Ordinal regression against the defined correct ratings
plotting_df %>% 
  ggplot(aes()) +
  ggtitle("Rater 2")+
  ylab("Rating")+
  xlab("Knee Angle")+
  geom_line(aes(x = LKnee_Angle.y, y = as.numeric(pred2)), color = "#00AFBB", size = .8,alpha = .9) +
  geom_point(aes(x = LKnee_Angle.x, y = as.numeric(Rater2)), shape = 17, size = 2.2, color = "#FC4E07", alpha = .7)+
  geom_line(aes(x = LKnee_Angle.y, y = Correct), linetype = "dashed", size = .8, alpha = .5) +
  ylim(c(1,5)) ->p2

# Plot of Rater 1's Ordinal regression against the defined correct ratings
plotting_df %>% 
  ggplot(aes()) +
  ggtitle("Rater 1")+
  ylab("Rating")+
  xlab("Knee Angle")+
  geom_line(aes(x = LKnee_Angle.y, y = as.numeric(pred1)), color = "#00AFBB", size = .8,alpha = .9) +
  geom_point(aes(x = LKnee_Angle.x, y = as.numeric(Rater1)), shape = 17, size = 2.2, color = "#FC4E07", alpha = .7)+
  geom_line(aes(x = LKnee_Angle.y, y = Correct), linetype = "dashed", size = .8, alpha = .5) +
  ylim(c(1,5)) ->p1
 

# Combine the 3 plots into one graphic, horizontally
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)

g <- cbind(g1, g2, g3, size = "first")
g$heights <- unit.pmax(g1$heights,g2$heights, g3$heights)
grid.newpage()
grid.draw(g)
  

#### Try and predict the raters' *errors* (ended up not using this)

m4 <- polr(factor(ER1,levels = c(0,1,2,3,4)) ~ LKnee_Angle , data = ratings, Hess = TRUE)
summary(m4)

# Add Col to ratings
ratings %>% 
  mutate(ER1PRED = 0) -> ratings

num <- 1
for(i in 1:54){
  if(is.na(ratings[i,"ER1"])){
    ratings[i,"ER1PRED"] <- NA
  } else{
    ratings[i,"ER1PRED"] <- as.numeric(predict(m4))[num]
    num <- num + 1
  }
}

m5 <- polr(factor(ER2,levels = c(0,1,2,3,4)) ~ LKnee_Angle , data = ratings, Hess = TRUE)
summary(m5)

# Add Col to ratings
ratings %>% 
  mutate(ER2PRED = 0) -> ratings

num <- 1
for(i in 1:54){
  if(is.na(ratings[i,"ER2"])){
    ratings[i,"ER2PRED"] <- NA
  } else{
    ratings[i,"ER2PRED"] <- as.numeric(predict(m5))[num]
    num <- num + 1
  }
}

# Not significant at all
m6 <- polr(factor(ER3,levels = c(0,1,2,3,4)) ~ LKnee_Angle , data = ratings, Hess = TRUE)
summary(m6)

# Add Col to ratings
ratings %>% 
  mutate(ER3PRED = 0) -> ratings

num <- 1
for(i in 1:54){
  if(is.na(ratings[i,"ER3"])){
    ratings[i,"ER3PRED"] <- NA
  } else{
    ratings[i,"ER3PRED"] <- as.numeric(predict(m6))[num]
    num <- num + 1
  }
}


############### PREDICITNG ANGLE WITH MULTIPLE LINEAR REGRESSION ############### 

#### Individual Raters

# Bad ratings, even when we flip the angles around for them 
just1 <- lm(LKnee_Angle ~ Rater1, data = ratings)
summary(just1)

# the flipped version
adjust1 <- lm(adjAngle ~ Rater1, data = ratings,na.action = na.exclude)
summary(adjust1)

# Extremely Good!
just2 <- lm(LKnee_Angle ~ Rater2, data = ratings,na.action = na.exclude)
summary(just2)

# Strange results, but good!
just3 <- lm(LKnee_Angle ~ Rater3, data = ratings,na.action = na.exclude)
summary(just3)

#### Not Enough Data 
# just4 <- lm(LKnee_Angle ~ Rater4, data = ratings2)
# summary(just4)

### In Pairs:

# 1 and 2: Not great because of rater 1
first2 <- lm(LKnee_Angle ~ Rater1 + Rater2, data = ratings,na.action = na.exclude)
summary(first2)

# 1 and 3
oneandthree <- lm(LKnee_Angle ~ Rater1 + Rater3, data = ratings,na.action = na.exclude)
summary(oneandthree)

# 2 and 3
twoandthree <- lm(LKnee_Angle ~ Rater2 + Rater3, data = ratings,na.action = na.exclude)
summary(twoandthree)

# All 3
alltogethernow <- lm(LKnee_Angle ~ Rater1 + Rater2 + Rater3, data = ratings,na.action = na.exclude)
summary(alltogethernow)

# fits for the last one
ratings <- cbind(ratings, resid = resid(alltogethernow), fitted = fitted(alltogethernow))

# Basic plot of results from last one
plot(x = ratings$LKnee_Angle, y = ratings$fitted, xlim = c(55, 160))
abline(lm(ratings$fitted~ratings$LKnee_Angle), col = "blue")


################################ GGPLOTS #######################################

# Raw errors: Rater 1
ratings %>% 
  ggplot(aes(x = ER1)) +
  geom_bar(stat = "count", aes(fill = "#00AFBB"))+
  xlim(c(-.5, 4)) +
  ylim(c(0,30))+
  ggtitle("Rater 1")+
  labs(x = "Rating Error", y = "Frequency")+
  theme(legend.position="none")+
  scale_fill_manual(values=c("#00AFBB")) ->rEr1

# Raw errors: Rater 2
ratings %>% 
  ggplot(aes(x = ER2)) +
  geom_bar(stat = "count", aes(fill = "#E7B800"))+
  xlim(c(-.5, 4)) +
  ylim(c(0,30))+
  ggtitle("Rater 2")+
  labs(x = "Rating Error", y = "Frequency")+
  theme(legend.position="none")+
  scale_fill_manual(values=c("#52854C"))->rEr2

# Raw errors: Rater 3
ratings %>% 
  ggplot(aes(x = ER3)) +
  geom_bar(stat = "count", aes(fill = "#FC4E07"))+
  xlim(c(-.5, 4)) +
  ylim(c(0,30))+
  ggtitle("Rater 3")+
  labs(x = "Rating Error", y = "Frequency")+
  theme(legend.position="none")+
  scale_fill_manual(values=c("#D16103"))->rEr3

# Combine these 3 vertically
g1 <- ggplotGrob(rEr1)
g2 <- ggplotGrob(rEr2)
g3 <- ggplotGrob(rEr3)


g <- cbind(g1, g2, g3, size = "first")
g$heights <- unit.pmax(g1$heights, g2$heights, g3$heights)
grid.newpage()
grid.draw(g)

# Setting up for a graphic
artificialRatings <- tibble(Rater1 = factor(c(1,2,3,4,5)),
                            Rater2 = factor(c(1,2,3,4,5)),
                            Rater3 = factor(c(1,2,3,4,5)))

artificialRatings %>% 
  mutate(predAng1 = predict(just1, newdata = artificialRatings),
         predAng2 = predict(just2, newdata = artificialRatings),
         predAng3 = predict(just3, newdata = artificialRatings),
         predAng12 = predict(first2, newdata = artificialRatings),
         predAng13 = predict(oneandthree, newdata = artificialRatings),
         predAng23 = predict(twoandthree, newdata = artificialRatings),
         predAng123 = predict(alltogethernow, newdata = artificialRatings)) ->artificialRatings

# Graphic for best multiple linear regression wit only 1 rater
# You can switch the rater with the first ggplot line, replace '3' with the 
# desired rater number y argument only. 
artificialRatings %>% 
  ggplot(aes(x = Rater1, y = predAng3)) +
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=80),
            fill="#E5310E", alpha = .15)+
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=80,ymax=87),
            fill="#FFA73E", alpha = .1)+
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=87,ymax=93),
            fill="#FFE83E", alpha = .1)+
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=93,ymax=100),
            fill="#51CC23", alpha = .1)+
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=100,ymax=Inf),
            fill="green", alpha = .1)+
  geom_point(shape = 17, size  = 4, color = "black")+
  ylim(c(70,160)) +
  ggtitle("Best Predictor using Single Rater (#3)")+
  labs(x = "Given Rating" , y = "Predicted Angle")
  

### Plot the trend lines : Knee vs fitted 
ratings %>% 
  mutate(just2FIT = predict(just2),
         just3FIT = predict(just3),
         first2FIT = predict(first2),
         oneandthreeFIT = predict(oneandthree),
         twoandthreeFIT = predict(twoandthree),
         alltogethernowFIT = predict(alltogethernow)) ->ratings

### Actual angle vs predicted Angle for the subsets of raters that were 
### statistically significant

# Just 2
ratings %>% 
  ggplot(aes(x = LKnee_Angle, y = just2FIT)) +
  geom_rect(data=NULL,aes(xmin=75,xmax=110,ymin=140,ymax=155),
            fill="white", color = "black")+
  geom_point(shape = 17, size  = 2, color = "#FC4E07")+
  geom_smooth(method = 'lm', se = FALSE, color = "steelblue")+
  ylim(c(70,160)) +
  xlim(c(70,160))+
  ggtitle("Only Rater 2")+
  labs(x = "Actual Angle" , y = "Predicted Angle")+
  annotate("text", x = 92.5, y = 148, label = paste("y = ", round(lm(ratings$just2FIT~ratings$LKnee_Angle)$coefficients[[2]],2),
                                                    "x + ",round(lm(ratings$just2FIT~ratings$LKnee_Angle)$coefficients[[1]],2),
                                                    sep = '')) -> j2
# Just 3
ratings %>% 
  ggplot(aes(x = LKnee_Angle, y = just3FIT)) +
  geom_rect(data=NULL,aes(xmin=75,xmax=110,ymin=140,ymax=155),
            fill="white", color = "black")+
  geom_point(shape = 17, size  = 2, color = "#FC4E07")+
  geom_smooth(method = 'lm', se = FALSE, color = "steelblue")+
  ylim(c(70,160)) +
  xlim(c(70,160))+
  ggtitle("Only Rater 3")+
  labs(x = "Actual Angle" , y = "Predicted Angle")+
  annotate("text", x = 92.5, y = 148, label = paste("y = ", round(lm(ratings$just3FIT~ratings$LKnee_Angle)$coefficients[[2]],2),
                                                   "x + ",round(lm(ratings$just3FIT~ratings$LKnee_Angle)$coefficients[[1]],2),
                                                  sep = '')) -> j3

# 1 and 2
ratings %>% 
  ggplot(aes(x = LKnee_Angle, y = first2FIT)) +
  geom_rect(data=NULL,aes(xmin=75,xmax=110,ymin=140,ymax=155),
            fill="white", color = "black")+
  geom_point(shape = 17, size  = 2, color = "#FC4E07")+
  geom_smooth(method = 'lm', se = FALSE, color = "steelblue")+
  ylim(c(70,160)) +
  xlim(c(70,160))+
  ggtitle("Raters 1 and 2")+
  labs(x = "Actual Angle" , y = "Predicted Angle")+
  annotate("text", x = 92.5, y = 148, label = paste("y = ", round(lm(ratings$first2FIT~ratings$LKnee_Angle)$coefficients[[2]],2),
                                                    "x + ",round(lm(ratings$first2FIT~ratings$LKnee_Angle)$coefficients[[1]],2),
                                                    sep = '')) ->j12
# 2 and 3
ratings %>% 
  ggplot(aes(x = LKnee_Angle, y = oneandthreeFIT)) +
  geom_rect(data=NULL,aes(xmin=75,xmax=110,ymin=140,ymax=155),
            fill="white", color = "black")+
  geom_point(shape = 17, size  = 2, color = "#FC4E07")+
  geom_smooth(method = 'lm', se = FALSE, color = "steelblue")+
  ylim(c(70,160)) +
  xlim(c(70,160))+
  ggtitle("Raters 1 and 3")+
  labs(x = "Actual Angle" , y = "Predicted Angle")+
  annotate("text", x = 92.5, y = 148, label = paste("y = ", round(lm(ratings$oneandthreeFIT~ratings$LKnee_Angle)$coefficients[[2]],2),
                                                    "x + ",round(lm(ratings$oneandthreeFIT~ratings$LKnee_Angle)$coefficients[[1]],2),
                                                    sep = '')) ->j13
# 2 and 3
ratings %>% 
  ggplot(aes(x = LKnee_Angle, y = twoandthreeFIT)) +
  geom_rect(data=NULL,aes(xmin=75,xmax=110,ymin=140,ymax=155),
            fill="white", color = "black")+
  geom_point(shape = 17, size  = 2, color = "#FC4E07")+
  geom_smooth(method = 'lm', se = FALSE, color = "steelblue")+
  ylim(c(70,160)) +
  xlim(c(70,160))+
  ggtitle("Raters 2 and 3")+
  labs(x = "Actual Angle" , y = "Predicted Angle")+
  annotate("text", x = 92.5, y = 148, label = paste("y = ", round(lm(ratings$twoandthreeFIT~ratings$LKnee_Angle)$coefficients[[2]],2),
                                                    "x + ",round(lm(ratings$twoandthreeFIT~ratings$LKnee_Angle)$coefficients[[1]],2),
                                                    sep = '')) ->j23
# 1 2 and 3
ratings %>% 
  ggplot(aes(x = LKnee_Angle, y = alltogethernowFIT)) +
  geom_rect(data=NULL,aes(xmin=75,xmax=110,ymin=140,ymax=155),
            fill="white", color = "black")+
  geom_point(shape = 17, size  = 2, color = "#FC4E07")+
  geom_smooth(method = 'lm', se = FALSE, color = "steelblue")+
  ylim(c(70,160)) +
  xlim(c(70,160))+
  ggtitle("Raters 1, 2 and 3")+
  labs(x = "Actual Angle" , y = "Predicted Angle")+
  annotate("text", x = 92.5, y = 148, label = paste("y = ", round(lm(ratings$alltogethernowFIT~ratings$LKnee_Angle)$coefficients[[2]],2),
                                                    "x + ",round(lm(ratings$alltogethernowFIT~ratings$LKnee_Angle)$coefficients[[1]],2),
                                                    sep = '')) ->j123

# Combine all 6 plots into 1 graphic
title1=textGrob("Actual Knee Bend Angles vs Multiple Regression Predictions")
grid.arrange(
  j2, j3, j12, j13, j23, j123,
  nrow = 3,
  top = title1
)
