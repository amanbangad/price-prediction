
install.packages("leaps")
install.packages("dplyr")
install.packages("reshape")
install.packages("Hmisc")
install.packages("class")
install.packages("tree")
install.packages("caret")
install.packages("mlbench")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("DAAG")
install.packages("MASS")
install.packages("MatchIt")

library("leaps")
library("dplyr")
library("reshape")
library("Hmisc")
library("class")
library("tree")
library("caret")
library("mlbench")
library("gridExtra")
library("ggplot2")
library("DAAG")
library("MASS")
library("MatchIt")

normalize=function(data) { #function used for normalizing the covariates
  maximum=max(data)
  minimum=min(data)
  return((data-minimum)/(maximum-minimum))
}
#----------------------------Outlier function----------------------------------------------------
outlierKD <- function(dt, var) { #outlier function used for eliminating the outliers 
  var_name <- eval(substitute(var), eval(dt))   
  na1 <- sum(is.na(var_name)) #sum
  m1 <- mean(var_name, na.rm = T) #mean
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  boxplot(var_name, main = "With outliers") # boxplot with variable and outliers
  hist(var_name,
       main = "With outliers",
       xlab = NA,
       ylab = NA)
  outlier <- boxplot.stats(var_name)$out 
  mo <- mean(outlier) # mean outliers
  var_name <- ifelse(var_name %in% outlier, NA, var_name) # final variables taking into account the outliers
  boxplot(var_name, main = "Without outliers") #boxplot without the outliers
  hist(var_name,
       main = "Without outliers",
       xlab = NA,
       ylab = NA)
  title("Outlier Check", outer = TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n") #identified outliers
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name)) * #proportion of outliers
                                            100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n") # mean of outliers
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n") #mean without removing the outliers
  cat("Mean if we remove outliers:", round(m2, 2), "n") #mean if the outliers are removed
  dt[as.character(substitute(var))] <- invisible(var_name) 
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  cat("Outliers successfully removed", "n") # outliers are successfully removed
  par(mfrow = c(1, 1)) 
  return(invisible(dt))
}
#------------------------------------ outlier function ends here-------------------------------------------------------
predict.regsubsets = function(regfit.full, newdata, t) {
  form = as.formula(regfit.full$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(regfit.full, id = t) #obtain the coefficients of the model corresponding to t
  xvars = names(coefi)
  pred = mat[, xvars] %*% coefi
  return(pred)
}
####################################################
####################################################
####################################################
housing <- read.csv("housing_data.csv") #given housing data
housing$year <- substr(housing$date, start = 0, stop = 4)
#-----------------Cleaning and making sense of the housing data-------------------------------
housing <-
  housing[, !(
    colnames(housing) %in% c(
      "id",
      "date",
      "sqft_living15",
      "sqft_living",
      "sqft_lot15",
      "yr_renovated",
      "waterfront"
    )
  )]
housing$year <- as.integer(housing$year)
housing$price <- housing$price / 1000
outlierKD(housing, price)
outlierKD(housing, sqft_above)
outlierKD(housing, sqft_lot)
housing <- na.omit(housing)
#-----------------------------------------------------------------------------------------------
crime <- read.csv("crime_data.csv") # crime data
#------------------cleaning and making sense of the crime data------------------------------------
crime$date <- as.Date(crime$Occ.Datetime, "%m/%d/%Y")
crime$year <- as.character(crime$date)
crime$year <- substr(crime$year, start = 0, stop = 4)
crime_data <-
  data.frame(crime %>% group_by(Neighborhood, year) %>% summarise(count = n()))

x <- melt(crime_data)
crime_data <- cast(x, Neighborhood ~ year)

crime_data[crime_data$Neighborhood == "ALASKA JUNCTION", "zip"] = 98116
crime_data[crime_data$Neighborhood == "ALKI", "zip"] = 98116
crime_data[crime_data$Neighborhood == "BALLARD NORTH", "zip"] = 98117
crime_data[crime_data$Neighborhood == "BALLARD SOUTH", "zip"] = 98117
crime_data[crime_data$Neighborhood == "BELLTOWN", "zip"] = 20003
crime_data[crime_data$Neighborhood == "BITTERLAKE", "zip"] = 98102
crime_data[crime_data$Neighborhood == "CAPITOL HILL", "zip"] = 98102
crime_data[crime_data$Neighborhood == "BRIGHTON/DUNLAP", "zip"] = 38011
crime_data[crime_data$Neighborhood == "CENTRAL AREA/SQUIRE PARK", "zip"] =
  98122
crime_data[crime_data$Neighborhood == "CHINATOWN/INTERNATIONAL DISTRICT - EAST", "zip"] =
  98104
crime_data[crime_data$Neighborhood == "CHINATOWN/INTERNATIONAL DISTRICT - WEST", "zip"] =
  98104
crime_data[crime_data$Neighborhood == "CLAREMONT/RAINIER VISTA", "zip"] =
  98144
crime_data[crime_data$Neighborhood == "COLUMBIA CITY", "zip"] = 29201
crime_data[crime_data$Neighborhood == "COMMERCIAL HARBOR ISLAND", "zip"] =
  98108
crime_data[crime_data$Neighborhood == "DOWNTOWN COMMERCIAL", "zip"] = 22824
crime_data[crime_data$Neighborhood == "EASTLAKE - EAST", "zip"] = 98102
crime_data[crime_data$Neighborhood == "EASTLAKE - WEST", "zip"] = 98102
crime_data[crime_data$Neighborhood == "FAUNTLEROY SW", "zip"] = 98136
crime_data[crime_data$Neighborhood == "FIRST HILL", "zip"] = 98104
crime_data[crime_data$Neighborhood == "FREMONT", "zip"] = 98103
crime_data[crime_data$Neighborhood == "GEORGETOWN", "zip"] = 98108
crime_data[crime_data$Neighborhood == "HIGH POINT", "zip"] = 98126
crime_data[crime_data$Neighborhood == "HIGHLAND PARK", "zip"] = 98106
crime_data[crime_data$Neighborhood == "HILLMAN CITY", "zip"] = 98118
crime_data[crime_data$Neighborhood == "JUDKINS PARK/NORTH BEACON HILL", "zip"] =
  98144
crime_data[crime_data$Neighborhood == "LAKECITY", "zip"] = 98125
crime_data[crime_data$Neighborhood == "LAKEWOOD/SEWARD PARK", "zip"] = 98118
crime_data[crime_data$Neighborhood == "MADISON PARK", "zip"] = 98112
crime_data[crime_data$Neighborhood == "MADRONA/LESCHI", "zip"] = 98122
crime_data[crime_data$Neighborhood == "MAGNOLIA", "zip"] = 98199
crime_data[crime_data$Neighborhood == "MID BEACON HILL", "zip"] = 98108
crime_data[crime_data$Neighborhood == "MILLER PARK", "zip"] = 98112
crime_data[crime_data$Neighborhood == "MONTLAKE/PORTAGE BAY", "zip"] = 98102
crime_data[crime_data$Neighborhood == "MORGAN", "zip"] = 98122
crime_data[crime_data$Neighborhood == "MORGAN", "zip"] = 98126
crime_data[crime_data$Neighborhood == "MOUNT BAKER", "zip"] = 98144
crime_data[crime_data$Neighborhood == "NEW HOLLY", "zip"] = 98118
crime_data[crime_data$Neighborhood == "NORTH ADMIRAL", "zip"] = 98116
crime_data[crime_data$Neighborhood == "NORTH BEACON HILL", "zip"] = 98144
crime_data[crime_data$Neighborhood == "NORTH DELRIDGE", "zip"] = 98106
crime_data[crime_data$Neighborhood == "NORTHGATE", "zip"] = 98125
crime_data[crime_data$Neighborhood == "PHINNEY RIDGE", "zip"] = 98117
crime_data[crime_data$Neighborhood == "PIGEON POINT", "zip"] = 98106
crime_data[crime_data$Neighborhood == "PIONEER SQUAR", "zip"] = 98104
crime_data[crime_data$Neighborhood == "PIONEER SQUARE", "zip"] = 98104
crime_data[crime_data$Neighborhood == "QUEEN ANNE", "zip"] = 98109
crime_data[crime_data$Neighborhood == "RAINIER BEACH", "zip"] = 98178
crime_data[crime_data$Neighborhood == "RAINIER VIEW", "zip"] = 98178
crime_data[crime_data$Neighborhood == "ROOSEVELT/RAVENNA", "zip"] = 98115
crime_data[crime_data$Neighborhood == "ROXHILL/WESTWOOD/ARBOR HEIGHTS", "zip"] =
  98146
crime_data[crime_data$Neighborhood == "SANDPOINT", "zip"] = 98115
crime_data[crime_data$Neighborhood == "SLU/CASCADE", "zip"] = 98109
crime_data[crime_data$Neighborhood == "SOUTH BEACON HILL", "zip"] = 98118
crime_data[crime_data$Neighborhood == "SODO", "zip"] = 98134
crime_data[crime_data$Neighborhood == "SOUTH DELRIDGE", "zip"] = 98106
crime_data[crime_data$Neighborhood == "SOUTH PARK", "zip"] = 98108
crime_data[crime_data$Neighborhood == "UNIVERSITY", "zip"] = 98195
crime_data[crime_data$Neighborhood == "WALLINGFORD", "zip"] = 98103
crime_data[crime_data$Neighborhood == "GREENWOOD", "zip"] = 98103
crime_data[crime_data$Neighborhood == "GENESEE", "zip"] = 98116
crime_data[crime_data$Neighborhood == "DOWNTOWN COMMERCIAL", "zip"] = 98203
crime_data[crime_data$Neighborhood == "COMMERCIAL DUWAMISH", "zip"] = 98164
crime_data[crime_data$Neighborhood == "COLUMBIA CITY", "zip"] = 98118
crime_data[crime_data$Neighborhood == "BELLTOWN", "zip"] = 98121
crime_data[crime_data$Neighborhood == "BRIGHTON/DUNLAP", "zip"] = 98108
crime_data <- crime_data[!is.na(crime_data$zip),]
crime_data <- crime_data[,-1]
crime_data <-data.frame(crime_data %>% group_by(zip) %>% summarise_all(sum))
seattle <- housing[housing$zipcode %in% crime_data[, "zip"],]
#-------------integrating the crime data with the housing data-------------------------------------------------------------------------------
for (i in seq(nrow(seattle))) {
  seattle[i, "crime"] = crime_data[crime_data$zip == seattle[i, "zipcode"], paste("X", seattle[i, "year"], sep = "")]
}

seattle <-
  seattle[, !(colnames(seattle) %in% c("zipcode"))]

outlierKD(seattle, price)
outlierKD(seattle, sqft_above)
outlierKD(seattle, sqft_lot)
outlierKD(seattle, bedrooms)
seattle <- na.omit(seattle)

################################################

################################################
################################################
par(mfrow = c(1, 1))

correlations <- data.frame(cor(seattle))

seattle$bath_bed = seattle$bedrooms * seattle$bathrooms
seattle$bed_sqft = seattle$bedrooms * seattle$sqft_above


trainrow = floor(0.75 * nrow(seattle))
trainset = seattle[1:trainrow, ]
testset = seattle[trainrow:nrow(seattle), ]
################################################################
################################################################

#Plot price with other variables
p1 = ggplot(trainset, aes(y = price, x = bedrooms)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p1)
p2 = ggplot(trainset, aes(y = price, x = bathrooms)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p2)
p3 = ggplot(trainset, aes(y = price, x = floors)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p3)
p4 = ggplot(trainset, aes(y = price, x = sqft_lot)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p4)
p5 = ggplot(trainset, aes(y = price, x = view)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p5)
p6 = ggplot(trainset, aes(y = price, x = condition)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p6)
p7 = ggplot(trainset, aes(y = price, x = grade)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p7)
p8 = ggplot(trainset, aes(y = price, x = yr_built)) +
  geom_point(col = "blue", alpha = 0.04) +
  labs(x = "built year", y = "price")
#print(p8)
p9 = ggplot(trainset, aes(y = price, x = lat)) +
  geom_point(col = "blue", alpha = 0.04) +
  labs(x = "latitude", y = "price")
#print(p9)
p10 = ggplot(trainset, aes(y = price, x = long)) +
  geom_point(col = "blue", alpha = 0.04) +
  labs(x = "longitude", y = "price")
#print(p10)
p11 = ggplot(trainset, aes(y = price, x = sqft_above)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p11)
p12 = ggplot(trainset, aes(y = price, x = sqft_basement)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p12)
p13 = ggplot(trainset, aes(y = price, x = year)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p13)
p14 = ggplot(trainset, aes(y = price, x = crime)) +
  geom_point(col = "blue", alpha = 0.04)
#print(p14)

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, ncol =
               4)
#ggsave("scatter1.jpg")



################################################################
#########################################
# regression on all variables

reg_all = lm(price ~ ., data = trainset)
summary(reg_all)
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(reg_all, las = 1)
par(opar)

#######################################################################
################################
#cross validation on linear regression

func_str = ""
for (i in colnames(seattle)) {
  func_str = paste(func_str, i, sep = "+")
}
func_str = substr(func_str, start = 8, stop = nchar(func_str))

cv_data = CVlm(
  data = trainset,
  form.lm = formula(
    price ~ bedrooms + bathrooms + sqft_lot + floors + view + condition + grade +
      sqft_above + sqft_basement + yr_built + lat + long + year + crime + bath_bed +
      bed_sqft
  ),
  m = 10
)

attr(cv_data, "ms")

#######################################################################
#######################################################################

#Residual Analysis
reg_all_residuals = resid(reg_all)

pp1 = ggplot(trainset, aes(y = reg_all_residuals, x = bedrooms)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp1)
pp2 = ggplot(trainset, aes(y = reg_all_residuals, x = bathrooms)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp2)
pp3 = ggplot(trainset, aes(y = reg_all_residuals, x = sqft_lot)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp3)
pp4 = ggplot(trainset, aes(y = reg_all_residuals, x = floors)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp4)
pp5 = ggplot(trainset, aes(y = reg_all_residuals, x = view)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp5)
pp6 = ggplot(trainset, aes(y = reg_all_residuals, x = condition)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp6)
pp7 = ggplot(trainset, aes(y = reg_all_residuals, x = grade)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp7)
pp8 = ggplot(trainset, aes(y = reg_all_residuals, x = sqft_above)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp8)
pp9 = ggplot(trainset, aes(y = reg_all_residuals, x = sqft_basement)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp9)
pp10 = ggplot(trainset, aes(y = reg_all_residuals, x = yr_built)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp10)
pp11 = ggplot(trainset, aes(y = reg_all_residuals, x = lat)) +
  geom_point(col = "black", alpha = 0.04) +
  labs(x = "sqft_living15", y = "residuals")
#print(pp11)
pp12 = ggplot(trainset, aes(y = reg_all_residuals, x = long)) +
  geom_point(col = "black", alpha = 0.04) +
  labs(x = "sqft_lot15", y = "residuals")
#print(pp12)
pp13 = ggplot(trainset, aes(y = reg_all_residuals, x = year)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp13)
pp14 = ggplot(trainset, aes(y = reg_all_residuals, x = crime)) +
  geom_point(col = "black", alpha = 0.04)
#print(pp14)
grid.arrange(pp1,
             pp2,
             pp3,
             pp4,
             pp5,
             pp6,
             pp7,
             pp8,
             pp9,
             pp10,
             pp11,
             pp12,
             pp13,
             pp14,
             ncol = 4)
#ggsave("residual_scatter.jpg")

###########################################################

##################################################################
##################################################################

control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3)
# train the model
model <-
  train(
    price ~ .,
    data = trainset,
    method = "lm",
    preProcess = "scale",
    metric = "RMSE",
    trControl = control
  )
# estimate variable importance
importance <- varImp(model, scale = FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
####################################################

# regression on selected variables given through resampling

reg_var = lm(price ~ grade + yr_built + lat + sqft_basement + view + condition + year + long + crime, data = trainset)
summary(reg_var)
pre_final = predict(reg_var,testset)
actual = testset$price
mean((actual - pre_final)^2)
#plot(actual, pre_final)

finalplot = ggplot(testset, aes(y=pre_final, x=actual)) +
  geom_point(alpha = 0.4) +
  geom_smooth()
print(finalplot)
################################################

# cross-validation on the above model
cv_data = CVlm(
  data = trainset,
  form.lm = formula(
    price ~ grade + yr_built + lat + sqft_basement + view + condition + year + long + crime
  ),
  m = 10
)

attr(cv_data, "ms")

#####################################################################
####################################################################
# selecting covariates using recursive feature elimination and random forests
control <- rfeControl(functions = rfFuncs,
                      method = "cv",
                      number = 10)
# run the RFE algorithm
results <-
  rfe(testset[, 2:ncol(testset)],
      testset[, 1],
      sizes = c(1:8),
      rfeControl = control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type = c("g", "o"))
#####################################################
#performing regression on selected variables
reg_subset = lm(price ~ sqft_basement + long + grade + sqft_above + sqft_lot +
                  lat + crime + bed_sqft,
                trainset)
summary(reg_subset)

######################################################

# cross-validation on the above model
cv_data = CVlm(
  data = trainset,
  form.lm = formula(
    price ~ sqft_basement + long + grade + sqft_above + sqft_lot +
      lat + crime + bed_sqft
  ),
  m = 10
)

attr(cv_data, "ms")


###########################################################
####################################################################
##### KNN 
seattle.new=seattle[,-c(2,3,4,5,6,7,11,12,16)] #taking only the significant variables in the Knn model
housing_new=as.data.frame(lapply(seattle.new[,2:8],normalize)) #normalizing the covariates 
frow=floor(0.75*nrow(housing_new)) # 75% of the total number of obervations
ffinal=nrow(housing_new) #total number of observations
housing_new_train=housing_new[1:frow,] #train data with the covariates
housing_new_test=housing_new[(frow+1):(ffinal),] #test data with the covariates
housing_new_train_labels=seattle.new[1:frow,1] #train price data
housing_new_test_labels=seattle.new[(frow+1):ffinal,1] #test price data
k.fold=10 # 10 fold cross validation variable
meaan=array(NA,dim=c(k.fold))
xx=array(NA,dim=c(7))
#------cross validation----------------------------------------
folds=sample(1:k.fold,nrow(housing_new_train),replace=TRUE) 
for(k1 in 1:length(housing_new)){
  for(j in 1:k.fold){
    pred.knn=knn(train=housing_new_train[folds!=j,],test=housing_new_train[folds==j,],cl=housing_new_train_labels[folds!=j],k=k1)  
    pred.new=as.character(pred.knn)
    pred.new.1=as.integer(pred.new)
    meaan[j]=(mean((pred.new.1-housing_new_train_labels[folds==j])^2))
  }
  xx[k1]=mean(meaan) #mean for each value of k 
  print(xx)
  meaan=array(NA,dim=c(k.fold))
}  
k.final=which.min(xx) #optimal k value
xx[k.final] #20462

#---------------------------------------------------------------------------------------------------
quartile=quantile(seattle.new$price,c(1/3,2/3)) #quartile range according to which the prices will be divided as inexpensive reasonable and expensive
lower=quartile[1] # 33.33 percentile range
upper=quartile[2] # 66.67 percentile range
#---loop assigning the price values--------------------------------
for (i in 1:nrow(seattle.new)){
  if (seattle.new$price[i]< lower)
  {
    seattle.new$price[i]="inexpensive"
  }
  else if (seattle.new$price[i]>upper){
    seattle.new$price[i]="expensive" 
  }
  else
    seattle.new$price[i]="reasonable"
}
View(seattle.new)
summary(seattle.new$price)
#---------------------------------------------------------------------------
housing_new_train_labels=seattle.new$price[1:frow] #train data
housing_new_test_labels=seattle.new$price[(frow+1):ffinal] #test data

xx=array(NA,dim=c(10))
xx1=array(NA,dim=c(7))
k.fold=10
#--------cross validation for the given problem----------------------------
folds=sample(1:k.fold,nrow(housing_new_train),replace=TRUE)
for(k1 in 1:length(housing_new)){
  for(j in 1:k.fold){
    pred.knn=knn(train=housing_new_train[folds!=j,],test=housing_new_train[folds==j,],cl=housing_new_train_labels[folds!=j],k=k1)  
    table.1=table(pred.knn,housing_new_train_labels[folds==j])
    xx[j]=(1/3)*(table.1[1,1]/sum(table.1[1,])+table.1[2,2]/sum(table.1[2,])+table.1[3,3]/sum(table.1[3,]))
  }
  xx1[k1]=mean(xx)
  print(xx1)
  xx=array(NA,dim=c(10))
}
which.max(xx1)
#------------------------------------------------------------------------------------
# sample reasonably optimal case
pred.knn=knn(train=housing_new_train[folds!=1,],test=housing_new_train[folds==1,],cl=housing_new_train_labels[folds!=1],k=which.max(xx1))
table.1=table(pred.knn,housing_new_train_labels[folds==1]) #sample table
table.1
sample.val=(1/3)*(table.1[1,1]/sum(table.1[1,])+table.1[2,2]/sum(table.1[2,])+table.1[3,3]/sum(table.1[3,])) #sample hit rate of 68.4%
#----------------------------------------------------------------------------------------------------------


#########################################################
#applying decision tree on the dataset
tree_price = tree(price ~ ., trainset)
plot(tree_price)
cv.price = cv.tree(tree_price)
text(tree_price, pretty = 0)
plot(cv.price$size, cv.price$dev, type = 'b')
prune.price = prune.tree(tree_price, best = 9)
plot(prune.price)
text(prune.price, pretty = 0)
plot(prune.price)
text(prune.price, pretty = 0)
yhat=predict(prune.price,newdata=trainset)
actualprice=trainset$price
mean((yhat-actualprice)^2)

###########################################################
#matchit on crime data

trainset$crime_div <- ifelse(trainset$crime > 1816, c(1), c(0))
mod_match <-
  matchit(
    crime_div ~ sqft_lot + yr_built + view + sqft_above + sqft_basement + bed_sqft + grade,
    method = "nearest",
    data = trainset
  )
dta_m <- match.data(mod_match)

t.test(dta_m$price[dta_m$crime_div==1],
       dta_m$price[dta_m$crime_div==0],paired=TRUE)
#regression with crime keeping everything else constant
lm_matchit2 <- lm(price ~ crime, data = dta_m)
summary(lm_matchit2)
#regression with all variables
lm_reg_all <- lm(price ~ ., data = dta_m)
summary(lm_reg_all)
#running regression without intercept
lm_wto = lm(price ~ . - 1, trainset)
summary(lm_wto)

View(seattle)
