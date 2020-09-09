# load packages
library(rgl)
library(readxl)
library(FNN)

# Import dataset
house <- read_excel('C:\\Users\\mboke\\Documents\\2020\\Fall Semester\\Machine Learning I\\Realestate.xlsx')

# View the neames of the columns
names(house)

# Choose two attributes as predictors of quality: age and distance to nearest MRT station
age <- house$`X2 house age`
mrt <- house$`X3 distance to the nearest MRT station`

# Extract target variable
price <- house$`Y house price of unit area`

# 3d plot 
par3d(windowRect=c(800,33,1500,800)); rgl.bringtotop(stay=T) # Positions the rgl window on my computer.
tmp1 <- plot3d(age,mrt,price,type="p",box=T,axes=T)

# scale age and mrt
age_scaled <- (age-mean(age))/sd(age)
mrt_scaled <- (age-mean(age))/sd(age)

# Create training dataframe
train_df <- data.frame(z_age = age_scaled, z_mrt = mrt_scaled)

# Create a 50x50 grid for the predictions
n <- 50
x1g <- (0:(n-1))/(n-1)*(max(age_scaled)-min(age_scaled))+min(age_scaled)
x1g <- matrix(rep(x1g,n),ncol=n)
x2g <- (0:(n-1))/(n-1)*(max(mrt_scaled)-min(mrt_scaled))+min(mrt_scaled)
x2g <- matrix(rep(x2g,n),ncol=n,byrow=T)
test_df <- data.frame(weight=as.vector(x1g),horsepower=as.vector(x2g))



# Predictions -------------------------------------------------------------


# Generate predictions
out <- knn.reg(train=train_df,test=test_df,y=price,k=20)
ypred <- matrix(out$pred,nrow=n)

# Plot the values again with the prediction surface
par3d(windowRect=c(800,33,1500,800)); rgl.bringtotop(stay=T) # Positions the rgl window on my computer.
tmp1 <- plot3d(age_scaled,mrt_scaled,price,type="p",box=T,axes=T)

# This shows the 50x50 grid of points for the prediction
tmp1 <- plot3d(x1g,x2g,matrix(rep(0,n*n),nrow=n),add=T)
#Prediction surface:
persp3d(x1g,x2g,ypred,front="lines",back="lines",add=T)


play3d(spin3d(rpm=3))

