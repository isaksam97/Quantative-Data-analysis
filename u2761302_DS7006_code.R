#-----Section 01-------------------------------------------

# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
London.dis <- read.csv("data_final.csv", stringsAsFactors = FALSE)
head(London.dis)
str(London.dis)

 # check for missing data
apply(London.dis, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(London.dis, col = c("black", "grey"), legend = FALSE)
#London.dis <- na.omit(London.dis)

#checking the distribution of data 

#------------------
library(rcompanion)
str(London.dis)
pairs(~ Covid_Death_Total + Population_Density +Children+ Good.Health + Teenagers +Adults , data = London.dis,
  
      main = "multivariate scatterplot matrix")
#-----------------

# Load necessary library
install.packages("GGally")
library(GGally)

# Example data frame
# Replace this with your own data frame
data(London.dis)
df <- London.dis

# Generate a multivariate plot
ggpairs(df)
#-----------------


#----------------------

op <- par(mar = c(5, 8, 4, 2) + 0.1)
boxplot( Covid_Death_Total ~ Borough, xlab="Deaths", ylab="",
        col = "Bisque", horizontal = TRUE, las = 1, cex.axis = 0.7)
par(op)

#------------------
# select a dependent variable and independent variables
London.dis2 <- data.frame(London.dis$Covid_Death_Total,  London.dis$Population_Density, London.dis$Children, London.dis$Children,
                          London.dis$Teenagers
                          , London.dis$Adults
, London.dis$Middle.Age.Adults,London.dis$Senior
,London.dis$Good.Health,
                          London.dis$Very.good.health,London.dis$Bad.health,London.dis$Very.bad.health

,London.dis$No.Dimension,
                          London.dis$one.dimension,London.dis$two.dimensions
,London.dis$three.dimensions,London.dis$four.dimensions,
                          London.dis$Economically.active..excluding.full.time.students.,London.dis$Economically.inactive)
#-----------------------------------


boxplot(London.dis$Covid_Death_Total,  London.dis$Population_Density, London.dis$Children, London.dis$Children,
        London.dis$Teenagers
        , London.dis$Adults
        , London.dis$Middle.Age.Adults,London.dis$Senior
        ,London.dis$Good.Health,
        London.dis$Very.good.health,London.dis$Bad.health,London.dis$Very.bad.health
        
        ,London.dis$No.Dimension,
        London.dis$one.dimension,London.dis$two.dimensions
        ,London.dis$three.dimensions,London.dis$four.dimensions,
        London.dis$Economically.active..excluding.full.time.students.,London.dis$Economically.inactive,
        main = "Boxplot for the data frame", 
        xlab = "X-axis Label", 
        ylab = "Y-axis Label", 
        col = "lightblue", 
        border = "darkblue", 
        notch = TRUE)
#-----Section 02-------------------------------------------

# correlation matrix

# Correlations among numeric variables in
cor.matrix <- cor(London.dis2, use = "pairwise.complete.obs", method = "spearman")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)

# rename rows and columns
#dimnames(cor.df) <- list(c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"),
#                         c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"))
round(cor.df, 2)
#-----Section 04-------------------------------------------

London.dis3 <- data.frame( London.dis$Children,
                           London.dis$Senior
                           ,London.dis$Good.Health,
                           London.dis$Very.good.health,
                           London.dis$Bad.health,
                           London.dis$one.dimension,
                           London.dis$two.dimensions ,
                           London.dis$Economically.active..excluding.full.time.students.,
                           London.dis$Economically.inactive)
#-------------------
# Load necessary library (if not already installed, install it using install.packages("ggplot2"))
library(ggplot2)

# Example data (replace 'data' with your dataset)
# Assuming 'x_column' and 'y_column' are the names of your variables
London.dis3 <- data.frame(x_column = rnorm(100), y_column = rnorm(100))

# Create scatter plot
ggplot(data, aes(x = x_column, y = y_column)) +
  geom_point(color = "blue", size = 3) +  # Add scatter points
  labs(title = "Scatter Plot", x = "X-axis Label", y = "Y-axis Label") +  # Customize labels
  theme_minimal()  # Use a clean theme

#-----Section 03-------------------------------------------
# Load necessary library
install.packages("GGally")
library(GGally)

# Example data frame
# Replace this with your own data frame
data(London.dis3)
df <- London.dis3

# Generate a multivariate plot
ggpairs(df)

#-------------------------
library(psych)

pairs.panels(London.dis3, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman")

library(corrgram)
# corrgram works best with Pearson correlation
corrgram(London.dis3, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="London variables")
#--------------------------------------------------------------------------------
cor.matrix <- cor(London.dis3, use = "pairwise.complete.obs", method = "spearman")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)

# rename rows and columns
#dimnames(cor.df) <- list(c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"),
#                         c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"))
round(cor.df, 2)


#---------------------------------------
London.dis4 <- data.frame( London.dis$Children,
                           London.dis$Senior
                           ,London.dis$Good.Health,
                           London.dis$Very.good.health,
                         
                           London.dis$one.dimension,
                          
                         
                           London.dis$Economically.inactive)
#-----------------------------------
cor.matrix <- cor(London.dis4, use = "pairwise.complete.obs", method = "spearman")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)

# rename rows and columns
#dimnames(cor.df) <- list(c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"),
#                         c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"))
round(cor.df, 2)


#---------------------------------
London.dis5 <- data.frame( London.dis$Children,
                           London.dis$Senior
                           ,London.dis$Good.Health,
                           London.dis$Very.good.health,
                           London.dis$Bad.health,
                           
                           London.dis$one.dimension,
                           
                           London.dis$Economically.active..excluding.full.time.students.)
#--------------
cor.matrix <- cor(London.dis5, use = "pairwise.complete.obs", method = "spearman")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)

# rename rows and columns
#dimnames(cor.df) <- list(c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"),
#                         c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"))
round(cor.df, 2)
`#----------------------------------------
str (London.dis5)
pairs(~  Very.good.health + Bad.health,  data = London.dis5,
      main = "multivariate scatterplot matrix")
library(rcompanion)

pairs(~ Senior +Good.Health+ Very.good.health+Bad.health+one.dimension+Economically.active..excluding.full.time.students. ,  data = London.dis5,
      main = "multivariate scatterplot matrix")




#-----Section 05-------------------------------------------
shapiro.test(London.dis3)


# Extract numeric column names
London.dis2 <- colnames(df[, sapply(df, is.numeric)])


# Load necessary library
library(ggplot2)

# Example: Load a dataset (you can replace this with your own data)
# data <- read.csv("your_dataset.csv")

# Function to plot histograms for all numeric variables in a data frame
plot_histograms <- function(London.dis2) {
  numeric_columns <- sapply(London.dis2, is.numeric)  # Identify numeric columns
  numeric_data <- London.dis2[, numeric_columns]     # Subset only numeric columns
  
  for (col_name in names(numeric_data)) {
    # Create a histogram using ggplot2
    p <- ggplot(London.dis2, aes_string(x = col_name)) +
      geom_histogram(binwidth = 05, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency") +
      theme_minimal()
    
    print(p)  # Display the plot
  }
}

# Use the function on your dataset
# plot_histograms(data)
plot_histograms(London.dis3)
#------------------------------------------------
library(ggplot2)
library(gridExtra)  # For arranging multiple plots in a grid

# Function to create histograms for all numeric variables and arrange them in one output
plot_histograms_grid <- function(London.dis2) {
  numeric_columns <- sapply(London.dis2, is.numeric)  # Identify numeric columns
  numeric_data <- London.dis2[, numeric_columns]     # Subset only numeric columns
  
  plot_list <- list()  # Empty list to store plots
  
  for (col_name in names(numeric_data)) {
    # Create a histogram using ggplot2
    p <- ggplot(London.dis2, aes_string(x = col_name)) +
      geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency") +
      theme_minimal()
    
    plot_list[[col_name]] <- p  # Add each plot to the list
  }
  
  # Arrange all plots in a grid
  do.call(grid.arrange, c(plot_list, ncol = 2))  # Adjust ncol for the number of columns
}

# Use the function on your dataset
# plot_histograms_grid(data)
plot_histograms_grid(London.dis2)
#----------------------


#-----------------------------------------------------


#-----Section 06-------------------------------------------


#-----Section 07-------------------------------------------

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(London.dis5))

# Determine Number of Factors to Extract
library(nFactors)

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(London.dis5))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

#-----Section 08-------------------------------------------

# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(GPArotation)

# principal() uses a data frame or matrix of correlations
fit <- principal(London.dis5, nfactors=4, rotate="varimax")
fit

#-----Section 09-------------------------------------------

# weed out further variables after first factor analysis

London.dis5 <- London.dis5[!myvars]
str(London.dis2)
rm(myvars)

# get eigenvalues
ev <- eigen(cor(London.dis5))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

# Varimax Rotated Principal Components
# retaining 'nFactors' components
fit <- principal(London.dis5, nfactors=4, rotate="varimax")
fit
#-------------------------------
# create four variables to represent the rorated components
fit$scores
fit.data <- data.frame(fit$scores)

# check new variables are uncorrelated
cor.matrix2 <-cor(fit.data, method = "spearman")
#cor.matrix2 <-cor(fit.data, method = "pearson") fro better correlation anaylsis
cor.df2 <- as.data.frame(cor.matrix2)
round(cor.df2, 2)
#-----
str(London.dis5)

#-----Section 10-------------------------------------------
# Multiple linear Regression

# model with all variables
model2 <- lm(Covid_Death_Total ~ Children + Senior + Good.Health + Very.good.health +Bad.health + one.dimension 
             + Economically.active..excluding.full.time.students.  ,data = London.dis)
             
            
summary(model2)
# calculate variance inflation factor
library(car)
vif(model2)
sqrt(vif(model2)) > 2  # if > 2 vif too high
#here we feel that is model is not good bcoz there are lot of true
#-------------------------------
#this 4 variables are slected on the basis of dominance in section 9 factor analysis
model3 <- lm(Covid_Death_Total ~ Children + Senior + Good.Health + Economically.active..excluding.full.time.students.,,data = London.dis)
summary(model3)
sqrt(vif(model3)) > 2

#=---------------------------------
model4 <- lm(Covid_Death_Total ~ Children + Senior +Bad.health+ Economically.active..excluding.full.time.students.,,data = London.dis)
summary(model3)
sqrt(vif(model3)) > 2

#--------------------------------
library(RcmdrMisc)
library(relaimpo)
# forward stepwise selection
model4 <- stepwise(model3, direction = "forward")
summary(model4)
hist(model4$death)
rug(model4$residuals)
plot(model4$residuals ~ model4$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))
sqrt(vif(model4)) > 2
calc.relimp(model4, type = c("lmg"), rela = TRUE)

# use a stepwise approach to search for a best model
model5 <- stepwise(model2, direction = "forward")
summary(model5)
hist(model5$residuals)
rug(model5$residuals)
plot(model5$residuals ~ model5$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model5$residuals, "pnorm", mean(model5$residuals), sd(model5$residuals))
sqrt(vif(model5)) > 2
calc.relimp(model5, type = c("lmg"), rela = TRUE)

# test whether model2 and model5 are significantky different using F test
anova(model2, model5, test = "F")



