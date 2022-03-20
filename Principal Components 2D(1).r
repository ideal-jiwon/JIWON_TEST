# Principal Components 2D 
# Illustrates centering and rotation of 2D data
Thanks to Dr. Carr for providing resources for this R code

Due:  3 points
2.  Juxtaposed plot
4.  Juxtaposed plot 
5.  Second Biplot

# 0. Setup

library(MASS)
library(ggplot2)
library(gridExtra)
source("hw.r")

hwThemeSmallText <- hw +
  theme(
  axis.text=element_text(size=rel(.8)),
  axis.title=element_text(size=rel(.8)),
  plot.title=element_text(size=rel(.8))
  )

# 1. Read data and check for missing values
  
mfcancer <- read.csv(file="US_CancerMortality_byGender.csv",
  header=TRUE, as.is=TRUE)
head(mfcancer)
tail(mfcancer)
any(is.na(mfcancer))

# Make a data frame for 2007 
# with male and female columns
# in separate columns

# Get the rate for column X2007
rate <- mfcancer[, "X2007"]

# Make a logical vector with TRUE for "Male"
# FALSE otherwise (Females0
male <- mfcancer[, "Gender"]=="Male"

# column bind the male and female rates into
# two column matrix
mat <- cbind(rate[male], rate[!male])

# Add state postal codes as rownames 
# Male and Female as column names
rownames(mat) <- mfcancer$State[1:51]
colnames(mat) <- c("Male","Female")
mat

# Make a data.frame
df <- as.data.frame(mat)
head(df)

# 2. Center the variables 
#    and compare plots of the data and
#    centered data.  Just the axis scales
#    should look different  

# The scale function 
#  scale(x,cen=TRUE,scale=TRUE) works on a numeric matrix

# By default it subtracts the mean from each column
# and then divides each column by its standard deviation.

matCen <- scale(mat,scale=FALSE)
dfCen <- as.data.frame(matCen)
head(dfCen)

# Save the means for reference lines
maleM <- mean(df$Male)
femaleM <- mean(df$Female)

# Produce female rate versus male rate scatterplots
# Produce both uncentered and centered scatterplot objects
# Juxtapose the plots using grid.arrange()

title <-paste("State Cancer Mortality Rates 2007",
        "\nDeaths Per 100,000",sep="")   

p <- ggplot(df,aes(x=Male,y=Female))+
     geom_hline(yintercept=femaleM,color='blue')+ 
     geom_vline(xintercept=maleM,color='blue')+
     geom_point(fill="red",shape=21)+
     labs( title=title)+hwThemeSmallText

pCen<- ggplot(dfCen,aes(x=Male,y=Female))+
       geom_hline(yintercept=0,color='blue')+ 
       geom_vline(xintercept=0,color='blue')+
       geom_point(fill="red",shape=21)+
       labs(x="Male - Mean(Male)",
         y="Female - mean(Female)",
         title=title)+hwThemeSmallText

windows(width=6, height=3)
grid.arrange(p,pCen,ncol=2)

# The plots look the excep for grid line
# labels and variable labels.  

# Some times grid.arrange is not updated
# Hand juxtaposing the two plots in the 
# assignment document is okday.  

#windows(w=3,h=3)
# p
#
# windows(w=3,h=3)
# pCen


# 3. Principal components and rotation 
#
# Below we keep the original units of
# measure by setting scale to FALSE
# Otherwise after centering the
# variables, prcomp will divide them by 
# their standard deviations so the
# variables being rotated are unitless.  
#
# In most situations the variables
# have different units of measure
# and researchers usually choose to divide the
# variables by their standard deviation
# to produce unitless values. Then
# the rotated values (linear combination
# of the variables) are also unitless. 
# If they are using R they will chose
# scale=TRUE.   
# 
# There is related but somewhat
# different numerical method for
# computing principal components. 
# The scale=TRUE options corresponds
# to using correlation matrix in
# the eigenvector method. 

# With scale=FALSE both
# variables remain expressed as 
# deaths per 100000 people. Rotation will 
# produce new coordinates in the same
# units of measure, deaths per 100000.
# The correspond to using covariance
# matrix in eigenvector numerical method.
   
# It much harder to interpret linear
# combinations of variables when the 
# origin variables have different units
# of measure.    

pc <- prcomp(mat,scale=FALSE)
pcDat <- pc$x # principal components
pcRotate <- pc$rotation  #rotation matrix

# Verify this is a rotation or
# or a rotation with reflections.  
# Reflections are possible
#   We can multiple any pc column by -1
#   and retain the center, variance and
#   orthogonality to other pc's 
   
# The determinant of a rotation matrix is 1. 
# If there are an odd number of reflections
# about the axes, the determinant will be -1. 

det(pcRotate)  # -1 means there is a reflection 

# Rotate the centered data and compare

matRot <- matCen %*% pcRotate
head(pcDat)
head(matRot)
all.equal(pcDat,matRot)

round(pcRotate,2)

# Look at the first column in the
# rotation matrix above. The first principal
# component is approximately 
# -.78 times the centered male vector plus
# -.63 times the centered female vector. 

# A rotation matrix for angle a in 2D
# has form 
#          sin(a)  cos(a)
#         -cos(a)  sin(a)
# The determinant is sin(a)**2 + cos(a)**2 = 1
#
# The reflection here has form
#          -sin(a)  cos(a)
#           cos(a)  sin(a)
# The determinant is -(sin(a)**2) -(cos(a)**2) = -1

a <- -pcRotate[1,1]
asin(a)*180/pi

# In 4. below, think of clockwise rotation
# of 51 degrees about the origin and then
# multiplying the resulting x coordinates
# by -1 to put West Virginia on left    

# 4. Juxtapose the centered data and  
#    principle component scatterplots
#    using the same x and y axis scale
#    limits.
#
#  Find x and y axis limits to accommodate
#  rotating the data.  
#  The first principal component has 
#  large variance but not necessarily
#  the point furthest from the origin

big <- max(abs(pcDat))

dfPC <- data.frame(PC1=pcDat[,1],PC2 = pcDat[,2])
rownames(dfPC) = rownames(dfCen)

# Find some extreme points to label
# with state postal codes

id1 <- which.min(dfPC$PC1)
id2 <- which.max(dfPC$PC1)
id3 <- which.min(dfPC$PC2)
subs <- c(id1,id2,id3)

# Add a column to dfPC and to dfCen
# for postal codes. Fill the columns
# with NAs except for the 3 ids
# we want to show. 
     
dfPC$State <- NA
dfPC$State[subs] <- row.names(dfPC)[subs] 

dfCen$State <- NA
dfCen$State[subs] <- row.names(dfPC)[subs]

# Store the two plots and then juxtapose
#
pCen <- ggplot(dfCen,aes(x=Male,y=Female))+
  geom_hline(yintercept=0,color='blue')+ 
  geom_vline(xintercept=0,color='blue')+
  geom_point(fill="red",shape=21)+
  ylim(-big,big)+
  xlim(-big,big)+
  geom_text(aes(y=Female-5,label=State),size=4,vjust=1)+
  labs(x="Male - Mean(Male)",
    y="Female - mean(Female)",
    title=title)+hwThemeSmallText

pRot <- ggplot(dfPC,aes(x=PC1,y=-PC2))+
  geom_hline(yintercept=0,color='blue')+ 
  geom_vline(xintercept=0,color='blue')+
  geom_point(fill="red",shape=21)+
  ylim(-big,big)+
  xlim(-big,big)+
  geom_text(aes(y=-PC2+5,label=State),size=4,vjust=0)+
  labs(x="PC1 From Male and Female Rates",
    y="-PC2 From Male and Female Rates",
    title=title)+hwThemeSmallText

grid.arrange(pCen,pRot,ncol=2)

# Observe that WV point rotated about 
# 135 degree counter clockwise about the center, 
# (0, 0).  So the UT points and the MS points.

 

# If grid arrange doesn't work
# windows(w=3,h=3)
# pCen
# window(w=3,h=3)
# pRot


# The warning message is not a problem
# The plan was to show just 3 of the
# 51 state codes.

# From the left panel to the right panel there
# is roughly a 135 degree rotation. Look at the
# script above to see that y=-PC2. Actually
# there was also a reflection about the y-axis.
# Think a 45 degree clockwise rotaton and the hte
# x values were mulitple by -1.       

# Multiplying a principal component by -1 does
# not change the two basic properties. 

# By convention
# principal components are listed in decreasing
# variance order.
   
# The principal component vectors are orthogonal. 
# This means the dot products are zero
# Dot product calculation

with(dfPC, sum(PC1 *   PC2 ))
with(dfPC, sum(PC1 * (-PC2)))

# Both are zero for practical purposes.


# 5. Biplot


# Without scaling the data Covariance version

windows(width=6,height=6)
biplot(pc,scale=0,las=1,
  main= "Covariance PC Loadings Top and Right Axes")

# The biplot show two superimposed plot
# that have different scales.
# Such plots run the risk of being
# confusing. 

# The first plot shows the first two principal
# components as points with respect to the
# bottom and left axes. 

head(pc$x)

#  This principal components did not divide the variables
#  by their standard deviations so are based on the covariance
#  matrix of the variables. 

# The second plot in red show values from the
# from the rotation matrix using red arrows
# tips with the arrows starting from the origin.
# The Male arrow tip values can be read
# from left column of the rotation matrix show
# below.  The Female arrow tip values are from
# right column

pc$rot

# The axes for the second plot appear on the top amd right
# of biplot.  Ideally tick mark labels should also
# be red to match the red tick marks, arrows and arrow
# labels. 

# If we read down the rotation matrix PC1 column
# we see linear combination of coefficents that multiply
# the center male and female values for each state to 
# produce PC1.  Roughly speaking the first (PC1) linear combination
# is a negative scale sum of the centered male and female
# values for each states.  Roughly speaking the second linear
# combination is the scaled difference of the center male
# and female value for each state.
# The columns are sometimes called factor loadings.      


# With scaling the Correlation version of PCs

windows(width=6, height=6)
pcCor <- prcomp(mat,scale=TRUE)
biplot(pcCor,scale=1,las=1,
  main="Correlation PC Loadings Top and Right Axes")

# 6. Percent of variability represented

# Guidance suggest looking the 
# standard deviations of the principal
# components

pc$sdev

# The screeplot shows variances
screeplot(pc)

# Another choice is to look 
# cumulative percent of total
# variance 
dfPC$State <- NULL
vec <- diag(var(dfPC))
100*cumsum(vec)/sum(vec)  

# The first principal component accounts for
# 95% of the variability 

# 6. Ordering States by the first principal
     component

ord <- with(dfPC,order(PC1))
round( dfPC[ord,1:2],1)

# The first principal component is often useful
# for sorting cases
