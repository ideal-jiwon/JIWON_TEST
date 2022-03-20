File        Principal Components 3D Centering and Rotation  
Original code was created by   Daniel B. Carr

Emphasis    Centering, Rotation, and may be reflection

Sections:   

1. Generating trivariate normal samples
2. A 3D data scatterplot with translucent
     ellipsoid as a density reference
3. Producing principal components  
4. A 3-D principal components plot
     with a reference ellipsoid

5. Results returned by prcomp()
6. A computational check using
     matrix multiplication 
7. Principal component interpretation
     in terms of rotation coefficients 
8. Picking the number principal components to use
9. 2-D scatterplots 

# =========================================

Due       Plots from 2, 4, 8, 9-first plot

#==========================================

0. Setup

## Run
library(MASS)
library(rgl)

## End

1. Generating random sample from a trivariate
   normal distribution

First we specify the mean vector and 
covariance matrix for three variables and
label them Mean and Cov.  

Then we use the mvrnorm() function from
the MASS library to generate 
1000 random samples from the 
trivariate normal distribution.

## Run_______________

Mean <- c(4,-2, 5)
Mean

Cov <- matrix(c(3,3.5,0,3.5,10,0,0,0,1), 3,3)
Cov

set.seed(37)
xyz <- mvrnorm(1000, Mean, Cov)

# End

# Below note that the sample mean vector
# and covariance matrix do not
# exactly match Mean and Cov
# theoretical distribution parameters.
#
# The Central Limit theorem guarantees
# that the sample mean vector converges
# to the parameter mean vector
# as the sample size gets larger.
#
# This applies to simulated samples
# unless there is problem with the 
# simulation method. The basic 
# distribution simulation methods
# have passed numerous tests.  
    
sampMean <- apply(xyz,2,mean) 
sampMean
Mean
sampMean - Mean

sampCov <- var(xyz)
round(sampCov,2)
Cov
round(sampCov-Cov,2)

## End_________________________

2. A 3-D data scatterplot with a translucent
   ellipoid as a density reference.   

Next we look a 3D scatterplot. To help show
the orientation of the cloud of points we will
add a translucent green ellipsoid that contains
roughly 90 percent of the observations. This is
reasonable because the sample is from a
normal distribution. 

Otherwise we could estimate the 3D point density
densities on 3-D grid provide and produce
'alpha' contours using thecontour3D function
in the misc3D package. We might choose an
'alpha" contour level that is .8 times the
maximum density. Contours are specified based
on fraction times the maximum density
are called alpha contours. These will only 
be roughly ellipsoidal.   

David Scott show multiple 3D contours in his
book Multivariate Density Estimation: Theory,
Practice, and Visualization (Aug 1992). 
One of his techniques removes sections between
parallel cutting plans to help us see part the
higher density contours hidden by lower
density contours.
 
Hies average shifted histogram approach readily
produces density estimates on a 3-D grid. 
Visualization of 3D contours is pretty old in
the computer graphics world.           
 
Note that a constant density contours may
consist of more that one disjoint shells.

When the RGL Device appears  
Left click on a corner and drag to enlarge the plot   
Left click in the plot and drag to rotate the contents
Right click in the plot and drag up or down
to zoom out or in

## Run______________________

open3d(FOV=0)
plot3d(xyz, box=TRUE,
 xlab="x", ylab="y", zlab="z")
aspect3d("iso")
xzyMean <- colMeans(xyz)
xCov <- var(xyz)
plot3d( ellipse3d(Cov,centre=Mean, level=.9),
  col="green", alpha=0.5, add = TRUE)

## End_____________________

After modifying the RGL view make a .png snapshot.

## Run

snapshot3d("Data and 90% Ellipsoid.png" )

## End_____________________

Observe that: 

1) The data is NOT centered at the origin.
2) The y-axis has the largest range, NOT the x axis.
3) Since the covariance matrix is not a diagonal
   matrix, the ellipsoid axis are not aligned the 
   the coordinate axes.

Again the one line principal component function
prcomp() will generate new variables called
principal components. These new variables are
1) Centered at the origin
2) Rotated so the covariance matrix is diagonal
3) Rotated so 
   The first variable has the largest variance
   The second variable has the next largest variance
   as so on.  
4) There may be reflections

Again prcomp() passes the data to scale() function to
center the data.  If we set the scale argument
in prcomp() to TRUE this is passed to the scale argument
in the scale() function. Then, after centering the variables,
they will be divided by their standard deviations. The
the rotation yields principal components. 

Applying the var() funtion to variables will produce
diagonal correlation matrix if scale was set to TRUE
or else a diagonal covariance matrix.

3. Producing principal components 

We assume our random number variables are in the
same units so choose not to scale the variables.
Then the prcomp rotate is based on the covariance
matrix.
 
## Run_____________

pcList <- prcomp(xyz)

## End_____________

4. A 3-D principal components plot
   with a reference ellipsoid.

The object returned by prcomp() is a list.

The principal components are in the 
$x component of the pcList above.

## Run_____________________

pc <- pcList$x
pcMeans <- colMeans(pc)
round(pcMeans,2)

pcCov <- var(pc)
round(pcCov,2)

open3d(FOV=0)
plot3d(pc, box=TRUE,
xlab="PC1",ylab="PC2",zlab="PC3")
aspect3d("iso")
plot3d(ellipse3d(pcCov, centre=pcMeans, level=.9),
  col="cyan", alpha=0.5, add = TRUE)

## End_____________________

After changing the view make a snapshot,  

## Run

snapshot3d("Principle components and 90% Ellipsoid.png" )

## End_____________________

5. Results returned by prcomp()

pcList$center 
  Contains the means of the input variables
  that were subtracted to center the data

pcList$rotation
  Is the rotation used to rotate the
  case points about the origin.

pcList$sdev has standard deviations of the
  principal components.  

pcList$scale is FALSE in this case 
  since the default uses the 
  covariance matrix. 

pcList$x contains the principal
  components as indicated above. 

6. A computation check using
   matrix multiplication 

The computation uses the singular value
decomposition function, svd(), to produce
the rotation matrix. Since we did not
scale the data, svd is based on the
data covariance matrix.  
The covariance for the n cases by 3 variables
data set is a 3 x 3 matrix.  
The rotation matrix is a 3 x 3 matrix.  

We can post multiply the n x 3 centered variable
data matrix by the 3 x 3 rotation matrix to obtain the 
n x 3 principal components matrix and compare this
against the results of prcomp().   

## Run____________________

rot <- pcList$rot
rot
det(rot)

xyzCentered <- scale(xyz,center=T,scale=FALSE)
head(xyzCentered)

pcCheck<- xyzCentered %*% rot

all.equal(pc, pcCheck)

## End______________________

7. Principal component interpretation
   in terms of rotation coefficients 

The scaling and the linear combination
of variables that make up principal components
provide the basis for interpreting 
principal components.   

When the variables are scaled, the coefficients
of each linear combination are comparable as 
standard deviation units.  We describe them
without reference to units of measure such
as degrees centigrade.

We did not scale our generated variables. 
They don't have units of measure.  We can
think of the coefficients as weights for 
variables that have difference variances.
    
The rotation matrix for our example is:

# round(rot,2)

       PC1   PC2   PC3
[1,] -0.36  0.93 -0.05
[2,] -0.93 -0.36  0.00
[3,] -0.01  0.04  1.00

PC1 <- -.36*Xcen - .93*Ycen-.01*Zcen

# round(var(xyz),1)

     [,1] [,2] [,3]
[1,]  3.0  3.5  0.1
[2,]  3.5 10.5  0.1
[3,]  0.1  0.1  1.0
> 

In the PC1 line about the -.36 weights
the Xcen variable moderately since the Xcen
variance of 3.0 in the table above is moderate in size.
 
The -.93 weights the Ycen variable most 
since YCen has the largest variance of 10.5 as
shown above.
  
The  -0.01 weights the Zcen variable only a little since
is variance is only 1. 

As a reminder on the computation of a
variance for a linear combination using
just the Xcen and Ycen part of the
first principal component   

var( a*Xcen+ b*Ycen ) = a^2*Var(Xcen) + 
              2*a*b*cov(Xcen,Ycen) +
                b^2*var(Ycen)

The first principal component has
the largest variance of the principal
components.  Plugging in the numbers
below we can see that even the
covariance term contributes to making
the variance large. 

Note that we could change
the sign on all the coefficients and
get the same variance.   

var( -.36Xcen -.93Ycen ) = (-.37)^2*3.0 + 
              2*(-.37)*(-.93)*3.5 +
                (-.93)^2*10.5

When a nice pattern appears in the coefficients
they can used to provide a rough interpretation
of patterns that appear in principal component
plots.

For example in a 5D case a principle
component might have weights close to

(small,  .82, small, -.41, -.41)

This could be interpreted as contrast between the
2nd variable and the average of the 
4th and 5th variables.  (A contrast is a 
set of weights that sum to 0.)  

Of course interpretion depends on 
understanding the variables and the
scientific context.

8. Picking the number principal components to use

The number of principal components produced
is limited to the number of linearly independent
variables in the data set.  This is typically
the same as the rank of the data's covariance or
correlation  matrix. If there are fewer cases
than variables, the number of cases is an upper
bound the rank.     

We typically show the largest principal components.
The hope is that the first few principal components
mostly represent the true structure and that
the last principal components mostly represent noise
or negligible structure.   

The more principal components we represent the higher
the faction of data variabilty that we show. Due to
principal component selection order the amount of
additional variability  represented decreases with
the addition of the next principal component.  One
stopping heuristic is to stop when incremental increase
in variance are gets small.

## Run_______________

screeplot(pcList, las=1)

## End________________

While we may select many principal component for 
subsequent modeling, for example using linear regression.
The number variables chosen to shown using
graphics is often very small.  Often we see a scatterplot
with just two principal components.  

With more principal components we can produce
scatterplot matrices or parallel coordinate plots.  
Scatterplot matrices for three principal components
are not that uncommon.  Of course rotation and
stereo graphics can show 3D points as can rgl.  
However, flatland style remains is popular.     

For maps regions, such as earth grid cells, a principal
component can determine color.  I remember seeing as
many as six univariate maps each representing a
different principal component.  These were primarly
interpreted as contrasts of the original variables as based
on high magnitude rotation coefficients.    

I have also seen three principal components used to
control the red, green and blue intensities for grid cells. 
Such maps show a lot of color variation. However color is an
integral encoding so we can't rip apart a color
into the values for three principle components
and interpret this in terms of the orginal variables. 

9.  2-D scatterplots 

When we make 2-D scatterplots with pairs
of principal components we might want to 
make the units per inch the same for the axes.  

Otherwise we have to read the scale and imagine
the different variances. 
   
## Run:  Quick and dirty window scaling

tmp <- apply(pc,2,range)
tmp

round(diff(tmp),1)
## 22.6 8.1 6.3

## Quick and dirty window scaling
#  to convey the ranges of the
#  first two principal components.  

winX <- 7.5
winY <- winX*8.1/22.6 
windows(w=winX,h=winY)
plot(pc[,1],pc[,2],las=T,
main="Maybe Interesting")

windows()
plot(pc[,1],pc[,2],las=T,
main="Not Very Interesting")

## End

