              The first Principal Component of Heptathlon scores
Thanks to Dr. Dan Carr fro providing resources fro this code

A heptathlon example is adapted from  
"A Handbook of Statistical Analysis Using R" 
by Everitt and Hothorn.

The first principal component from heptathlon
scores matches the judges score amazingly well.   

Sections
1. Read the data and make a scatterplot matrix
2. Use identify() to name an outlier 
3. Transforming data before input to principal components 
4. Obtaining principal components
5. Assess the variation in the principal components
6. Relate the first principal component to the judges score

Due: Plot from 2 with the name of the outlier and
     the plot from 6. 

#_______________________________________________________

1. Read the data and make a scatterplot matrix

## Run

heptathlon = read.csv(file="heptathlon.csv",row.names=1)
heptathlon

# remove the official score
hepDat = heptathlon[,-ncol(heptathlon)]

## End 
  
The values for hurdles, run200m and run800m are in seconds
The values are highjump, shot, long jump and javelin are meters

We can look the data with a scatterplot matrix 

## Run 
windows(width=7,height=7)      
nam <- c("Hurdles\nSeconds","Highjump\nMeters","Shotput\nMeters",
  "Run200m\nSeconds","Longjump\nMeters","Javelin\nMeters",
  "Run800m\nSeconds")
myPanelSmooth = function(x,y,...)panel.smooth(x,y,lwd=3,col.smooth='red',...)
pairs(hepDat,lab=nam, panel=myPanelSmooth,pch=21,cex=1.5,
     col=rgb(0,.6,1),bg=rgb(0,.6,1),gap=0,las=1)

## End

Note the outlier along the top of both the top and bottom row.  
One athlete has a long hurdle time, a low high jump, a short long jump,
a pretty long 200 meter run time and long time for the 800 meter run
as compared to the other Olympic athletes.   
 
2. Use identify() to name an outlier
 
The script below used the identify() function. After running this
left click in the window a little to the left of the upper right
point to find out name associated with the outlier. 
You can identify a few more points for  fun.  Then right click
in the plot and select stop to proced.   

## Run
plot(hepDat[c(1,7)],pch=16,col='blue',cex=1.2,las=1)
identify(heptathlon[,1],heptathlon[,7],lab=row.names(heptathlon))
# End

3. Transforming data before input to principal components 

A good way to simplify intepretation is make all the
positive outcomes have larger values.  Hence the following
script transforms the timed outcomes so the fastest times
(currently smallest values) become largest values.  The 
transformation below subtracts the person's time from the
longest time.  The person with the shorted time will have the
largest value.  In words the value is the seconds ahead of the
last person.  
     
## Run

hepDat$hurdles=max(hepDat$hurdles)-hepDat$hurdles
hepDat$run200m=max(hepDat$run200m)-hepDat$run200m
hepDat$run800m=max(hepDat$run800m)-hepDat$run800m

##End

4. Obtaining principal components

When the variables are assessed in different
units of measure standard practice is to 
use the correlation matrix in obtaining
principle components.  Below this means
set the prcomp() scale argument to TRUE
which after subtracting the means
divides the variables by their standard
deviations.

## Run   

#
heptPca = prcomp(hepDat,scale=TRUE) 

heptPca$center # means of the variables 
heptPca$scale  # standard deviation of the variables
heptPca$x      # principal components
heptPca$sdev   # standand deviations of the principal components
round(heptPca$rotation,2)

## End

Look at the rotation columns. A graphic represention
could help show the patterns. In the first column
the magnitude are roughly the same except the small
value for javelin.  In the second column the javelin
value, -.84, sticks out as having a largest magnitude.  
Throwing the javelin has physical requirements that are less
compatible with the training for other events.   

5. Assess the variation in the principal components

## Run 
names(heptPca$sdev) = paste('Comp',1:length(heptPca$sdev),sep='')
plot(heptPca)

heptVar = heptPca$sdev**2
100*cumsum(heptVar)/sum(heptVar)
## End

The first principal component accounts for about 64% of the variation.

6. Relate the first principal component to the judges score

## Run

plot(heptathlon$score,-heptPca$x[,1],las=1,pch=21,bg="red",
     xlab="Official Score",ylab="First Principal Component",
     main="Comparing Scores and First Principal Component")
correl = cor(-heptPca$x[,1],heptathlon$score)
xloc = mean(par()$usr[1:2])
text(xloc,4,paste("Correlation =",round(correl,2)),adj=.5,cex=1.1)

## End

The script above multiplied the first principal components by -1
to make the correlation positive.  Multiplying by -1 does
not change the orthogonality of the principal components. 

Note the high agreement of the first principal component
as a score and the official score.  This is interesting. 
Of course omitting one person and the choice of variable
transformations may have had something to do with the
close match. Still it makes one wonder if the official
scoring system emerged from the application of principal
components to old data sets. Much effort goes into most scoring
in the effort to make the scoring fair for individual events and I
suppose the weights for combining event where established long ago.
Information is avaiable via Google.
.



