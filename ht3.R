# ht3()
# Requires
# library(ggplot2)
# source('themeHt.R')
# load('vState.RData')

# Arguments:
#
# dat:  a data.frame
#   1st column: state names or state postal codes
#     Rows with other labels such as thenational public
#     are put in a reference data.frame for possible
#     use future use.
#   Three or more additional columns of three level factor
#      Note: The factor levels for the three selected
#          columns are use for:
#          x-axis strip labels
#          y-axis strip labels and
#          legend fill color labels, respectively.
#
# vars: A vector wih names or column numbers
#     specifying three factor variables in dat
#     to use if the default does not suffice.
#     The default vector is 2:4.
#
# varLabs: A vector variable labels to use for the
#    x-axis, y-axis and fill color legend if the
#    default does not suffice.
#    The default uses the chosen variable names
#    with '.' is replaced by ' '.
#
# title:  The plot title that has a default
#   to replace.
#
# fillCol: a vector with three colors used
#    for factor numbers 1,2 and 3 respectively.
#
# postalId: dat column 1 descriptor
#    The default is FALSE
#    FALSE: official US state names
#    TRUE:  official US postal codes
#
#    If additional codes are encountered
#    the cases are currently set aside
#    in separate data frame and ignored
#
# plotId:
#   Default is FALSE
#   TRUE plots postal codes in
#     highlighed state polygons

ht3 <- function(dat,vars=2:4,varLabs=NULL,
    title='Three Factor Hypothesis Test Class Map',
    fillCol=c(rgb(.85,.60,1),gray(.85),rgb(.5,.95,.5)),
    postalId=FALSE, plotId=TRUE){

# 1. Get variable names for variable selection
#    and variable labels for plots.
#    When variable labels are not provided
#    the variable names are used with
#    '.' replace by  ' '.

varNams <- vars
if(is.numeric(vars))varNams <- colnames(dat[,vars])
if(is.null(varLabs)) varLabs <-
   c(gsub('.',' ',varNams[1],fixed=TRUE),
     gsub('.',' ',varNams[2],fixed=TRUE),
     gsub('.',' ',varNams[3],fixed=TRUE))

# 2.variable selection
dat <- cbind(postal=dat[,1],dat[,varNams])

# 3. Separate states from non-states:
#      datG versus datRef
#    State names convert to Postal Ids
#    for internal use

if(!postalId){
  subs <- match(dat[,1],vStateIds$Full)
  datG <- dat[subs,]
  datG[,1] <- vStateIds$Postal[subs]
  if(any(is.na(subs))){
    refVals <- TRUE
    datRef <- dat[is.na(subs),]
  }
} else {
  subs <- match(dat[,1],vStateIds$Postal)
  datG <- dat[subs,]
  if(any(is.na(subs))){
    refVals <- TRUE
    datRef <- dat[is.na(subs),]
  }
}

# 4. Convert background Row and Column numbers
#    to factors using foreground Row and Column
#    factor levels

stBg <- vStateBg3x3
colF <- levels(datG[,2])
rowF <- levels(datG[,3])
stBg$Col <- factor(colF[stBg$colInt],levels=colF)
stBg$Row <- factor(rowF[stBg$rowInt],levels=rowF)

# 5. Build the foreground data.frame for highlighting states
#    This associates the 3 class values for each state
#    with all of its polygon vertices.
#    Two states have more than one polygon.  No problem.

vertexStateNams <- as.character(vStatePolys$st)
subs <- match(vertexStateNams,datG[,1])
fgDat <- data.frame(datG[subs,])
colnames(fgDat) <- c('st','Col','Row','Fill')
fg <- cbind(vStatePolys,fgDat[,-1])

# 6. Build a data.frame to plot postalIds

if(plotId){
  colnames(datG)= c('st','Col','Row','Fill')
  subs <- match(datG[,1],vStateIdLocs$st)
  fgText <- cbind(vStateIdLocs[subs,1:3],datG[,2:3])
}

# 7. Get labels for the fill color legend

names(fillCol) <- levels(datG[,4])

# 8. Produce the plot

if(plotId){
  ggplot(stBg,aes(x=x,y=y))+
  geom_polygon(color=gray(.75),fill='white') +
  geom_polygon(data=fg,aes(x=x,y=y,fill=Fill),color="black")+
  geom_text(data=fgText,aes(x=x,y=y,label=st),
    fontface="bold",size=2.2,vjust=.4)+
  facet_grid(Row~Col,switch="both",as.table=FALSE)+
  scale_fill_manual(values=fillCol)+
  labs(x=varLabs[1], y=varLabs[2],title=title)+
  guides(fill=guide_legend(legend.position="top",
    title.position='top',
    title=varLabs[3],
    title.hjust=.5,title.vjust=.5,
    legend.margin=unit(c(0,0,0,0),"cm"),
    label.position="bottom", label.hjust=.5,
    label.theme=element_text(color='black',angle=0,size=10),
    keywidth=5,keyheight=0.8))+
  htTheme3
} else {
  ggplot(stBg,aes(x=x,y=y))+
  geom_polygon(color=gray(.75),fill='white') +
  geom_polygon(data=fg,aes(x=x,y=y,fill=Fill),color="black")+
  facet_grid(Row~Col,switch="both",as.table=FALSE)+
  scale_fill_manual(values=fillCol)+
  labs(x=varLabs[1], y=varLabs[2],title=title)+
  guides(fill=guide_legend(legend.position="top",
    title.position='top',
    title=varLabs[3],
    title.hjust=.5,title.vjust=.5,
    legend.margin=unit(c(0,0,.3,0),"cm"),
    label.position="bottom", label.hjust=.5,
    label.theme=element_text(color='black',angle=0,size=10),
    keywidth=5,keyheight=0.8))+
  htTheme3
}
}

