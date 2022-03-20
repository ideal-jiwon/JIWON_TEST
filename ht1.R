# ht1
# Requires
# library(ggplot2)
# source('themeHT.R')
# load('vState.RData')

# Arguments:
#
# dat:  a data.frame that has
#   1) A column of state names or state postal codes
#   2) One more columns indicating state membership
#      in 1 of three classes.  For example there
#   can be columns for 4th and 8th grade Math and Reading
#   Currently classes the result of
#   hypothesis tests comparing state estimates to the
#   corresponding national estimate or state trends
#   to zero.
#
#   Example class labels for state are "Below", "Similar",
#   and "Above."   Example class labels for trends are
#   falling similar and rising.
#
# var: A name or column number
#     factor variables in dat
#     to use if the default does not suffice.
#     The default value is 2.
#
# idVar: A name or column number
#        of state names or postal ids.
#        The default value is 1.
#
# idType: "State"  or "Postal"
#   The default, "State"
#   means the full state name.
#
# title:  The plot title has a default
#   that should be replaced.
#
# layout: "Col","Row", "Fill"
#   The default is "Fill"
#
#   Currently
#     "Col" means three columns of maps in a row.
#     "Row" means three rows of maps in column.
#     "Fill" means a single map with three fill colors.
#
#  fillColors:
#    Default:  c(rgb(.9,.45,1), gray(.9), rgb(.3,.9,.1))
#    The use of three fill colors
#    to reinforces the Col and Row
#    highlighting of states in different panels has
#    not yet been implemented
#
# oneFill:
#   Default: rgb(.9,.45,1)
#
# Not yet used::  useOneFill:
#      Default: FALSE
#      If TRUE the Row and Col layouts
#      use the just the oneFill color.
#
# plotId
#   Default is TRUE
#   This plots postal codes in
#     highlighted state polygons

# Future considerations
#
# A variable labels option could be added
# to avoid renaming variables externally.
#
# While label length is a consideration, more precise
# statistical labels provide options.  The label "Similar"
# may not accurately communicate the failure to reject the
# null hypotheses when the comparing
# state values to the national value.
# Some longer candidate labels are
# "Not Significantly Different", "Plausibly Similar" or
# "Equality Not Rejected".  More information could be
# included in a caption line or possible in a subtitle.
#
# Additional labeling might document the use of multiple
# comparison procedures to control the error rates.
#
# Suppressed or missing results.  This can be addressed.
# Currently all states are plot with a background fill color
# and the overplotted with foreground fill color.
# If state's class membership is missing for
# a class variable used in the plot Column, Row, Fill
# is not known casewise deletion is reasonable when
# overplotting states in foreground plot.
# The appearance of background fill color for a state
# could indicated one or more suppress state values.
#
# There is already an alternative polygon data structure
# that include square for DoDEA and apolygon could be
# added for for Puerto Rico. If this data structure
# were used with the current function I think the
# this would simple omit polygons when is no data.
# This is likely okay when the states do not have
# missing data.

ht1 <- function(dat, var = 2, idVar = 1,
    idType=c('State','Postal')[1],
    title = 'Hypothesis Test Class Map',
    layout = c('Col','Row','Fill')[3],
    fillColors = c(rgb(.9,.6,1), gray(.9), rgb(.5,.9,.5)),
    oneFill = rgb(.9,.45,1),
#    useOneFill = FALSE is not working
    plotId = TRUE){

# 0. Fill color control
    if(layout != "Fill") fillColors <- rep(oneFill,3)

# 1. Select the variable and state id columns
#   by position or name.

  cnames <- colnames(dat)
  if (is.numeric(var)) varNam <- cnames[var] else
    varNam = var
  if (is.numeric(idVar)) stateNam <- cnames[idVar] else
    stateNam = idVar
  dat <- dat[,c(stateNam,varNam)]

# 2. Remove non-state rows
#    Convert full state sames to
#    to postal codes for matching
#    polygon boundary ids1

if (idType == "State") {
  good <- !is.na(match(dat[, 1], vStateIds$Full))
  datG <- dat[good, ]
  subs <- match(datG[,1],vStateIds$Full)
  datG[, 1] <- vStateIds$Postal[subs]
} else {
  good <- !is.na(match(dat[, 1], vStateIds$Postal))
  datG <- dat[good, ]
}
colnames(datG)[1] <- "Postal"

# 3. Convert polygon Row and/or Column numbers
#    to factors using foreground Row and/or Column
#    factor levels

switch(layout,
  Fill = {
  stBg <- vStatePolys
  fillF <- levels(datG[, 2])
  vertexStateNams <- as.character(vStatePolys$st)
  subs <- match(vertexStateNams, datG[, 1])
  stBg$Fill <- factor(as.character(datG[,2])[subs],
    levels = fillF)
  },
  Col = {
    stBg <- vStateBg3
    colF <- levels(datG[, 2])
    stBg$Col <- factor(colF[stBg$colInt], levels = colF)
  },
  Row =  {
    stBg <- vStateBg3
    rowF <- levels(datG[, 2])
    stBg$Row <- factor(rowF[stBg$colInt], levels = rowF)
  }
)

# 4. Build the foreground data.frame for highlighting states
#    This associates the class values of each state
#    with all of its polygon vertices.
#    Two states have more than one polygon.  No problem.

# The repeats code above in the Fill section
# Consider simplification

fg <- vStatePolys
vertexStateNams <- as.character(vStatePolys$st)
subs <- match(vertexStateNams, datG[, 1])
fg$Fill <- factor(as.character(datG[,2])[subs],
levels(datG[,2]))
colnames(fg)[4] <- layout

# 5. Build a data.frame to plot postalIds

if (plotId) {
  subs <- match(datG[,1],vStateIdLocs$st)
  fgText <- cbind(vStateIdLocs[subs,1:3],datG[,2])
  colnames(fgText)[4] <- layout
}

# 6. Get labels for the fill color legend

names(fillColors) <- levels(datG[,2])

# 7. Produce the plot

switch( layout,
  Fill = {
    if( plotId ) {
      ggplot(stBg, aes(x = x, y = y)) +
      geom_polygon(color = gray(.75), fill = 'white') +
      geom_polygon(data = fg,
        aes(x = x, y = y,fill = Fill),
        color = 'black') +
      labs(x = NULL, y = NULL, title = title) +
      guides(fill = guide_legend(
          legend.position = "top",
          title.position = 'top',
          title = varNam,
          title.hjust = .5, title.vjust = .5,
          legend.margin = unit(c(0, 0, 0, 0), "cm"),
          label.position = "bottom",
          label.hjust = .5,
          label.theme = element_text(
            color = 'black',angle = 0, size = 10),
          keywidth = 5, keyheight = 0.8)
      ) +
      geom_text(
        data = fgText,
        aes(x = x, y = y, label = st),
        fontface = "bold", size = 2.5,
        color = "black") +
      scale_fill_manual(values = fillColors) +
      htTheme3
    } else {
      ggplot(stBg, aes(x = x, y = y)) +
      geom_polygon(color = gray(.75), fill = 'white') +
      geom_polygon(fg,
        aes(x = x, y = y,fill = Fill),
        color = 'black') +
      labs(x = NULL, y = NULL, title = title) +
      guides(fill = guide_legend(
        legend.position = "top",
        title.position = 'top',
        title = varNam[2],
        title.hjust = .5,vtitle.vjust = .5,
        legend.margin = unit(c(0, 0, 0, 0), "cm"),
        label.position = "bottom",
        label.hjust = .5,
        label.theme = element_text(
            color = 'black', angle = 0, size = 10),
          keywidth = 5, keyheight = 0.8)
       ) +
       scale_fill_manual(values = fillColors) +
       htTheme3
      }
  },
  Col = {
    if (plotId) {
      ggplot(stBg, aes(x = x, y = y)) +
      geom_polygon(color = gray(.75), fill = 'white') +
      geom_polygon(data = fg,
        aes(x = x, y = y),
        fill = oneFill, color = 'black') +
      labs(x = varNam, y = NULL, title = title) +
      geom_text(data = fgText,
        aes(x = x, y = y, label = st),
        fontface = "bold", size = 2.2) +
      facet_grid(.~Col, switch = "x") +
      htTheme1Col
    } else {
      ggplot(stBg, aes(x = x, y = y)) +
      geom_polygon(color = gray(.75), fill = 'white') +
      geom_polygon(data = fg,
         aes(x = x, y = y),
         fIll = oneFill, color = 'black') +
      labs(x = varNam, y = NULL, title = title) +
        facet_grid(. ~ Fill,switch = 'x') +
      scale_fill_manual(values = fillColors) +
      htTheme1Col
    }
  },
  Row = {
    if (plotId) {
      ggplot(stBg, aes(x = x, y = y)) +
      geom_polygon(color = gray(.75), fill = 'white') +
      geom_polygon(data=fg,
        aes(x = x, y = y),
        fill = oneFill, color = "black") +
      labs(y = varNam, x = NULL, title = title) +
      geom_text(data = fgText,
        aes(x = x, y = y, label = st),
        fontface = "bold", size = 2.2) +
      facet_grid(Row ~ ., switch = "y", as.table = FALSE) +
      scale_fill_manual(values = fillColors) +
      htTheme1Row
    } else {
      ggplot(stBg, aes(x = x, y = y)) +
      geom_polygon(color = gray(.75), fill = 'white') +
      geom_polygon(data=fg,
        aes(x = x, y = y),
        fill = oneFill, color = "black") +
      labs(y = varNam, x = NULL, title = title)
      facet_grid(Row ~ ., switch = "y", as.table = FALSE) +
      scale_fill_manual(values = fillColors) +
      htTheme1Row
    }
  }
  )
}
