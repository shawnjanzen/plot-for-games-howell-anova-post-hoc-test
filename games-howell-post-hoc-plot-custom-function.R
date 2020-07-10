# Author: Shawn Janzen (original code brouwern)
# Date: 10 July 2020

# inspiration for this code came from a comment on the following blog post asking how to plot the Games-Howell post hoc results in R
# https://statisticsbyjim.com/anova/welchs-anova-compared-to-classic-one-way-anova/

# credit for the original version of this code: https://rpubs.com/brouwern/plotTukeyHSD2
# additional reading on the Games-Howell post hoc test: https://rpubs.com/aaronsc32/games-howell-test

# this plot function requires that you have a "posthocTGH" class object which can be created using the posthocTGH function from the userfriendlyscience package


# ****************************

# means = 3 mean values calcualted from raw data
# se = 3 standard errors
# categories = 3 categorical / factors that group the data
# x.axis.label = what should be plotted on the x axis
# y.axis.label = what should be plotted on the y axis


#### The function STARTS here ####
plotGamesHowell <- plotGamesHowell <- function(ght.out,
                           x.axis.label = "Comparison",
                           y.axis.label = "Effect Size",
                       axis.adjust = 0,
                       adjust.x.spacing = 5){
  
  #ght.out <- as.data.frame(ght.out[[1]])
  means <- ght.out$intermediate$dmeans
  categories <- ght.out$intermediate$pairNames
  groups <- length(categories)
  ci.low <- ght.out$intermediate$gh.low
  ci.up  <- ght.out$intermediate$gh.high                         
  
  n.means <- length(means)
   
  #determine where to plot points along x-axis
  x.values <- 1:n.means
  x.values <- x.values/adjust.x.spacing
  
                             
  # calculate values for plotting limits            
  y.max <- max(ci.up) +                    
    max(ci.up)*axis.adjust
  y.min <- min(ci.low) - 
    max(ci.low)*axis.adjust
  
  if(groups == 2){ x.values <- c(0.25, 0.5)}
  if(groups == 3){ x.values <- c(0.25, 0.5,0.75)}
  
  x.axis.min <- min(x.values)-0.05
  x.axis.max <- max(x.values)+0.05
  
  x.limits <- c(x.axis.min,x.axis.max)
  
  #Plot means
  plot(means ~ x.values,
       xlim = x.limits,
       ylim = c(y.min,y.max),
       xaxt = "n",
       xlab = "",
       ylab = "",
       cex = 1.25,
       pch = 16)
  
  axis(side = 1, 
       at = x.values,
       labels = categories,
      )
  
  #Plot upper error bar 
  lwd. <- 2
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.up,
         x1 = x.values,
         length = 0,
         lwd = lwd.)
  
  #Plot lower error bar
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.low,
         x1 = x.values,
         length = 0,
         lwd = lwd.) 
  
  #add reference line at 0
  abline(h = 0, col = 2, lwd = 2, lty =2)
  
  mtext(text = x.axis.label,side = 1,line = 1.75)
  mtext(text = y.axis.label,side = 2,line = 1.95)
  mtext(text = "Error bars = 95% CI",side = 3,line = 0,adj = 0)
  
  
}

#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####