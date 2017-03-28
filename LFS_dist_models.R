
#LONGFIN SMELT DISTRIBUTION MODELS#
#### FRONT MATTER ####
# AUTHOR: Vanessa Tobias
# AGENCY: CDFW

#### Load Libraries ####
library(mgcv)
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(berryFunctions) #for classify()

# Load data from LFS_data_explore.R ####
# This .R file loads the data, cleans up variable names, and adds necessary variables for the model.
source(file = "LFS_data_steps.R")  
# You might need to change the file path or your working directory.
# Check source file for packages that you might need to download.


#### FIT THE MODELS ####
# Notes ####
# Binomial (presence/absence) models #

# formula notes:
#  s() = smooth terms for single variables
#  ti() = smooth terms for interacting variables when you include a separate s() term for the main effects (te() is for not including separate main effects)
#  pa_ = presence/absence of the specified age class
#  "by = bay" makes separate smooths for each bay (similar to categorical variable interaction terms in a GLM)
#  sb = "cc" makes cyclical functions for the seasonal component (months).  This tells the model that months 1 & 12 are related to each other.
#  k values: The choice of k for each component isn't arbitrary, but it hasn't strictly been tested, either. I'm happy with these ks for a first attempt. Use gam.check to determine whether you want to adjust k.

# data notes:
#  series 1 & 2 (1 = the longest time series stations + 2 = additional stations for more coverage of shoals)
#  Series 2 starts in 1987 so we're only using Series 1 data from 1987 and later.

# other notes:
# list(nthreads = 6) speeds up the model fitting by letting the computer use more than one core of the processor.  
#     It's using 6 cores in this case. This does not affect the model output, just the time you have to wait to see it.
#     If you're not using a CDFW Synthesis Team computer, you need to check that you have 6 cores available (not threads).
#     The maximum number of cores you should use is the number of physical cores minus one. Use fewer cores if you want to use your computer for other things while the models run.
# method = "REML" This is the preferred method for selecting the smoothness coefficient.  
#     I haven't found a good explanation for why recently and I don't remember why right now. I'll look into this some more.

#---- Midwater Trawl Models ####
mwt0 <- gam(pa0 ~ s(month, by = bay, bs = "cc", k = 10)
            + s(year, by = bay, k = 10)
            + ti(month, year, by = bay, k = 3),  #ti is for when you want to include main effects in the model separately.
            data = mwt[which(mwt$series %in% c(1, 2) & mwt$year > 1986),], #using only series 1 & 2 #series 2 starts in 1987
            family = binomial, 
            link = logit,
            control=list(nthreads = 6),
            method = "REML")

mwt1 <- gam(pa1 ~ s(month, by = bay, bs = "cc", k = 10)
            + s(year, by = bay, k = 10)
            + ti(month, year, by = bay, k = 3),  #ti is for when you want to include main effects in the model separately.
            data = mwt[which(mwt$series %in% c(1, 2) & mwt$year > 1986),],
            family = binomial, 
            link = logit,
            control=list(nthreads = 6),
            method = "REML")

mwt2 <- gam(pa2 ~ s(month, by = bay, bs = "cc", k = 10)
            + s(year, by = bay, k = 10)
            + ti(month, year, by = bay, k = 3),  #ti is for when you want to include main effects in the model separately.
            data = mwt[which(mwt$series %in% c(1, 2) & mwt$year > 1986),],
            family = binomial, 
            link = logit,
            control=list(nthreads = 6),
            method = "REML")

par(mfrow = c(2, 2))
summary(mwt0)
gam.check(mwt0)
summary(mwt1)
gam.check(mwt1)
summary(mwt2)
gam.check(mwt2)

par(mfrow = c(1, 1))

#---- Otter Trawl Models ####
ot0 <- gam(pa0 ~ s(month, by = bay, bs = "cc", k = 10)
           + s(year, by = bay, k = 10)
           + ti(month, year, by = bay, k = 3),  #ti is for when you want to include main effects in the model separately.
            data = ot[which(ot$series %in% c(1, 2) & ot$year > 1986),],
            family = binomial, 
            link = logit,
            control=list(nthreads = 6),
            method = "REML")

ot1 <- gam(pa1 ~ s(month, by = bay, bs = "cc", k = 10)
           + s(year, by = bay, k = 10)
           + ti(month, year, by = bay, k = 3),  #ti is for when you want to include main effects in the model separately.
            data = ot[which(ot$series %in% c(1, 2) & ot$year > 1986),],
            family = binomial, 
            link = logit,
            control=list(nthreads = 6),
            method = "REML")

ot2 <- gam(pa2 ~ s(month, by = bay, bs = "cc", k = 10)
           + s(year, by = bay, k = 10)
           + ti(month, year, by = bay, k = 3),  #ti is for when you want to include main effects in the model separately.
            data = ot[which(ot$series %in% c(1, 2) & ot$year > 1986),],
            family = binomial, 
            link = logit,
            control=list(nthreads = 6),
            method = "REML")

summary(ot0)
summary(ot1)
summary(ot2)

gam.check(ot0)
gam.check(ot1)
gam.check(ot2)


#### MAKE PREDICTIONS ####

# Make a data frame to use in making predictions ####
#include all bay, year, and month combinations so that we can use the same dataset once for each model
# bay = c(6, 7) isn't in series = c(1,2) so bay only goes from 1:5
newDF <-data.frame(year = rep(1987:2015, times = 12*5), 
                   month = rep(1:12, each = length(1987:2015), times = 5), 
                   bay = rep(1:5, each = length(1987:2015)*12))   #with(DF, data.frame(YEAR = unique(YEAR)))

# This section could be cleaner.  Lots of copy and paste.  Sorry about that. 
#  If you change the breaks for one model, make sure you change them for all six models.
# Midwater Trawl Age-0 ####
mwt0.pred <- predict(mwt0, newdata = newDF, type = "response", se.fit=TRUE)
mwt0.pred <- cbind(newDF, mwt0.pred)  #package the data with the predictions
mwt0.link <- predict(mwt0, newdata = newDF, type = "link", se.fit = TRUE)
mwt0.pred$ucl <- with(mwt0.link, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
mwt0.pred$lcl <- with(mwt0.link, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))
rm(mwt0.link) #clean up
mwt0.pred$pclass <- classify(mwt0.pred$fit, method = "usergiven", 
                             breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))$index  #need '$index' because classify() returns a bunch of info about the breakpoints, etc. index is the results vector

# Midwater Trawl Age-1 ####
mwt1.pred <- predict(mwt1, newdata = newDF, type = "response", se.fit=TRUE)
mwt1.pred <- cbind(newDF, mwt1.pred)  #package the data with the predictions
mwt1.link <- predict(mwt1, newdata = newDF, type = "link", se.fit = TRUE)
mwt1.pred$ucl <- with(mwt1.link, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
mwt1.pred$lcl <- with(mwt1.link, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))
rm(mwt1.link) #clean up
mwt1.pred$pclass <- classify(mwt1.pred$fit, method = "usergiven", 
                             breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))$index  #need '$index' because classify() returns a bunch of info about the breakpoints, etc. index is the results vector

# Midwater Trawl Age-2+ ####
mwt2.pred <- predict(mwt2, newdata = newDF, type = "response", se.fit=TRUE)
mwt2.pred <- cbind(newDF, mwt2.pred)  #package the data with the predictions
mwt2.link <- predict(mwt2, newdata = newDF, type = "link", se.fit = TRUE)
mwt2.pred$ucl <- with(mwt2.link, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
mwt2.pred$lcl <- with(mwt2.link, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))
rm(mwt2.link) #clean up
mwt2.pred$pclass <- classify(mwt2.pred$fit, method = "usergiven", 
                             breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))$index  #need '$index' because classify() returns a bunch of info about the breakpoints, etc. index is the results vector


# Otter Trawl Age-0 ####
ot0.pred <- predict(ot0, newdata = newDF, type = "response", se.fit=TRUE)
ot0.pred <- cbind(newDF, ot0.pred)  #package the data with the predictions
ot0.link <- predict(ot0, newdata = newDF, type = "link", se.fit = TRUE)
ot0.pred$ucl <- with(ot0.link, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
ot0.pred$lcl <- with(ot0.link, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))
rm(ot0.link) #clean up
ot0.pred$pclass <- classify(ot0.pred$fit, method = "usergiven", 
                             breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))$index  #need '$index' because classify() returns a bunch of info about the breakpoints, etc. index is the results vector


# Otter Trawl Age-1 ####
ot1.pred <- predict(ot1, newdata = newDF, type = "response", se.fit=TRUE)
ot1.pred <- cbind(newDF, ot1.pred)  #package the data with the predictions
ot1.link <- predict(ot1, newdata = newDF, type = "link", se.fit = TRUE)
ot1.pred$ucl <- with(ot1.link, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
ot1.pred$lcl <- with(ot1.link, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))
rm(ot1.link) #clean up
ot1.pred$pclass <- classify(ot1.pred$fit, method = "usergiven", 
                            breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))$index  #need '$index' because classify() returns a bunch of info about the breakpoints, etc. index is the results vector

# Otter Trawl Age-2+ ####
ot2.pred <- predict(ot2, newdata = newDF, type = "response", se.fit=TRUE)
ot2.pred <- cbind(newDF, ot2.pred)  #package the data with the predictions
ot2.link <- predict(ot2, newdata = newDF, type = "link", se.fit = TRUE)
ot2.pred$ucl <- with(ot2.link, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
ot2.pred$lcl <- with(ot2.link, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))
rm(ot2.link) #clean up
ot2.pred$pclass <- classify(ot2.pred$fit, method = "usergiven", 
                            breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))$index  #need '$index' because classify() returns a bunch of info about the breakpoints, etc. index is the results vector

# Explore distribution of predicted probabilities for cutoff values
#par(mfrow=c(1,1), mar = c(2, 2, 2, 2))
#hist(mwt1.pred$fit)

#### MAKE PREDICTION GRAPHS ####

# Notes ####
#  The following code makes a LOT of graphs, but they're contained in PDFs.
#  Apologies for the sea of for loops.  There are probably better/faster/prettier ways, but this works so I'm going with it until I learn a better way.
#    The loops run through the combinations of month, year, and age class to produce all possible figures.
#    If you just want a specific graph, you can set the loop counters to a specific value and run the pieces of the loops in single chunks.
#  The PDFs will write to your working directory unless you change the file path.
#    File names make sense to me, but you're welcome to change them if they don't make sense to you.

# Make a couple of data frames ####
#  to hold information to make the graphs easier to code 
seasons <- data.frame(season = c("Winter", "Spring", "Summer", "Fall"),
                      num = 1:4)
bays <- data.frame(num = 1:7,
                   name = c("South SF Bay", "Central SF Bay", "San Pablo Bay", "Suisun Bay", 
                     "West Delta (confluence)", "Sacramento River", "San Joaquin River"))
models <- data.frame(name = as.character(c("mwt0.pred", "mwt1.pred", "ot0.pred", "ot1.pred", "mwt2.pred", "ot2.pred")),
                     age = c(0, 1, 0, 1, 2, 2),
                     gear = c ("Midwater Trawl", "Midwater Trawl", "Otter Trawl", "Otter Trawl", "Midwater Trawl", "Otter Trawl"))
models$name <- as.character(models$name)

# Seasonal Timeline graphs without CIs ####
pdf("season_timeline_NoCIs.pdf", width = 11, height = 8.5) #create a pdf file to contain the plots
for(m in 1:length(models$name))
{
  for(b in bays$num[1:5])
  {
    par(mfrow = c(2, 2),     #4 plots per page, in a grid
        xpd = TRUE,          #allows printing in margins to stop the titles from getting cut off
        oma = c(2, 2, 2, 2)) #puts some space around the grid of plots
    for(i in seasons$num)
    {
      plot(0, 0, type="n",
           xlim = c(1987, 2015),
           ylim = c(0, 1),
           main = seasons$season[i],
           bty = "l",
           ylab = "Probability of Presence",
           xlab = "")
      #shaded polygons for CIs:
      # polygon(x = c(1987:2015, 2015:1987),
      #        y = c(eval(parse(text = models$name[m]))$ucl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 1+3*(i-1))],
      #              rev(eval(parse(text = models$name[m]))$lcl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 1+3*(i-1))])),
      #        border = NA, col = rgb(0, 0, 0, 1/4))
      # polygon(x = c(1987:2015, 2015:1987),
      #        y = c(eval(parse(text = models$name[m]))$ucl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 2+3*(i-1))],
      #              rev(eval(parse(text = models$name[m]))$lcl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 2+3*(i-1))])),
      #        border = NA, col = rgb(0, 0, 0, 1/4))
      # polygon(x = c(1987:2015, 2015:1987),
      #        y = c(eval(parse(text = models$name[m]))$ucl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 3+3*(i-1))],
      #              rev(eval(parse(text = models$name[m]))$lcl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 3+3*(i-1))])),
      #        border = NA, col = rgb(0, 0, 0, 1/4))
      # #lines for predicted values:  
      lines(1987:2015, eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 1+3*(i-1))],
            lwd = 2, col="black", lty = 1)
      lines(1987:2015, eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 2+3*(i-1))],
            lwd = 2, col="black", lty = 2)
      lines(1987:2015, eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 3+3*(i-1))],
            lwd = 2, col="black", lty = 3)
      legend("topright", legend = month.name[c(1+3*(i-1), 2+3*(i-1), 3+3*(i-1))], lwd = 2, col = "black", lty = c(1, 2, 3))  
      title(paste(bays$name[b], paste0("Age ", models$age[m]), models$gear[m], sep = " - "), outer=TRUE)
    }
  }
}
dev.off()  #close the pdf writer

# Seasonal Timeline graphs with CIs in different colors ####
pdf("season_timeline_colorCIs.pdf", width = 11, height = 8.5) #create a pdf file to contain the plots
for(m in 1:length(models$name))
{
  for(b in bays$num[1:5])
  {
    par(mfrow = c(2, 2),     #4 plots per page, in a grid
        xpd = TRUE,          #allows printing in margins to stop the titles from getting cut off
        oma = c(2, 2, 2, 2)) #puts some space around the grid of plots
    for(i in seasons$num)
    {
      plot(0, 0, type="n",
           xlim = c(1987, 2015),
           ylim = c(0, 1),
           main = seasons$season[i],
           bty = "l",
           ylab = "Probability of Presence",
           xlab = "")
      #shaded polygons for CIs:
      polygon(x = c(1987:2015, 2015:1987),
              y = c(eval(parse(text = models$name[m]))$ucl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 1+3*(i-1))],
                    rev(eval(parse(text = models$name[m]))$lcl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 1+3*(i-1))])),
              border = NA, col = rgb(red = 24, green = 158, blue = 119, alpha = 255*1/4, maxColorValue = 255))
      polygon(x = c(1987:2015, 2015:1987),
              y = c(eval(parse(text = models$name[m]))$ucl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 2+3*(i-1))],
                    rev(eval(parse(text = models$name[m]))$lcl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 2+3*(i-1))])),
              border = NA, col = rgb(217, 95, 2, 255*1/4, maxColorValue = 255))
      polygon(x = c(1987:2015, 2015:1987),
              y = c(eval(parse(text = models$name[m]))$ucl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 3+3*(i-1))],
                    rev(eval(parse(text = models$name[m]))$lcl[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 3+3*(i-1))])),
              border = NA, col = rgb(117, 112, 179, 255*1/4, maxColorValue = 255))
      #lines for predicted values:  
      lines(1987:2015, eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 1+3*(i-1))],
            lwd = 2, col = rgb(24, 158, 119, maxColorValue = 255), lty = 1)
      lines(1987:2015, eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 2+3*(i-1))],
            lwd = 2, col = rgb(217, 95, 2, maxColorValue = 255), lty = 2)
      lines(1987:2015, eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == 3+3*(i-1))],
            lwd = 2, col = rgb(117, 112, 179, maxColorValue = 255), lty = 3)
      legend("topright", legend = month.name[c(1+3*(i-1), 2+3*(i-1), 3+3*(i-1))], 
             lwd = 2, 
             col = c(rgb(24, 158, 119, maxColorValue = 255), rgb(217, 95, 2, maxColorValue = 255), rgb(117, 112, 179, maxColorValue = 255)), 
             lty = c(1, 2, 3))  
      title(paste(bays$name[b], paste0("Age ", models$age[m]), models$gear[m], sep = " - "), outer=TRUE)
    }
  }
}
dev.off()  #close the pdf writer


#Explore line types
plot(c(1,2), c(1, 1), 
     ylim = c(0, 6),
     type = "l", lty=1, col="black", lwd=2)
lines(c(1,2), c(2, 2),
      lty=2, col="black", lwd=2)
lines(c(1,2), c(3, 3),
      lty=3, col="black", lwd=2)
lines(c(1,2), c(4, 4),
      lty=1, col="grey", lwd=2)
lines(c(1,2), c(5, 5),
      lty=2, col="grey", lwd=2)

# Graphs 1 for each month, lines for each bay, over years ####
#x=years, y=P(), main=months
pdf("bays_months.pdf", width = 11, height = 8.5) #create a pdf file to contain the plots
for(m in 1:length(models$name))
{
      par(mfrow = c(2, 2),     #4 plots per page, in a grid
        xpd = TRUE,          #allows printing in margins to stop the titles from getting cut off
        oma = c(2, 2, 2, 2)) #puts some space around the grid of plots
for(i in 1:12) #months
  {
  
    plot(0, 0, type="n",
         xlim = c(1987, 2015),
         ylim = c(0, 1),
         main = month.name[i],
         bty = "l",
         ylab = "Probability of Presence",
         xlab = "")
    for(b in bays$num[1:5])
    {
      
      lines(1987:2015, eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$month == i)],
            lwd = 2, col=c("black", "black", "black", "grey", "grey")[b], lty = c(1, 2, 3, 1, 2)[b])
      text(1998, 1.1, paste(models$name[m]))

    }
}
}
#Legend
  plot(0, 0, type="n",
       xlim = c(1987, 2015),
       ylim = c(0, 1),
       main = "",
       bty = "n",
       ylab = "",
       xlab = "", 
       xaxt="n", yaxt="n")
  legend("center", legend = bays$name[1:5], 
         col=c("black", "black", "black", "grey", "grey"), 
         lty = c(1, 2, 3, 1, 2))
dev.off()
      
# Graphs 1 for each bay, lines are selected years, over a seasonal cycle ####
#x=months, y=P(), main = bays
pdf("seasonal_bays.pdf", width = 11, height = 8.5) #create a pdf file to contain the plots
for(m in 1:length(models$name))
{
  par(mfrow = c(2, 2),     #4 plots per page, in a grid
      xpd = TRUE,          #allows printing in margins to stop the titles from getting cut off
      oma = c(2, 2, 2, 2)) #puts some space around the grid of plots
  for(b in bays$num[1:5])
  {
    
    plot(0, 0, type="n",
         xlim = c(1, 12),
         ylim = c(0, 1),
         main = bays$name[b],
         bty = "l",
         ylab = "Probability of Presence",
         xlab = "",
         xaxt = "n")
    axis(side = 1, at = 1:12, labels = month.abb, las = 2)
    text(5, 1.1, paste(models$name[m]))
    for(y in 1:4) #selected years
    {
      i=c(1988, 1992, 2000, 2010)[y]
      lines(1:12, 
            eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b & eval(parse(text = models$name[m]))$year == i)],
            lwd = 2, col=brewer.pal(5, "YlGnBu")[2:5][y], lty = 1)
    }
  }
}
dev.off()

#Legend
plot(0, 0, type="n",
     xlim = c(1987, 2015),
     ylim = c(0, 1),
     main = "",
     bty = "n",
     ylab = "",
     xlab = "", 
     xaxt="n", yaxt="n")
legend("topright", legend = c(1988, 1992, 2000, 2010), 
       col=brewer.pal(5, "YlGnBu")[2:5], 
       lty = 1, lwd = 2)
dev.off()

#find a color pallette: ####
display.brewer.all(n=6, colorblindFriendly = TRUE)

# Traditional GAM surface graph example ####
vis.gam(mwt1, view = c("month", "year"), cond = list(bay = 3), theta = 150, scale = "response")

# Maps of predicted probabilities ####
source(file = "BayStudyGIS.R")
# spplot(sfbs.pg, mwt1.pred$fit[mwt1.pred$month == 6 & mwt1.pred$year == 2010])
# spplot(sfbs.pg, c(0.021709860, 0.045809107, 0.036359178, 0.055434010, 0.001705079))
# sfbs.pg$mwt1.2010.6 <- c(0.021709860, 0.045809107, 0.036359178, 0.055434010, 0.001705079)
# sfbs.pg$mwt1.1991.01 <- c(0.2378117, 0.1115996, 0.3143610, 0.3530907, 0.4544418)
# spplot(sfbs.pg, "mwt1.2010.6", col.regions = c("white", brewer.pal(9, "YlGnBu")), 
#        at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# spplot(sfbs.pg, "mwt1.1991.01", col.regions = c("white", brewer.pal(9, "YlGnBu")), 
#        at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

# windows(height = 8.5, width = 11)
# grid.arrange(
# list(for(m in 2) #modeled probability
# {
#   for(y in 1990) #years
#   {
#     for(i in 1:12) #months
#     {
#       sfbs.pg$temp <- eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$year == y & eval(parse(text = models$name[m]))$month == i)]
#       png(eval(parse(text = paste0("~newfolder/tempmap",".png"))))
#       spplot(sfbs.pg, "temp", col.regions = c("white", brewer.pal(9, "YlGnBu")), 
#              at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
#              main = paste(models$name[m], month.name[i], y, sep = " - "))
# dev.off()
#     }
#   }
# }),
# nrow = 3, ncol = 4)
# grid.arrange(grobs= map.grobs,
#              nrow = 3, ncol = 4)
# 
# mwt1.pred$pclass[which(eval(parse(text = models$name[m]))$year == y & eval(parse(text = models$name[m]))$month == i)]

pdf("prob_maps.pdf", width = 8.5, height = 11) #create a pdf file to contain the plots
  #Legend
  par(mfrow=c(1,1))
  plot(0, 0, type="n",
       xlim = c(1987, 2015),
       ylim = c(0, 1),
       main = "Longfin Smelt Presence",
       bty = "n",
       ylab = "",
       xlab = "", 
       xaxt="n", yaxt="n")
  legend("center", 
         title = "Expected Probability",
         legend = rev(c("<0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1")), 
         fill=rev(c("white", brewer.pal(9, "YlGnBu"))))
for(m in 1:4) #modeled probability
{
  for(y in c(1988, 1992, 2000, 2010)) #years
  {
    par(mfrow = c(4,3),
        mar = c(0.1, 0.1, 1.2, 0.1),
        oma = c(0.1, 0.1, 2, 0.1))
    for(i in 1:12) #months
    {
      plot(sfbs.pg, 
           main = month.name[i],
           bty = "o",
           col = c("white", brewer.pal(9, "YlGnBu")[mwt1.pred$pclass[which(eval(parse(text = models$name[m]))$year == y & eval(parse(text = models$name[m]))$month == i)]]))
    }
    title(paste(models$gear[m], paste("Age", models$age[m]), y, sep = " - "), outer=TRUE)
  }
}
dev.off()

#12 months of maps in one row:
# "The Andy Warhol Figure"
pdf("prob_maps.pdf", width = 8.5, height = 11) #create a pdf file to contain the plots
#Legend
par(mfrow=c(1,1))
plot(0, 0, type="n",
     xlim = c(1987, 2015),
     ylim = c(0, 1),
     main = "Longfin Smelt Presence",
     bty = "n",
     ylab = "",
     xlab = "", 
     xaxt="n", yaxt="n")
legend("center", 
       title = "Expected Probability",
       legend = rev(c("<0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1")), 
       fill=rev(c("white", brewer.pal(9, "YlGnBu"))))
par(mfrow = c(4, 12),
    mar = c(0.1, 0.1, 1.2, 0.1),
    oma = c(0.1, 0.1, 0.1, 0.1))
for(m in 1:4) #modeled probability
{
  
  for(y in c(1988, 1992, 2000, 2010)) #years
  {
    for(i in 1:12) #months
    {
      plot(sfbs.pg, 
           #main = month.name[i],
           bty = "o",
           col = c("white", brewer.pal(9, "YlGnBu")[mwt1.pred$pclass[which(eval(parse(text = models$name[m]))$year == y & eval(parse(text = models$name[m]))$month == i)]]))
    }

  }
    title(paste(models$gear[m], paste("Age", models$age[m]), sep = " - "), outer=TRUE)
    title("January  February   March   April    May    June   July   August  September  October  November  December", outer = TRUE, line = -2)
    }
dev.off()


# Heat Maps ####
#same idea as above, but with boxes instead of maps:

#  One heat map for each bay:
pdf("prob_heat_1page.pdf", width = 11, height = 8.5) #create a pdf file to contain the plots
  par(mar = c(3, 3, 1, 1),
      #oma = c(rep(8, 4)),
      #mfrow = c(1, 1))
      oma = c(rep(4, 4)),
      mfrow = c(4, 5))
for(m in c(1, 3, 2, 4, 5, 6)) #modeled probability
{    
  for(b in bays$num[1:5])
  {
    image(matrix(as.vector(eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b)]), ncol=29, nrow = 12, byrow = TRUE),
          axes = FALSE, 
          main = paste(models$gear[m], paste("Age", models$age[m]), bays$name[b], sep = " - "), 
          col = terrain.colors(256),
          ylim = c(1, 0), #reversing the ylim makes 1987 at the top and 2015 at the bottom
          zlim=range(0, 1)) #puts all plots on a common scale
    axis(side = 1, at = seq(0, 1, length.out = 12), labels = month.abb, las = 2, tick = FALSE, line = NA)
    axis(side = 2, at = seq(0, 1, length.out = 29), labels = 1987:2015, las = 2, tick = FALSE, line = NA)
  }
}
dev.off()


# Heat maps with a simplified color scheme
#  Simplified = 5 colors instead of 256
pdf("prob_heat_5col_1page.pdf", width = 11, height = 8.5) #create a pdf file to contain the plots
par(mar = c(3, 3, 1, 1),
    #oma = c(rep(8, 4)),
    #mfrow = c(1, 1))
    oma = c(rep(4, 4)),
    mfrow = c(4, 5))
for(m in c(1, 3, 2, 4, 5, 6)) #modeled probability
{    
  for(b in bays$num[1:5])
  {
    image(matrix(as.vector(eval(parse(text = models$name[m]))$fit[which(eval(parse(text = models$name[m]))$bay == b)]), ncol=29, nrow = 12, byrow = TRUE),
          axes = FALSE, 
          main = paste(models$gear[m], paste("Age", models$age[m]), bays$name[b], sep = " - "), 
          col = terrain.colors(5),
          ylim = c(1, 0), #reversing the ylim makes 1987 at the top and 2015 at the bottom
          zlim=range(0, 1)) #puts all plots on a common scale
    axis(side = 1, at = seq(0, 1, length.out = 12), labels = month.abb, las = 2, tick = FALSE, line = NA)
    axis(side = 2, at = seq(0, 1, length.out = 29), labels = 1987:2015, las = 2, tick = FALSE, line = NA)
  }
}
dev.off()
