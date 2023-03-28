# A simple example
# Build a dataframe first using 4 vectors
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opinion <- c("Excellent", "No effect", "Some effect", "No effect", "Some effect")

# Create the dataframe from vectors
drugs <- data.frame(dose, drugA, drugB, opinion)
str(drugs)

# Difficult to understand a general plot of data
plot(drugs)

# Show drugs data with circles for data points, joined with 
# a blue line
attach(drugs)
plot(dose, type = "o", col = "blue")

? plot
# Choose generic x-y plotting
# option type="b" indicates that
# both points and lines should be
# plotted.
plot(dose, drugA, type = "b")
dose

# If we want to plot categorical data then
# convert to factor first
drugs$opinion <- as.factor(drugs$opinion)
# las = 2 rotates x-axis
plot(drugs$opinion, las = 2)

# par function - customise many features of a graph 
# including (fonts, colours, axes, and labels) 
# using PAR() function
? par
# We can also plot more than 1
# line on the chart using par 

help(par)
# Note - parameters set with the par() function apply
# to all graphs, whereas parameters specified in the plot() 
# functions only apply to a specific graph

# adding no.readonly=TRUE produces a list
# of current graphical settings and can be modified

# Here I'm storing the contents of par to a variable
# and I can then restore defaults to contents of this variable
opar <- par(no.readonly = TRUE)
par(new = TRUE)
plot(dose, drugB, type = "b")

# lty = line type
# lwd = line width

# lty=2 dashed line
# pch=17 solid triangle
par(lty = 2, pch = 17)
plot(dose, drugA, type = "b")
par(opar)


-------------------------------------
# Specify graphical parameters using 
# optionname=value
plot(dose, drugA, type = "b", lty = 2, pch = 17)

# dotted line, 3 times wider than default
# Connecting points are filled squares
plot(drugA, type = "b", lty = 3, lwd = 3, pch = 15, cex = 2, ylim = c(0, 100))
title(main = "Drug dosage", col.main = "blue", font.main = 4)

# Graph Drug B with red dashed line and square points
lines(drugB, type = "o", pch = 22, lty = 2, col = "red")

# Now we'll manually edit the chart so that it can automatically change to suit
# values in the vectors

# Calculate range from 0 to max value of drug A and drug B
graph_range <- range(0, drugA, drugB)
graph_range

# Graph drug A using y axis that ranges from 0 to max 
# value in drug A or drug B vector.  Turn off axes and 
# annotations (axis labels) so we can specify them ourself
plot(drugA, type = "o", col = "blue", ylim = graph_range, axes = FALSE, ann = FALSE)

# Graph Drug B with red dashed line and square points
lines(drugB, type = "o", pch = 22, lty = 2, col = "red")

# Make x axis with ml labels
axis(1, at = 1:5, lab = c("20 ml", "40 ml", "60 ml", "80 ml", "100 ml"))

# Make y axis with horizontal labels that display ticks at 
# every 5 marks. 5*0:graph_range[2] is equivalent to c(0, 5, 10, ..., 60).
axis(2, las = 1, at = 5 * 0:graph_range[2])

# Create box around plot
box()

# Label the x and y axes with dark green text
title(xlab = "Dosage (ml)", col.lab = rgb(0, 0.5, 0))
title(ylab = "Drugs", col.lab = rgb(0, 0.5, 0))

# Create a legend at (1, graph_range[2]) that is slightly smaller 
# (cex) and uses the same line colours and points used by 
# the actual plots 
legend(1, graph_range[2], c("Drug A", "Drug B"), cex = 0.8,
       col = c("blue", "red"), pch = 21:22, lty = 1:2)

# Create a title with a red, bold/italic font
title(main = "Drug dosage", col.main = "red", font.main = 4)

# Lets enhance the simple example
# Save current graphical parameter settings - restore later
opar <- par(no.readonly = TRUE)
# Modify default graphical parameters - graph is 2" wide & 3" tall
par(pin = c(2, 3))
# Lines will be twice the default width and 
# symbols will be 1.5 times default size
par(lwd = 2, cex = 1.5)
# Axis text set to italic and scaled to 75 % of the default
par(cex.axis = .75, font.axis = 3)
# first plot created using filled red circles and dashed lines
plot(dose, drugA, type = "b", pch = 19, lty = 2, col = "red")
# second plot is created using filled green diamonds
# and a blue border and blue dashed lines
plot(dose, drugB, type = "b", pch = 23, lty = 6, col = "blue", bg = "green")
# restore the original graphical parameter settings
par(opar)

# Complete example of creating a line chart including axes and labels
plot(dose, drugA, type = "b",
     col = "red", lty = 2, pch = 2, lwd = 2,
     main = "Clinical Trials for Drug A",
     sub = "This is hypothetical data",
     xlab = "Dosage", ylab = "Drug Response",
     xlim = c(0, 60), ylim = c(0, 70))

# use title() to add a title and axis labels to a plot
title(main = "My Title", col.main = "red",
      sub = "My Subtitle", col.sub = "blue",
      xlab = "My X label", ylab = "My Y label",
      col.lab = "green", cex.lab = 0.75)

# Custom axes example
# Specify data
x <- c(1:10)
y <- x
z <- 10 / x
# Copy current settings
opar <- par(no.readonly = TRUE)
# Increase margins
par(mar = c(5, 4, 4, 8) + 0.1)

# Advance to new graphics frame
plot.new()
# Create x and y elements
x <- c(1:10)
y <- 1 / x
# create new chart. Suppress annotations for y axis. 
# Show annotations for x-axis
plot(x, y, type = "b",
     pch = 21, col = "red",
     yaxt = "n", lty = 3, ann = FALSE)
? axis
# Draw the axes
# side 1 = x axis, 2 = y axis, 3 = top, 4 = right
# at = points when ticks are to be drawn
# las - labels are parallel (=0) or perpendicular(=2) to axis
axis(2, at = y, labels = y, col.axis = "red", las = 0, tck = .05)
# tck reference depth of tick mark
# cex.axis references text size

# Show axis on right of chart
axis(4, at = y, labels = round(y, digits = 2),
     col.axis = "blue", las = 0, cex.axis = 0.5, tck = .05)

# Add text to side = 4 axis
? mtext
mtext("y=1/x", side = 4, line = 3, cex.lab = 1, las = 2, col = "blue")
# Add titles and text
title("An Example of Creative Axes",
      xlab = "X values",
      ylab = "Y=X")

par(opar)

# adds solid horizontal lines at y = 1, 0.5, and 0.7,
abline(h = c(1, 0.5, 0.7))
? seq
# Draw vertical lines using the seq() function. This example draws a line from 1 to 10, in 0.5 increments
abline(v = seq(1, 10, 0.5), lty = 2, col = "blue")

# Example of drug data chart
# Input data
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly = TRUE)
# Increases line, text, symbol, and label size
par(lwd = 2, cex = 1.5, font.lab = 2)
# Generates the graph
plot(dose, drugA, type = "b",
     pch = 15, lty = 1, col = "red", ylim = c(0, 60),
     main = "Drug A vs. Drug B",
     xlab = "Drug Dosage", ylab = "Drug Response")
# Add a line
lines(dose, drugB, type = "b",
      pch = 17, lty = 2, col = "blue")
abline(h = c(30), lwd = 1.5, lty = 2, col = "gray")
# Adds minor tick marks
library(Hmisc)
minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
# Adds a legend
legend("topleft", inset = .05, title = "Drug Type", c("A", "B")
       lty = c(1, 2), pch = c(15, 17), col = c("red", "blue"))
par(opar)


attach(mtcars)
opar <- par(no.readonly = TRUE)
# Include the graphical parameter
# mfrow = c(nrows, ncols) to create a matrix of 
# nrows × ncols plots that are filled in by row
# Here we have 2 rows by 2 cols to display our charts
par(mfrow = c(2, 2))
# Our 4 charts
plot(wt, mpg, main = "Scatterplot of wt vs. mpg")
plot(wt, disp, main = "Scatterplot of wt vs. disp")
# hist() includes a default title - use main = "" to suppress it
# or ann = FALSE to suppress all titles and labels
hist(wt, main = "Histogram of wt")
boxplot(wt, main = "Boxplot of wt")
# Reset parameters
par(opar)
detach(mtcars)

# arrange three plots in three rows and one column
attach(mtcars)
opar <- par(no.readonly = TRUE)
par(mfrow = c(3, 1))
hist(wt)
hist(mpg)
hist(disp)
par(opar)
detach(mtcars)

# layout(mat), where mat is a matrix object
# specifying the location of the multiple plots to combine
# Eg one figure is placed in row 1 
# and two figures are placed in row 2:
attach(mtcars)
help(layout)
# Show layout 
layout.show(3)

# here one figure is placed in row 1 
# and two figures are placed in row 2:
nf <- layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
layout.show(nf)
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

# one figure is again placed in row 1 
# and two figures are placed in row 2. 
# But the figure in row 1 is one-third 
# the height of the figures in row 2
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE),
       widths = c(3, 1), heights = c(1, 2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

install.packages("vcd")

library(vcd)
# table() returns a contingency table, an object 
# of class "table", an array of integer values. 
# Note that the result is always an array, 
# a 1D array if one factor is given
help(table)
counts <- table(Arthritis$Improved)
counts

# Simple bar chart
barplot(counts,
        main = "Simple Bar Plot",
        xlab = "Improvement", ylab = "Frequency")

# Horizontal bar plot
barplot(counts,
        main = "Horizontal Bar Plot",
        xlab = "Frequency", ylab = "Improvement",
        horiz = TRUE)

# If categorical variable to be plotted is a factor or ordered factor
# you can create a vertical bar plot quickly with the plot() function
# Produces same bar charts as previous ones using Table function
plot(Arthritis$Improved, main = "Simple Bar Plot",
     xlab = "Improved", ylab = "Frequency")


plot(Arthritis$Improved, horiz = TRUE, main = "Horizontal Bar Plot",
     xlab = "Frequency", ylab = "Improved")

# Stacked and grouped bar plots
library(vcd)
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts

# If height is a matrix rather than a vector
# the resulting graph will be a stacked (below) or grouped bar plot
barplot(counts,
        main = "Stacked Bar Plot",
        xlab = "Treatment", ylab = "Frequency",
        col = c("red", "yellow", "green"),
        legend = rownames(counts))

# If beside=TRUE, each column of the matrix represents 
# a group, and the values in each column are 
# juxtaposed rather than stacked.

# Grouped bar plot

barplot(counts,
        main = "Grouped Bar Plot",
        xlab = "Treatment", ylab = "Frequency",
        col = c("red", "yellow", "green"),
        legend = rownames(counts), beside = TRUE)

# Mean bar plots
help(state) # Data sets of 50 states of US
states <- data.frame(state.region, state.x77)
states

# Generate means data subset using illiteracy rate from states
# separated into state.region subset and aggregated into mean information
means <- aggregate(states$Illiteracy, by = list(state.region), FUN = mean)
means

# using the title() function is equivalent to adding
# the main option in the plot call
# option names.arg = means$Group.1 is added to provide labels
barplot(means$x, names.arg = means$Group.1)
title("Mean Illiteracy Rate")

# Now sort by mean from smallest to largest
means <- means[order(means$x),]
means

# re-plot data
barplot(means$x, names.arg = means$Group.1)
title("Mean Illiteracy Rate")

# Tweak the bar plot

# Increases the size of the y margin
par(mar = c(5, 8, 4, 2))

# Rotates the x-axis bar labels
par(las = 2)
counts <- table(Arthritis$Improved)
barplot(counts,
        main = "Treatment Outcome",
        horiz = TRUE,
        # Decreases the font size
        cex.names = 0.8,
        # Changes the labels text
        names.arg = c("No Improvement", "Some Improvement",
                      "Marked Improvement"))

# Spinograms
# stacked bar plot is rescaled so that the height of
# each bar is 1 and the segment heights represent proportions
library(vcd)
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main = "Spinogram Example")
# The larger percentage of patients with
# marked improvement in the Treated condition is
# quite evident when compared with the Placebo
# condition.

detach(Arthritis)

# Pie charts
# 2 rows by 2 cols - for 4 charts
par(mfrow = c(2, 2))

# First chart - shows title with chart broken into
# slices vector
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls,
    main = "Simple Pie Chart")

# Add percentages to the pie chart
# Calculate each percent for labels on chart
pct <- round(slices / sum(slices) * 100)
# Create label structure
lbls2 <- paste(lbls, " ", pct, "%", sep = "")
# Create the pie chart
pie(slices, labels = lbls2, col = rainbow(length(lbls2)),
    main = "Pie Chart with Percentages")

# Now create a chart from the table
# Need to swap to R 3.3.2.0 to use the plotrix library
install.packages("plotrix")
library(plotrix)
# Use the same label structure as aove - same data
pie3D(slices, labels = lbls, explode = 0.1, main = "3D Pie Chart ")


# Create a table structure for state.region (showing summary)
state.region
mytable <- table(state.region)
mytable
# Use table headers for chart labels
lbls3 <- paste(names(mytable), "\n", mytable, sep = "")
# Create the 3D pie chart
pie(mytable, labels = lbls3,
    main = "Pie Chart from a Table\n (with sample sizes)")

# Fan plot - attempt to improve the readability
# of pie charts
# slices are rearranged to overlap each other
# and the radii are modified so that each slice is visible
library(plotrix)
# Reset layout back to 1 chart
par(mfrow = c(1, 1))
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
fan.plot(slices, labels = lbls, main = "Fan Plot")

# Histograms
# Set 2 x 2 display
par(mfrow = c(2, 2))
# Show simple historgram
# with default values
# Each bin (bar) represents a range 
# Eg first bar is values within (10-15), second is (15-20) etc
hist(mtcars$mpg)

# Show histogram with 12 breaks
# Red bars on chart, x-axis = "Miles per gallon"
# and main heading = "Coloured histogram with 12 bins"
# More breaks = more detail as more space to show data
mtcars$mpg
hist(mtcars$mpg,
     breaks = 12, # no of bins on chart
     col = "red",
     xlab = "Miles Per Gallon",
     main = "Coloured histogram with 12 bins")

# maintains the same colours, bins, labels, and titles as the
# previous plot but adds a density curve and rug-plot overlay
# Rug-plot described in next section
hist(mtcars$mpg,
     freq = FALSE,
     breaks = 12,
     col = "red",
     xlab = "Miles Per Gallon",
     main = "Histogram, rug plot, density curve")
# Rug plot is 1-dimensional representation of
# actual data values
# Jitter data to add a small rnandom amount
# to each data point - to avoid overlapping points
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col = "blue", lwd = 2)

# similar to the second but has a superimposed normal
# curve and a box around the figure
x <- mtcars$mpg
h <- hist(x,
          breaks = 12, # no of bins on chart
          col = "red",
          xlab = "Miles Per Gallon",
          main = "Histogram with normal curve and box")

xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
# diff returns suitably lagged and iterated differences
# Created from online post
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)
# Put a box around the chart
box()

# Create a sequence of values within min and max range of x
# with maximum 40 values
xfit

# Create a sequence of values from x which are lagged
# and which have mean and SD values for x
yfit

# Kernel density plot ------------------------------------
# an effective way to view the distribution of a 
# continuous variable
# Not affected by no of bins. Smoother
# distribution by smoothing out noise
par(mfrow = c(2, 1))
d <- density(mtcars$mpg)
# Creates minimal graph with defaults
plot(d)

d <- density(mtcars$mpg)
# Adds title
plot(d, main = "Kernel Density of Miles Per Gallon")
# Colours the curve blue and fills the
# area under the curve with solid red
# polygon() function draws a polygon whose vertices
# are given by x and y. These values are provided
# by the density() function
polygon(d, col = "red", border = "blue")
# Adds a brown rug - lines just above x axis
rug(mtcars$mpg, col = "brown")

# bandwidth is a measure of how closely you 
# want the density to match the distribution
# Automatically generated
d


# Comparative density plots ---------------------------
# Install sm package
install.packages("sm")
library(sm)
par(mfrow = c(1, 1))
# Attach to mtcars data frame
attach(mtcars)
cyl
# Create a factor of cyliners
# with 4 levels and each level assigned to
# a specific label
cyl.f <- factor(cyl, levels = c(4, 6, 8),
                labels = c("4 cylinder", "6 cylinder",
                           "8 cylinder"))

cyl.f
# Create the density plot 
# using mpg, cyl from mtcars, with x label
sm.density.compare(mpg, cyl, xlab = "Miles Per Gallon")
# Set title
title(main = "MPG Distribution by Car Cylinders")
# Create a vector of colours
# from 2 to max level in cyl.f factor (2 3 4)
colfill <- c(2:(1 + length(levels(cyl.f))))
colfill
# legend is added to the plot via the legend() function.
# The locator(1) option indicates that you ll place 
# the legend interactively by clicking in the
# graph where you want the legend to appear

# Second option provides a character vector
# of the labels. The third option assigns a colour 
# from the vector colfill to each level of cyl.f.
legend(locator(1), levels(cyl.f), fill = colfill)
detach(mtcars)

# Boxplot ------------------------------------
# Show a boxplot with title and y label
# Lowest line = lower hinge
# Lowest line in box = lower quartile
# Middle line in box = median
# Top line in box = upper quartile
# Top line in chart = upper hinge
boxplot(mtcars$mpg, main = "Box plot", ylab = "Miles per Gallon")
# Show stats for the boxplot
boxplot.stats(mtcars$mpg)


mpg
cyl
# Draw a separate boxplot for variable
# mpg for each value of cyl
# using data from mtcars data frame
boxplot(mpg ~ cyl, data = mtcars,
        main = "Car Mileage Data",
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon")

# Dotchart  - plotting a large number of 
# labeled values on a simple horizontal scale
dotchart(mtcars$mpg, labels = row.names(mtcars), cex = .7,
         main = "Fuel Mileage for Car Models",
         xlab = "Miles Per Gallon")

# Show dotchart by mpg
# First sort the daatset by mpg
x <- mtcars[order(mtcars$mpg),]
x
# Then encode cly as a factor
x$cyl <- factor(x$cyl)
x$cyl
# Add 
x$colour[x$cyl == 4] <- "red"
x$colour[x$cyl == 6] <- "blue"
x$colour[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg,
         labels = row.names(x),
         cex = .7,
         groups = x$cyl,
         gcolor = "black",
         color = x$colour,
         pch = 19,
         main = "Fule Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")


# Colours also available in rgb() and hsv()

# RColorBrewer package is popular 
# for creating attractive colour palettes
# use the brewer.pal(n, name) function to 
# generate a vector of colours.

# Eg this returns a vector of seven colors 
# in hexadecimal format from the Set1 palette
library(RColorBrewer)
n <- 7
mycolors <- brewer.pal(n, "Set1")
barplot(rep(1, n), col = mycolors)

# list of the available palettes
brewer.pal.info

# Lots of grey output
# Greys are generated with the gray() function 
# in the base installation.
# In this case, you specify gray levels as a vector of 
# numbers between 0 and 1. gray(0:10 / 10) produces 10 gray levels.

n <- 10
mycolors <- rainbow(n)
pie(rep(1, n), labels = mycolors, col = mycolors)
mygrays <- gray(0:n / n)
pie(rep(1, n), labels = mygrays, col = mygrays)

