#My routine to for files.
##########################
#Checking working directory. This is where the images will be put into.
getwd()

#Listing objects in active workspace. 
ls()

#CAUTION! This will remove the all the objects listed in currently active workspace.
#Before using the command below, double check you saved current workspace and 
#save this workspace under new name before removing the workspace list. 
#rm(list = ls())



##########################
#ColorRamp for the "4th dimenssion"

#Assigning Color ramp with opacity percentage. 1 = 100%. 
colorfunction<-colorRampPalette(c(rgb(139/255,0,0,0.6),rgb(1,1,0,0.6),rgb(0,1,127/255,0.6)),alpha = TRUE)

# You can assign multiple different colors for the ramp up. 
#I found that the alpha/opacity doesn´t really work with color names. 
#You can find multiple different guides on the subject by googling "color palette in r"

#This is modified of some solution i found earlier when i had issues with color palette. 
#I will add the link and credit to the original poster when/if i find the post. ps. If you know the post/solution contact me!

color.range <- function(x, colors=colorfunction, colorlevels=100) {
  return( colorfunction (colorlevels) [ findInterval(x, seq(min(x),max(x), length.out=colorlevels)) ] )
}

# findInterval sorts through the variable and sets value according to the range of values in the variable. 
# This interval can be replaced by any scale that seems logical.
# Caution please note that the color.range return only a interval levels according the range. 
#Let say there is no minimum values of 0. then the first variable gets the outermost color instead of correct colorscaling.
#If you are using a set range of variable values I recommend using set values instead of "min(x),max(x)"

color.range <- function(x, colors=colorfunction, colorlevels=100) {
  return( colorfunction (colorlevels) [ findInterval(x, seq(0,1, length.out=colorlevels)) ] )
}



##########################
#Creating the data set

Applicant <- (1:10) 
Age <- rep(20:27,length.out=10)
Test <- rep(seq(60,95,by=5),length.out=10)
Task <-  c(seq(60,95,by=5),62,73)
Fit <- seq(0.45,0.90,by=0.05)
Salary<- sample(2500:4500,10)

Applicants <- cbind(Applicant,Age,Test,Task,Fit,Salary)
colnames(Applicants) <- c("Applicant Id","Age","Test Score","Task Score","Fit Percetage","Salary requirements")

summary(Applicants)




##########################
#Plot with symbols
#Please look at the image "4-dim_Applicants_01.pdf"
#Please note that this image has been made as a pdf. The marginals will be affected by change to the image type/size parameters. 





pdf("4-dim_Applicants_01.pdf", width = 9, height = 7, pointsize = 1)
par(mfrow=c(1,1),oma = c(0, 0, 0, 0),mar=c(8,10,3,16)) #Plot Outer Marginal Area and the image marginal.
#Please note that mfrow is here to ensure that only 1 image area is in use.
# "oma" and "mar" settings are c("Bottom marginal","left marginal","Top marginal","Right marginal")


layout(matrix(c(1,1,1,1,1,1,1,2,2,2),5,2,byrow=FALSE), widths=c(5,0.8),heights=c(1,1)) #Layout defines where the following images will be drawn into. 
# The width and height of different images have been set so, image_1 = (5,1) and image_2 = (0.8,1) 
#The layout can be manipulated purely with the structure of image number matrix.

#These values can me set manually like so:
plot(0,type="l",ylab=colnames(Applicants)[2],xlab=colnames(Applicants)[4],ylim=c(0,35),xlim=c(50,100), mgp=c(5,1,0),cex.main=4, cex.lab=4, yaxt="n", xaxt="n") 


#These values can be set for different sets of data with min() and max() values so, 
#if the range of observations change between looped datasets it won´t spoil the images. 


par(new=T) # This sets that the next plot won´t make a new plot. This fixes the plot/symbols command as an image_01 or "1st image"

#plot, (X,Y,Z,W) Where W is the color of the circles, and z is the size of the circles. 
#Please note that "inches = FALSE" only works when the X axis and the Z have roughly the same variable range. 
#In this dataset you can try the Test and Task scores in place of X and Z axis. Then swapping to "Salary requirements"
#as the Z (circle size) and you will notice the problem. By selecting "inches = TRUE" the r will resize the symbols according to
#the variable range. Making the biggest value 1-inch in diameter. 

symbols(x=Applicants[,4],y=Applicants[,2], circles=Applicants[,6]/1800, inches = FALSE, add = FALSE, ylim=c(0,35),xlim=c(50,100) ,yaxt="n",xaxt="n",ylab="",xlab="",fg="black",bg=color.range(Applicants[,5]))
axis(1, at=seq(0,100,5),labels=seq(0,100,5),las=1, cex.axis = 2)
axis(2, at=seq(0,100,5), labels=seq(0,100,5), las=2, cex.axis = 2)
#Horizontal lines
abline(h=seq(0,45,5),col="lightgrey",lty = 2,lwd=1)
#vertical lines
abline(v=seq(50,100,5),col="lightgrey",lty = 2,lwd=1)


#These two settings aren´t perfect but there is way around them. You can either choose to add a dummy values for "inches = TRUE", so that rest of the values are smaller.
#This means you need to select the draw points (X,Y) outside plot area in order to hide the "resizing symbol"

#For the " inches = FALSE " the Z is sized so that it´s proportional to X-axis values. So if the X-axis is from 0 to value 100 and
#the Z is from 2000 to 4500 you can rescale the Z values by dividing the Z values with some fitting value. ie. 900-1800.
 

#New marginal for the image_2
par(mar=c(8,11,6,5))

#plot for image_2
plot(y=c(1:100),x=rep(1,100),col=(colorfunction(100)), pch=15,cex=10,ylab="",xlab="",yaxt="n",xaxt="n",xlim=c(1,1),main="Fitness test \n score \n percentage",cex.main=1.5)
axis(2, at=c(seq(0,100,20)),labels=c("0%","20%","40%","60%","80%","100%"),las=2,cex.axis=3)
# if you need to cut your main texts to multiple line remember to use \n like shown above.



#closing the plot.
dev.off() 











