rm(list=ls())
library(FrF2) # tools for design and analysis of fractional factorials
library(pid)
library(MASS)
library(rsm)

HeliDesign <- read.csv("./HeliFFD/fracfact.csv")
#store then omit center points 
CP <- HeliDesign[33:36,]
HeliDesign <- HeliDesign[1:32,]


# main effects and all 2-way interactions
HeliMod_2way <- lm(time ~(.)^2, data = HeliDesign)
summary(HeliMod_2way)
# looking for contributing effects
pid::paretoPlot(HeliMod_2way)
# reduced model after stepwise backwards selection (alpha = 0.1)
step <- stepAIC(HeliMod_2way, direction = "both", trace = FALSE)

HeliMod_reduced <- step

pid::paretoPlot(HeliMod_reduced)
summary(HeliMod_reduced)

# effect plots
MEPlot(HeliMod_reduced) # main effects
IAPlot(HeliMod_reduced) # interactions
cubePlot(HeliMod_reduced, eff1 = "winglength", eff2 = "paperclips", 
         eff3 = "foldedbody",
         main = "Cube Plot for Distance to Optimal Helicopter")

##boxbehnke design
bb_design_1 <- bbd(
  k  = 3,            # Number of factors,
  n0 = 4,            # Number of center points,
  block = FALSE,     # Consider blocks or not in the design 
  randomize = FALSE)

names <-c("foldedbody","paperclips","winglength")

designbb <- bb_design_1[,3:5]
setnames(designbb,names)
#fwrite(designbb,"~/Desktop/PSU/SeniorYear/Spring Semester/STAT470W/HeliFFD/BBDesign.csv")

#response surface design
HeliD <- read.csv("./HeliFFD/BBDesign.csv")
model<-rsm(time~ FO(foldedbody,paperclips,winglength), data=HeliD) 
summary(model)

# countour plots
par(mfrow=c(1,3))
contour(model,~foldedbody+paperclips+winglength, 
        image=TRUE, at=summary(model$canonical$xs))

dev.off()

persp(model, ~ foldedbody + paperclips, image = TRUE,
      at = c(summary(model)$canonical$xs, Block="B2"),
      theta=30,zlab="Time",col.lab=33,contour="colors")
persp(model, ~ winglength + paperclips, image = TRUE,
      at = c(summary(model)$canonical$xs, Block="B2"),
      theta=30,zlab="Time",col.lab=33,contour="colors")
persp(model, ~ winglength + foldedbody, image = TRUE,
      at = c(summary(model)$canonical$xs, Block="B2"),
      theta=30,zlab="Time",col.lab=33,contour="colors")

#optimal design for longest time in the air
steepest(model)
