#### Project name: total alcohol consumption

#install.packages("Boruta")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("corrplot")
#install.packages("bda")
#install.packages("MASS")
#install.packages("dplyr")
#install.packages("broom")

library(Boruta)
library(ggplot2)
library(GGally)
library(corrplot)
library(bda)
library(MASS)
library(dplyr)
library(broom)

###DATA MANIPULATION
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d1$totalgrade=(d1$G1+d1$G2+d1$G3)/60*100
d1$Talc=d1$Dalc+d1$Walc

##CONTOUR PLOT
indx <- sapply(d1, is.factor)
d1[indx] <- lapply(d1[indx], function(x) as.numeric((x)))
res <- cor(d1, method = "pearson", use = "complete.obs")
corrplot(res, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 6) +
  theme(strip.text = element_text(size = 20, face = "bold"))+ title("Correlation Plot")

####DATA SUBSETTING
d1<-subset(d1,select=c("totalgrade","goout","traveltime","Medu","failures","Dalc","Walc","Talc"))
d1$Talc= round((d1$Talc)/2, 0)

ggpairs(d1,columns =c("totalgrade","goout","traveltime","Medu","failures","Talc") )


#####UNIVARIATE ANALYSIS
ggplot(d1, aes(x = totalgrade)) + stat_ecdf()
ggplot(d1, aes(x = totalgrade)) + geom_histogram()

#only total grades gives an almost normal plot, rest are irregular hence i think not very important statitically
ggplot(d1, aes(x = totalgrade)) + geom_density()
ggplot(d1, aes(x = Medu)) + geom_density()
ggplot(d1, aes(x = Talc)) + geom_density()
ggplot(d1, aes(x = goout)) + geom_density()
ggplot(d1, aes(x = failures)) + geom_density()
ggplot(d1, aes(x = traveltime)) + geom_density()


########### BIVARIATE ANALYSIS ###########

#####CATEGORICAL VARIABLES

summary(d1)
#d1$Consumption = recode_factor(d1$Walc, '1' = "very low", '2' = "low", '3' = "medium" , '4' = "high", '5' = "very high")
d1$Grade[d1$totalgrade>=0 & d1$totalgrade<25] <- "Very Low Grades"
d1$Grade[d1$totalgrade>=25 & d1$totalgrade<50] <- "Low Grades"
d1$Grade[d1$totalgrade>=50 & d1$totalgrade<75] <- "Above average grades"
d1$Grade[d1$totalgrade>=75 & d1$totalgrade<100] <- "Good grades"


# Taking total alcohol as the explanatory variable. Drawing jitter plot.

nice.palette = c("#999999", "#E69F00", "#56B4E9", "#0072B2") 
gg = ggplot(d1, aes(x = d1$Talc, y = d1$Grade, color = d1$Grade)) + geom_jitter(width = 0, height = 0.2) 

gg + ggtitle("Total grades") + scale_color_manual(values = nice.palette)

# Checking the distribution of total alcohol consumption variable.
gg = ggplot(d1, aes(x = d1$Talc, fill = d1$Grade)) + geom_histogram(breaks = seq(0, 10, 1)) + ggtitle("Total grades wrt total alcohol consumption")
gg + scale_fill_manual(values = nice.palette) + labs(x ="Total alcohol consumption",y = "Count",fill = "Grades")

# Total grades conditioned on total alcohol consumption

ggplot(d1, aes(x = d1$Talc, ..count.., fill = d1$Grade)) + geom_density(position = "stack") + ggtitle("Total grades") + scale_fill_manual(values = nice.palette)

ggplot(d1, aes(x = d1$Talc, ..count.., fill = d1$Grade)) + geom_density(position = "fill") + ggtitle("Total grades conditioned on total alcohol consumption") + scale_fill_manual(values = nice.palette)+ labs(x ="Total alcohol consumption",y = "Count",fill = "Grades")


# Making probabilistic predictions
library(MASS)

alc.polr = polr(as.factor(d1$Grade) ~ d1$Talc, data = d1) 
#install.packages("arm")
library(arm) 
display(alc.polr)

# Making prediction for an individual, # below graph will show distribution for a person with alcohol consumption = 9

prediction = coefficients(alc.polr) *  9
latent = prediction + rlogis(10000) 
gg = ggplot(as.data.frame(latent), aes(x = latent)) + geom_density() 
gg + geom_vline(xintercept = alc.polr$zeta, color = "red")

# the plot shows that very low alcohol consumption is most likely for the person with final grades as 90.

beta = coefficients(alc.polr) 
zeta = alc.polr$zeta 
library(boot) 
inv.logit(zeta[1] - beta * 9)

#There is 40% chance a person with total alcohol consumption as 9 will have above average grade.

# Graphing and checking the model

alc = seq(min(d1$Talc), max(d1$Talc), (max(d1$Talc) - min(d1$Talc))/394) 
alc.probs = predict(alc.polr, newdata = data.frame(alc), type = "prob") 
alc.probs.df = data.frame(alc, alc.probs) 
names(alc.probs.df) = c("Alcohol", "Very Low Grades", "Low Grades", "Above average grades","Good grades") 
library(tidyr) 
alc.probs.long = alc.probs.df %>% gather(Likelihood, Probability, c(2:5)) # Put levels in reverse order 
alc.probs.long$Likelihood = factor(alc.probs.long$Likelihood, levels = c("Very Low Grades", "Low Grades", "Above average grades","Good grades")) 
reverse.palette = c("#56B4E9", "#E69F00", "#999999", "#0072B2","#800000") 
gg = ggplot(alc.probs.long, aes(x = Alcohol , y = Probability, group = Likelihood,
                                color = Likelihood)) + geom_line() + ggtitle("Total grades") 
gg + scale_color_manual(values = reverse.palette)

gg = ggplot(alc.probs.long, aes(x = Alcohol, y = Probability, group = Likelihood, fill = Likelihood)) + geom_area() + ggtitle("Total grades") 
gg + scale_fill_manual(values = reverse.palette)

# Checking the fit 

consump.fitted = fitted.values(alc.polr)[, 2] + 0 * fitted.values(alc.polr)[, 3] 
consump.resid = d1$totalgrade - consump.fitted
alc.polr.df = data.frame(d1, .fitted = consump.fitted, .resid = consump.resid)
# Sort 
alc.polr.df = alc.polr.df[order(d1$Talc), ] 
gg = ggplot(alc.polr.df, aes(x = d1$Talc, y = d1$totalgrade)) + geom_jitter(width = 0, height = 0.2, aes(color = d1$Grade)) + geom_line(aes(x = d1$Talc, y = .fitted)) 
gg + ggtitle("Likelihood of consuming alcohol") + scale_color_manual(values = nice.palette)

#Looking at the residuals

gg = ggplot(alc.polr.df, aes(x = d1$Talc, y = .resid)) + geom_point() 
gg + geom_smooth(method.args = list(degree = 1))

###########################################################################

#Analysis of Mothers education

medu.gg <- ggplot(d1, aes(x = Medu, y = totalgrade)) + geom_point() + geom_smooth() + ggtitle("Variation of Total grade with Mother's education")
medu.gg

medu.loess <- loess(totalgrade ~ Medu, data = d1)
medu.loess
medu.loess.df <- augment(medu.loess)

#Fitting Loess
ggplot(medu.loess.df, aes(x = Medu, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0) + ggtitle("Residual plot with Mother's education as explanatory variable")

#Calculating Variance
var_medu <- var(medu.loess.df$.fitted)/var(log2(d1$totalgrade))
var_medu

#checking for homoskedasticity using spread- location plot

ggplot(medu.loess.df, aes(x = .fitted, y = sqrt(abs(.resid)))) + geom_point() +
  geom_smooth()

#RLM model
library(MASS)
ggplot(d1, aes(x = Medu, y = totalgrade)) + geom_point() + geom_smooth(method = "lm",se = FALSE) + geom_smooth(method = "rlm", se = FALSE, col = "orange")

Medu <- d1$Medu
total <- d1$total
medu.rlm <- rlm(total ~ Medu, psi = psi.bisquare)
tidy(medu.rlm)
medu.rlm.df <- augment(medu.rlm)
ggplot(medu.rlm.df, aes(x = Medu, y = .resid)) + geom_point()+ geom_smooth() + geom_abline(slope = 0)

#linear model

medu.lm <- lm(totalgrade ~ Medu, data = d1)
medu.lm
medu.lm.df <- augment(medu.lm)
ggplot(medu.lm.df, aes(x = Medu, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0)

#quadratic model
medu.q <- lm(totalgrade ~ Medu + I(Medu ^2), data = d1)
medu.q
medu.q.df <- augment(medu.q)
ggplot(medu.q.df, aes(x = Medu, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0)

#square root model
ggplot(medu.q.df, aes(x = Medu, y = sqrt(abs(.resid)))) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0)



###################################
#Analysis of Goingout

goout.gg <- ggplot(d1, aes(x = goout, y = totalgrade)) + geom_point() + geom_smooth() + ggtitle("Variation of Total grade with goingout")
goout.gg

goout.loess <- loess(totalgrade ~ goout, data = d1)
goout.loess
goout.loess.df <- augment(goout.loess)

#Loess model
ggplot(goout.loess.df, aes(x = goout, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0) + ggtitle("Residual plot with goingout as explanatory variable")

#Calculating Variance
var_goout<- var(goout.loess.df$.fitted)/var(log2(d1$totalgrade))
var_goout


#Linear model

goout.lm <- lm(totalgrade ~ goout, data = d1)
goout.lm
goout.lm.df <- augment(goout.lm)
ggplot(goout.lm.df, aes(x = goout, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0)

#Quadratic model
goout.q <- lm(totalgrade ~ goout + I(goout ^2), data = d1)
goout.q
goout.q.df <- augment(goout.q)
ggplot(goout.q.df, aes(x = goout, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0)

#Square root model
ggplot(goout.q.df, aes(x = goout, y = sqrt(abs(.resid)))) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0)

###################################

#Analysis of Total Alchohol consumption

Talc.gg <- ggplot(d1, aes(x = Talc, y = totalgrade)) + geom_point() + geom_smooth() + ggtitle("Variation of Total grade with Total alchohol consumption")
Talc.gg

#Loess model
Talc.loess <- loess(totalgrade ~ Talc, data = d1)
Talc.loess
Talc.loess.df <- augment(Talc.loess)

ggplot(Talc.loess.df, aes(x = Talc, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0) + ggtitle("Residual plot with Total alchohol consumption as explanatory variable")

#Variance
var_Talc<- var(Talc.loess.df$.fitted)/var(log2(d1$totalgrade))
var_Talc

#linear model

Talc.lm <- lm(totalgrade ~ Talc, data = d1)
Talc.lm
Talc.lm.df <- augment(Talc.lm)
ggplot(Talc.lm.df, aes(x = Talc, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0)

#quadratic model
Talc.q <- lm(totalgrade ~ Talc + I(Talc ^2), data = d1)
Talc.q
Talc.q.df <- augment(Talc.q)
ggplot(Talc.q.df, aes(x = Talc, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0)

#square root model
ggplot(Talc.q.df, aes(x = Talc, y = sqrt(abs(.resid)))) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0)


######################################

#Analysis of traveltime

#Loess model
gg6.loess <- ggplot(d1, aes(x = traveltime, y = totalgrade)) + geom_point() + geom_smooth() + ggtitle("Variation of Total grade with Travel time")
gg6.loess

gg6.loess <- loess(totalgrade ~ traveltime, data = d1)

gg6.loess.df <- augment(gg6.loess)

ggplot(gg6.loess.df, aes(x = traveltime, y = .resid)) + geom_point() + geom_smooth() +
  geom_abline(slope = 0, intercept = 0) + ggtitle("Residual plot with Travel time as explanatory variable")

#Variance calculation
var_traveltime<- var(gg6.loess.df$.fitted)/var(log2(d1$totalgrade))
var_traveltime



#TRIVARIATE ANALYSIS

d1_copy <- d1
d1_copy$Medu <- as.factor(d1_copy$Medu)
d1_copy$Talc = as.factor(d1_copy$Talc)
d1_copy$goout = as.factor(d1_copy$goout)
d1_copy$failures = as.factor(d1_copy$failures)
d1_copy$traveltime = as.factor(d1_copy$traveltime)

##ggpairs plot of all different ocmbinations
ggpairs(d1_copy, columns = c(1,2,3), aes(colour = goout))
ggpairs(d1_copy, columns = c(1,2,3), aes(colour = traveltime))
ggpairs(d1_copy, columns = c(1,2,4), aes(colour = goout))
ggpairs(d1_copy, columns = c(1,2,4), aes(colour = traveltime))
ggpairs(d1_copy, columns = c(1,2,5), aes(colour = goout))
ggpairs(d1_copy, columns = c(1,2,5), aes(colour = failures))
ggpairs(d1_copy, columns = c(1,2,8), aes(colour = goout))
ggpairs(d1_copy, columns = c(1,2,8), aes(colour = Talc))
ggpairs(d1_copy, columns = c(1,3,4), aes(colour = traveltime)) + ggtitle("Pairwise relationship between total grades, mother's education and travel time")
ggpairs(d1_copy, columns = c(1,3,4), aes(colour = medu_f))
ggpairs(d1_copy, columns = c(1,3,5), aes(colour = traveltime))
ggpairs(d1_copy, columns = c(1,3,5), aes(colour = failures))
ggpairs(d1_copy, columns = c(1,3,8), aes(colour = traveltime))
ggpairs(d1_copy, columns = c(1,3,8), aes(colour = Talc))
ggpairs(d1_copy, columns = c(1,4,5), aes(colour = Medu))
ggpairs(d1_copy, columns = c(1,4,5), aes(colour = failures))
ggpairs(d1_copy, columns = c(1,4,8), aes(colour = Medu))
ggpairs(d1_copy, columns = c(1,4,8), aes(colour = Talc))     
ggpairs(d1_copy, columns = c(1,5,8), aes(colour = failures))
ggpairs(d1_copy, columns = c(1,5,8), aes(colour = Talc))

####model fitting for TRAVEL TIME VS MOTHER'S EDUCATION
d1.loess = loess(totalgrade ~ traveltime * Medu, data = d1, span = 1, family = "symmetric")
d1.grid = expand.grid(traveltime = (seq(1,4, by = 1)), Medu = (seq(0,4, by = 1)))
d1.predict = predict(d1.loess, newdata = d1.grid)
d1.grid.df = data.frame(d1.grid, fit = as.vector(d1.predict))

ggplot(d1.grid.df, aes(x = Medu, y = fit, group = traveltime, color = factor(traveltime))) +
  geom_line() + ggtitle("Total grade VS Mother's education") + ylab("Total grades")

fitted.loss = fitted.values(d1.loess)
residual.loss = residuals(d1.loess)
d1.loess.df = data.frame(fitted.loss, residual.loss)

ggplot(d1.loess.df, aes(x = fitted.loss, y = sqrt(abs(residual.loss)))) + 
  geom_point() + geom_smooth(method.args = list(degree = 1)) + 
  ggtitle("Spread location plot") + xlab("fitted loss") + ylab("residual loss")

var(d1.loess.df$fitted.loss)/var(d1$totalgrade)
#6.84%

############# model fitting  for alcohol consumption VS goout
d1.loess = loess(totalgrade ~ traveltime * Talc, data = d1, span = 1, family = "symmetric")
d1.grid = expand.grid(traveltime = (seq(1, 4, by = 1)), Talc = (seq(1,5, by = 1)))
d1.predict = predict(d1.loess, newdata = d1.grid)
d1.grid.df = data.frame(d1.grid, fit = as.vector(d1.predict))

ggplot(d1.grid.df, aes(x = Talc, y = fit, group = traveltime, color = factor(traveltime))) +
  geom_line() + ggtitle("Total grade VS total alcohol consumption") + xlab("Total alcohol consumption")
ylab("Total grades")

fitted.loss = fitted.values(d1.loess)
residual.loss = residuals(d1.loess)
d1.loess.df = data.frame(fitted.loss, residual.loss)
ggplot(d1.loess.df, aes(x = fitted.loss, y = sqrt(abs(residual.loss)))) +
  geom_point() + geom_smooth(method.args = list(degree = 1)) +
  ggtitle("Spread location plot for residuals VS the fitted values") +
  xlab("fitted loss") + ylab("residual loss")

var(d1.loess.df$fitted.loss)/var(d1$totalgrade)
#6.84%

####model fitting for TRAVELTIME VS GOOUT
d1.loess = loess(totalgrade ~ traveltime * goout, data = d1, span = 1, family = "symmetric")

d1.grid = expand.grid(traveltime = (seq(1,4, by = 1)), goout = (seq(1,5, by = 1)))
d1.predict = predict(d1.loess, newdata = d1.grid)
d1.grid.df = data.frame(d1.grid, fit = as.vector(d1.predict))
ggplot(d1.grid.df, aes(x = goout, y = fit, group = traveltime, color = factor(traveltime))) +
  geom_line() + ggtitle("Total grades VS goout")  + ylab("Total grades")

fitted.loss = fitted.values(d1.loess)
residual.loss = residuals(d1.loess)
d1.loess.df = data.frame(fitted.loss, residual.loss)
ggplot(d1.loess.df, aes(x = fitted.loss, y = sqrt(abs(residual.loss)))) +
  geom_point() + geom_smooth(method.args = list(degree = 1)) + 
  ggtitle("Spread location plot for residuals VS the fitted values")  + xlab("fitted loss") + ylab("residual loss")

var(d1.loess.df$fitted.loss)/var(d1$totalgrade)
#4.6%

####contour plots
ggplot(d1.grid.df, aes(x = traveltime, y = goout, z = fit)) + geom_raster(aes(fill = fit)) +
coord_fixed() + scale_fill_distiller(palette = "RdYlBu") + geom_contour()

ggplot(d1, aes(x = failures, y = totalgrade)) + 
  geom_point() + 
  geom_smooth(span = 1, method.args = list(degree = 1, family = "symmetric"),se = FALSE) + 
  facet_wrap(~Talc + Medu,ncol=5,drop = FALSE)

loess.grid = expand.grid(traveltime=c(1,2,3,4), Medu = c(1,2,3,4), failures = c(0,1,2),Talc=c(2,3,4,5,6,7))

#crop.grid = (loess.grid$temperature < (-2 * loess.grid$wind + 112)) & (loess.grid$temperature >(-2 * loess.grid$wind + 87))
#environmental.df = environmental.df[crop.grid, ]
#+ theme(axis.text.x = element_text(angle=45, hjust = 1))

#1
grade.lo=loess(totalgrade~traveltime*failures*Talc,data=d1,span=1)
loess.grid = expand.grid(traveltime=c(1,2,3,4), failures = c(0,1,2),Talc=c(2,3,4,5,6,7))
grade.predict=predict(grade.lo, newdata = loess.grid)
grade.df = data.frame(loess.grid, fit = as.vector(grade.predict))
ggplot(grade.df, aes(x = Talc, y = fit)) + 
  geom_line(size=1) + 
  facet_wrap(~traveltime +failures, drop = FALSE,ncol=3,labeller = labeller(traveltime=c('1'="<15min",'2'="15-30min",'3'="30min-1hr",'4'=">1hr"),failures=label_both)) +
  labs(title = "Left to Right: Increasing Failures\nTop to Bottom: Increasing Traveltime")+
  xlab("Total Alcohol")+
  ylab("Totalgrade")+
  theme(plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#2
ggplot(grade.df, aes(x =traveltime , y = fit)) + 
  geom_line(size=1) + 
  facet_wrap(~Talc +failures, drop = FALSE,ncol=3,labeller = labeller(Talc=label_both,failures=label_both)) +
  labs(title = "Left to Right: Increasing Failure\nTop to Bottom: Increasing Alcohol consumption")+
  scale_x_discrete(name ="Traveltime", limits=c("<15min","15-30min","30min-1hr",">1hr"))+
  ylab("Totalgrade")+
  theme(plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#3
ggplot(grade.df, aes(x = failures, y = fit)) + 
  geom_line(size=1) + 
  facet_wrap(~traveltime+Talc, drop = FALSE,ncol=3,labeller = labeller(traveltime=c('1'="<15min",'2'="15-30min",'3'="30min-1hr",'4'=">1hr"),Talc=label_both)) +
  labs(title = "Left to Right: Increasing Traveltime\nTop to Bottom: Increasing Alcohol consumption")+
  scale_x_discrete(name ="Failures")+
  ylab("Totalgrade")+
  theme(plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#4
grade.lo=loess(totalgrade~traveltime*failures*Medu,data=d1,span=1)
loess.grid = expand.grid(traveltime=c(1,2,3,4), failures = c(0,1,2),Medu=c(1,2,3,4))
grade.predict=predict(grade.lo, newdata = loess.grid)
grade.df = data.frame(loess.grid, fit = as.vector(grade.predict))

ggplot(grade.df, aes(x = failures, y = fit)) + 
  geom_line(size=1) + 
  facet_wrap(~traveltime+Medu, drop = FALSE,ncol=4,labeller = labeller(traveltime=c('1'="<15min",'2'="15-30min",'3'="30min-1hr",'4'=">1hr"),Medu=c('1'="Primary",'2'="5th-9th grade",'3'="Secondary",'4'="Higher"))) +
  labs(title = "Left to Right: Increasing Education\nTop to Bottom: Increasing Traveltime")+
  scale_x_discrete(name ="Failures")+
  ylab("Totalgrade")+
  theme(plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#5
ggplot(grade.df, aes(x = traveltime, y = fit)) + 
  geom_line(size=1) + 
  facet_wrap(~failures+Medu, drop = FALSE,ncol=4,labeller = labeller(failures=label_both,Medu=c('1'="Primary",'2'="5th-9th grade",'3'="Secondary",'4'="Higher"))) +
  labs(title = "Left to Right: Increasing Education\nTop to Bottom: Increasing Failure")+
  scale_x_discrete(name ="Traveltime", limits=c("<15min","15-30min","30min-1hr",">1hr"))+
  ylab("Totalgrade")+
  theme(plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#6
ggplot(grade.df, aes(x = Medu, y = fit)) + 
  geom_line(size=1) + 
  facet_wrap(~traveltime+failures, drop = FALSE,ncol=3,labeller = labeller(traveltime=c('1'="<15min",'2'="15-30min",'3'="30min-1hr",'4'=">1hr"),failures=label_both)) +
  labs(title = "Left to Right: Increasing Failure\nTop to Bottom: Increasing Traveltime")+
  scale_x_discrete(name ="Mother's Education",limits=c("Primary","5th-9th grade","Secondary","Higher"))+
  ylab("Totalgrade")+
  theme(plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#7
grade.lo=loess(totalgrade~traveltime*Medu*Talc,data=d1,span=1)
loess.grid = expand.grid(traveltime=c(1,2,3,4), Medu = c(1,2,3,4),Talc=c(2,3,4,5,6,7))
grade.predict=predict(grade.lo, newdata = loess.grid)
grade.df = data.frame(loess.grid, fit = as.vector(grade.predict))
ggplot(grade.df, aes(x = Talc, y = fit)) + 
  geom_line(size=1) + 
  facet_wrap(~traveltime +Medu, drop = FALSE,ncol=4,labeller = labeller(traveltime=c('1'="<15min",'2'="15-30min",'3'="30min-1hr",'4'=">1hr"),Medu=c('1'="Primary",'2'="5th-9th grade",'3'="Secondary",'4'="Higher"))) +
  labs(title = "Left to Right: Increasing Mothers Education\nTop to Bottom: Increasing Traveltime")+
  xlab("Total Alcohol")+
  ylab("Totalgrade")+
  theme(plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#8
ggplot(grade.df, aes(x =traveltime , y = fit)) + 
  geom_line(size=1) + 
  facet_wrap(~Talc +Medu, drop = FALSE,ncol=4,labeller = labeller(Talc=label_both,Medu=c('1'="Primary",'2'="5th-9th grade",'3'="Secondary",'4'="Higher"))) +
  labs(title = "Left to Right: Increasing Mothers Education\nTop to Bottom: Increasing Alcohol consumption")+
  scale_x_discrete(name ="Traveltime", limits=c("<15min","15-30min","30min-1hr",">1hr"))+
  ylab("Totalgrade")+
  theme(plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#9
ggplot(grade.df, aes(x = Medu, y = fit)) + 
  geom_line(size=1) + 
  facet_wrap(~traveltime+Talc, drop = FALSE,ncol=6,labeller = labeller(traveltime=c('1'="<15min",'2'="15-30min",'3'="30min-1hr",'4'=">1hr"),Talc=label_both)) +
  labs(title = "Left to Right: Increasing Traveltime\nTop to Bottom: Increasing Alcohol consumption")+
  scale_x_discrete(name ="Mother's Education",limits=c("Primary","5th-9th grade","Secondary","Higher"))+
  ylab("Totalgrade")+
  theme(axis.text.x = element_text(angle=45, hjust = 1),plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#Final graph
grade.lo=loess(totalgrade~traveltime*Medu*Talc,data=d1,span=1)
loess.grid = expand.grid(traveltime=c(1,2,3,4), Medu = c(1,2,3,4),Talc=c(2,3,4,5,6,7))
grade.predict=predict(grade.lo, newdata = loess.grid)
grade.df = data.frame(loess.grid, fit = as.vector(grade.predict))
ggplot(grade.df, aes(x =traveltime , y = fit, group = Talc, colour = Talc)) + 
  geom_line(size=1) + 
  facet_wrap(~Medu, drop = FALSE,ncol=4,labeller = labeller(Talc=label_both,Medu=c('1'="Primary",'2'="5th-9th grade",'3'="Secondary",'4'="Higher"))) +
  labs(title = "Left to Right: Increasing Mothers Education")+
  scale_x_discrete(name ="Traveltime", limits=c("<15min","15-30min","30min-1hr",">1hr"))+
  ylab("Totalgrade")+
  labs(color='Total Alcohol') +
  theme(axis.text.x = element_text(angle=45, hjust = 1),plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))

#Data distribuition among the predictors
ggplot(subset(d1,Talc<8 & Medu>0), aes(x =traveltime , y = totalgrade)) + 
  geom_point() + 
  facet_wrap(~Talc+Medu, drop = FALSE,ncol=4,labeller = labeller(Talc=label_both,Medu=c('1'="Primary",'2'="5th-9th grade",'3'="Secondary",'4'="Higher"))) +
  labs(title = "Left to Right: Increasing Mothers Education\nTop to Bottom: Increasing Alcohol consumption")+
  scale_x_discrete(name ="Traveltime")+
  ylab("Totalgrade")+
  theme(axis.text.x = element_text(angle=45, hjust = 1),plot.title = element_text(face="bold",hjust = 0.5),strip.text.x = element_text(size = 10,face="bold"),axis.title=element_text(size=14,face="bold"))