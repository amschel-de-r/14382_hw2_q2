## loading the data
rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console
setwd("/Users/Tom/Google Drive (iamtomharris98@gmail.com)/14.382 PhD Metrics")

Penn<- as.data.frame(read.table("penn_jae.dat", header=T ));


n <- dim(Penn)[1]
p_1 <- dim(Penn)[2]
Penn<- subset(Penn, tg==4 | tg==0)
attach(Penn)
Penn$treatment <- ifelse(Penn$tg == 4, 1, 0)


#####


### FURTHER ANALYSIS BELOW checking for balance!!  and doing some matching !! ####
if (!require(MatchIt)) install.packages("MatchIt"); library(MatchIt)
if (!require(optmatch)) install.packages("optmatch"); library(optmatch)
if (!require(cobalt)) install.packages("cobalt"); library(cobalt)


m.out <- matchit(treatment~(female+black+othrace+factor(dep)+q2+q3+q4+q5+q6+agelt35+agegt54+
                              durable+lusd+husd),data= Penn, method = "full")  
summary(m.out)


## More tests incl. Kolmogorov Smirnov - test of equality of distributions
# i.e. do the randomised treatment and control groups come from the same original distribution?


love.plot(m.out, stats = "m", drop.distance = TRUE, which = "unadjusted", 
          var.order = "unadjusted",
          abs = FALSE,
          line = TRUE, 
          thresholds = c(m = .05),
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"),
          sample.names = c("Unweighted", "PS Weighted"),
          limits = c(-0.06, .06),
          position = c(.75, .25)) +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))



love.plot(m.out, stats = c("m", "ks"), drop.distance = TRUE, which = "unadjusted", 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          binary = "raw",
          stars = "raw",
          thresholds = c(m = .05, ks = 0.05),
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"),
          sample.names = c("Unweighted", "PS Weighted"),
          wrap = 20,
          position = "top")
          
      


X = model.matrix (~ (female+black+othrace+factor(dep)+q2+q3+q4+q5+q6+agelt35+agegt54+durable+lusd+husd)^2)[,-1]

m.out.flex <- matchit(treatment~(X),data= Penn, method = "full")  
summary(m.out.flex)


love.plot(m.out.flex, stats = "m", drop.distance = TRUE, which = "unadjusted", 
          var.order = "unadjusted",
          abs = FALSE,
          line = TRUE, 
          thresholds = c(m = .05),
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"),
          sample.names = c("Unweighted", "PS Weighted"),
          limits = c(-0.06, .06),
          position = c(.75, .25)) +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


### Now doing the same but for the wage gap data

m.out_2 <- matchit(sex~(lwage+shs+hsg+scl+clg+ad+ne+mw+so+we+exp1), data = data, method = "full")
                              

summary(m.out_2)
                        
                        
###
love.plot(m.out_2, stats = "m", drop.distance = TRUE, which = "unadjusted", 
          var.order = "unadjusted",
          abs = FALSE,
          line = TRUE, 
          thresholds = c(m = .05),
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"),
          sample.names = c("Unweighted", "PS Weighted"),
          limits = c(-0.15, .15),
          position = c(.75, .25)) +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


###

love.plot(m.out_2, stats = "m", drop.distance = TRUE, which = "unadjusted", 
          var.order = "unadjusted",
          abs = FALSE,
          line = TRUE, 
          thresholds = c(m = .05),
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"),
          sample.names = c("Unweighted", "PS Weighted"),
          limits = c(-0.1, .1),
          position = c(.75, .25)) +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


