# TODO: Add comment
# 
# Author: rawkintrevo
###############################################################################


############ 
## USEAGE

########### Setup
library(MASS)
data(petrol)
setwd("irene")
source("functions.R")
source("creature_functions.R")
source("species_functions.R")




irene.library <- list()
irene.library[[1]] <- function(x) (1+exp(-x))^(-1) ##sigmoid
irene.library[[2]] <- function(x) atan(x)
irene.library[[3]] <- function(x) cos(x)

source("ecosystem_functions.R")
##############################################################

#template <- list(	rep("input",ncol(processed.input)),
#		c(1, 2,3,1),
#		c(2,2), c(1))

#nnet <- irene.init(template)
#template <- irene.add.node(template, 5, 2)
#nnet <- irene.init(template)
#param.matrix <- irene.create.params(nnet)

#plot(nnet)

#nn.sse <- irene.calc.network(param.matrix, nnet, processed.input, petrol[,6],  library, "sse" )
#glm.sse <- sum((glm(petrol[,6]~petrol[,2]+petrol[,3]+petrol[,4]+petrol[,5])$fitted.values - petrol[,6])^2)


#ga.opt <- ga(type= "real-valued", fitness= irene.calc.network,
#		nnet= nnet, processed.input= processed.input, processed.output= petrol[,6], library= library, output.type="-sse",
#		min= -as.vector(nnet[,]),max= as.vector(nnet[,]))

#plot(ga.opt)

#ga.opt.sse <- irene.calc.network(matrix(ga.opt@solution,ncol=ncol(nnet[,])) , nnet, processed.input, petrol[,6],  library, "sse" )
## Weights of the 'champion'.  Not stuck in a corner on any parameter


#ga.Controls <- irene.make.ga.controls()
#irene.cycle(nnet, processed.input, petrol[,6], library, ga.Controls)

##nnet.template= c(5,rep(1,10))

#irene.species.cycle(nnet.template, processed.input, output, irene.library, ga.Controls)


######### TODO package it all up
######### TODO profit

controls <- irene.ecosystem.init(petrol[,2:5],petrol[,6])
irene.ecosystem.cycle(controls)


#length(irene.museum)					# Look at the museum
#length(irene.templates)
#length(irene.museum[[1]])
#nmuseum <- as.numeric(irene.museum[[1]][2:length(irene.museum[[1]])] )  
#champ <- which(nmuseum==max(nmuseum))+1   # here's your current champ
#print(champ)
#plot(irene.init(irene.create.nnet.template.from.template(irene.templates[[champ]],irene.Controls)))

