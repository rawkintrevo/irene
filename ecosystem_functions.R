# TODO: Add comment
# 
# Author: rawkintrevo
###############################################################################

irene.ecosystem.init  <- function(input,
                                  output,
                                  network.max.layer= 3,
                                  network.max.nodes.per.layer= 5,
                                  species.popSize= 11,
                                  species.pcrossover= 0.8,
                                  species.pmutation= 0.1,
                                  species.maxiter= 3){

  irene.Controls <- structure(NULL, class="irene.Control")
  
  irene.Controls$processed.input <- irene.preprocess(input)
  irene.Controls$validation.input <- irene.preprocess(input)
  
  irene.Controls$processed.output <- irene.preprocess(output)
  irene.Controls$validation.output <- irene.preprocess(output)
  
  
  irene.Controls$inputs <- ncol(processed.input)
  irene.Controls$network.max.layer= network.max.layer
  irene.Controls$max.nodes.per.layer= network.max.nodes.per.layer

  irene.Controls$library <- irene.library  ################ This is pulling from global envrionment TODO FIX

  irene.Controls$species.popSize= species.popSize
  irene.Controls$species.pcrossover= species.pcrossover 
  irene.Controls$species.pmutation= species.pmutation
  irene.Controls$species.elitism = base::max(1, round(irene.Controls$species.popSize*0.05)) 
  irene.Controls$species.monitor = gaMonitor
  irene.Controls$species.maxiter = species.maxiter
  irene.Controls$species.run = irene.Controls$species.maxiter
  irene.Controls$species.maxfitness = -Inf
  irene.Controls$species.keepBest = FALSE
  irene.Controls$species.parallel = FALSE
  
  irene.Controls$creature.popSize = 25
  irene.Controls$creature.pcrossover = 0.8
  irene.Controls$creature.pmutation = 0.1
  irene.Controls$creature.elitism = base::max(1, round(irene.Controls$creature.popSize*0.05))
  irene.Controls$creature.monitor = gaMonitor
  irene.Controls$creature.maxiter = 50
  irene.Controls$creature.run = irene.Controls$creature.maxiter
  irene.Controls$creature.maxfitness = -Inf
  irene.Controls$creature.keepBest = FALSE
  irene.Controls$creature.parallel = FALSE
  
  irene.Controls$max.template <- make.max.nnet.template(irene.Controls)
  irene.Controls$min.template <- make.min.nnet.template(irene.Controls)

  irene.Controls$irene.museum <- list()
  irene.Controls$irene.museum[[1]] <- "index"
  
  irene.Controls$irene.templates <- list()
  irene.Controls$irene.templates[[1]] <- min.template
  return(irene.Controls)
}


irene.ecosystem.cycle <- function(irene.Controls, return.type="top-level"){
	
	ga.out <- ga(type= "real-valued", fitness= irene.species.cycle,
					processed.input= processed.input, output= output, 
					irene.library= irene.library, irene.Controls= irene.Controls,
					max= max.template,
					min= min.template,
					popSize = irene.Controls$species.popSize, 
					pcrossover = irene.Controls$species.pcrossover, 
					pmutation = irene.Controls$species.pmutation, 
					elitism = irene.Controls$species.elitism, 
					monitor = irene.Controls$species.monitor,
					maxiter = irene.Controls$species.maxiter,
					run = irene.Controls$species.run,
					maxfitness = irene.Controls$species.maxfitness,
					keepBest = irene.Controls$species.keepBest,
					parallel = irene.Controls$species.parallel)
		if (return.type=="top-level") return(irene.Controls)
    if (return.type=="not-the-top") return(ga.out@fitnessValue)	
}

## set summary and plot methods for irene.Control class
