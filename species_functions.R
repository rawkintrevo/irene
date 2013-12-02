# Functions relevent to species level
# TODO: Add comment
# 
# Author: Trevor 'Lucky Guess' Grant
###############################################################################



#################################################################################################################
##  These functions return the min and max vectors for the ga call that processes the species
##  and then for transforming the vectors ga seeks to optimize into templates for nnets

make.max.nnet.template <- function(irene.Controls){
	layers 	<- irene.Controls$network.max.layer+ .49		## Max Layers
	nodes.per.layer 	<- rep(irene.Controls$max.nodes.per.layer, irene.Controls$network.max.layer)+.49	# node slots
	books	<-	rep(length(irene.Controls$library)+.49, irene.Controls$max.nodes.per.layer * irene.Controls$network.max.layer)

	return (c(layers,nodes.per.layer, books) )
}

make.min.nnet.template <- function(irene.Controls){
	layers 	<- .51		## Max Layers
	nodes.per.layer 	<- rep(1, irene.Controls$network.max.layer)
	#^^ need at least 1 node, then ability to have 0 nodes in remaining slots
	books	<-	rep(.51, irene.Controls$max.nodes.per.layer * irene.Controls$network.max.layer)
	
	return (c(layers,nodes.per.layer, books) )
}

irene.create.template.from.nnet.template <- function(nnet.template, irene.Controls){
	nnet.template <- round(nnet.template)
	layers <- nnet.template[1]
	nodes.per.layer <- nnet.template[2:(irene.Controls$network.max.layer+1)]
	node.call.numbers <- nnet.template[(irene.Controls$network.max.layer+2):length(nnet.template)]
	
	template <- list()
	template[[1]] <- rep("input", irene.Controls$inputs)
	
	for (layer in 1:(layers) ){
		template[[layer+1]] <- node.call.numbers[ (sum(nodes.per.layer[0:(layer-1)])+1): sum(nodes.per.layer[0:layer])]
		
	} 
	return(template)
}

irene.create.nnet.template.from.template <- function(irene.template, irene.Controls){
	layers <- irene.template[1]
	nnet.template <- list()
	nnet.template[[1]] <- rep("input",irene.Controls$inputs)
	for (l in 1:layers){
		start.position <- 1+irene.Controls$network.max.layer+(l-1)*irene.Controls$max.nodes.per.layer+1
		end.position   <- start.position+irene.template[l+1]-1
		nnet.template[[l+1]] <- irene.template[start.position:end.position]
	}
	nnet.template[[layers+2]] <- "output"
	return(nnet.template)
}


irene.species.cycle <- function(nnet.template, processed.input, output, irene.library, irene.Controls, output.type="-sse"){
	template <- irene.create.template.from.nnet.template(nnet.template, irene.Controls)
	nnet <- irene.init(template)
	
	#### This little block checks for prior experience on this particular network structure
	#######################################################################################
  temp.matrix <- matrix(unlist(irene.templates), ncol=length(irene.templates[[1]]), byrow=TRUE)
	nnet.template <- round(nnet.template)
	prior_experience <- which(apply(temp.matrix, 1, function(x) all(x == nnet.template)))
	if (length(prior_experience)==0){
		suggest <- as.vector(nnet[,])
	}
	else if (length(prior_experience)>1){
		prior_experience <- prior_experience[1]
		suggest <- irene.museum[[prior_experience]]
		cat("\n...ummm something wierd happened when I was looking for prior experience...\n")
	} else suggest <- irene.museum[[prior_experience]]
	##################################################################################################################
  
	
	ga.opt <- ga(type= "real-valued", fitness= irene.calc.network,
			nnet= nnet, processed.input= processed.input, processed.output= output, 
			irene.library= irene.library, output.type="-sse", template= template,
			min= -as.vector(nnet[,]),max= as.vector(nnet[,]) ,
			popSize = irene.Controls$creature.popSize,
			pcrossover = irene.Controls$creature.pcrossover,
			pmutation = irene.Controls$creature.pmutation,
			elitism = irene.Controls$creature.elitism,
			monitor = irene.Controls$creature.monitor,
			maxiter = irene.Controls$creature.maxiter,
			run = irene.Controls$creature.run,
			maxfitness = irene.Controls$creature.maxfitness,
			suggestions = suggest, 
			keepBest = irene.Controls$creature.keepBest,
			parallel = irene.Controls$creature.parallel
	)
	
	ga.opt.sse <- irene.calc.network(matrix(ga.opt@solution,ncol=ncol(nnet[,])) , nnet, processed.input, output,  irene.library, "sse", template )
	#irene.cycle.return <- list(ga.opt= ga.opt, sse= ga.opt.sse)
	if (length(prior_experience)==0){ irene.museum.index <- length(irene.museum)+1   
	} else irene.museum.index <- prior_experience  #### We want to put the new params back in the original template slot, not create a new one
	
	irene.Controls$irene.museum[[1]][irene.museum.index] <<- -ga.opt.sse
	#### TODO: This is sloppy and should be fixed
	irene.Controls$irene.museum[[ irene.museum.index ]] <<- ga.opt@solution
	irene.Controls$irene.templates[[irene.museum.index]] <<- round(nnet.template)
 	return(-ga.opt.sse)
}
