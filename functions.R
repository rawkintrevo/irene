#### Dependencies
library(network)
library(GA)

#### Functions

irene.init <- function(template){
	inputs <- length(template[[1]])
	nnet <- network.initialize( inputs )
	set.vertex.attribute(nnet, "layer", 0, v=1:inputs)
	
	simp.template <- unlist(lapply(template, length))
	for (layer in 2:length(simp.template)){
		add.vertices(nnet, simp.template[layer] )
		set.vertex.attribute(nnet, "layer", layer-1, 
				v=(sum( simp.template[1:(layer-1)])+1):sum(simp.template[1:layer]))
		
	}
	for (layer in 1:length(simp.template)){
		nnet[get.vertex.attribute(nnet,"layer") == layer-1,get.vertex.attribute(nnet,"layer") == layer] <- 1
		## ^ this creates standard nnets, koch and feed forwards could easily be added by changing this line
	}
	return(nnet)
}

irene.preprocess <- function(dataset){
  dataset <- as.data.frame(dataset)
	for (col in 1:ncol(dataset)){
	  v <- (dataset[,col]-min(dataset[,col]))
	  dataset[,col] <-  v/max(v)
	} 
	return(as.data.frame(dataset))
} 


