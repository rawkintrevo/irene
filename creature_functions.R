# Home of the functions that control creatures
# 
# Author: rawkintrevo
###############################################################################


irene.add.node <- function(template, layer, call.number){
	if (layer+1 > length(template)){
		template[[length(template)+1]] <- call.number
	}else template[[layer+1]] <- c(template[[layer+1]], call.number)
	return(template)
}


irene.create.params <- function(nnet){
	param.matrix <- matrix( 0,
			ncol=ncol(nnet[,]),
			nrow=nrow(nnet[,]) )
	param.matrix[nnet[,]==1] <- runif(sum(nnet[,]==1),  min= -1)
	return( param.matrix )		
}


irene.calc.network <- function(param.matrix, nnet, processed.input, processed.output, irene.library, output.type, template ){
	temp <- as.matrix(processed.input)
	if(is.null(dim(param.matrix)) ) param.matrix <- matrix(param.matrix, ncol=ncol(nnet[,]) )
	for (layer in 1:max(get.vertex.attribute(nnet,"layer")) ){
		temp <- temp %*% param.matrix[get.vertex.attribute(nnet,"layer") == layer-1,get.vertex.attribute(nnet,"layer") == layer]
		if (!is.null(ncol(temp))){
			for ( j in 1:ncol(temp) ) temp[,j] <- irene.library[[ template[[layer+1]][j] ]](temp[,j])
		}else temp <-  irene.library[[ template[[layer+1]][1] ]](temp)
	}
	output <- glm(processed.output~ temp)
	if(output.type=="sse") return( sum((output$fitted.values-processed.output)^2) )
	if(output.type=="-sse") return( -sum((output$fitted.values-processed.output)^2) )
	return(output)
	
}

