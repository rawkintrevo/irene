# network Class fields and methods
# 
# Author: rawkintrevo
###############################################################################

setClass("irene.network", representation(	
									structure = network,
									weight.matrix = matrix, 
									fitted.values= matrix,
									SSE= numeric,
									inputs= numeric))


setMethod("init", "irene.network", function(object){
			temp.net <- network.initialize(inputs) 
			set.vertex.attribute(temp.net, "layer", 0, v=1:inputs)
			add.vertices(temp.net, 1)
			set.vertex.attribute(temp.net, "layer", 1, v=inputs+1)
			
		})