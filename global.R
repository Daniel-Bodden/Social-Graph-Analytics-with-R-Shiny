##################################################################
# GLOBAL MODULE OF APP 
# App for Project ALSSN 
# @Author Daniel Bodden
##################################################################

graphFilename = list.files( path = "data/", include.dirs = FALSE ) 


#dummy variable to create a graph
g <- make_ring(10) %>% set_vertex_attr("name", value = LETTERS[1:10])





