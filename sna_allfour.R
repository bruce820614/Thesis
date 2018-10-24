library(Hmisc)
library(quanteda)
library(tm)
library(NLP)
library(openNLP)
library(jiebaR)
library(reshape)
library(igraph)
library(statnet)
library(dils)
dir.list = list.files("D:/論文/yahoo_news" , full.name = TRUE)
cc = worker()
codelist <- list()
for(i in 1:length(dir.list)){
  file0 = dir.list[i]
  s =readLines(file0,encoding="ASCII")
  s = toString(s)
  s = gsub("[0-1][0][0-9]",replacement = "",s)
  s = gsub("[1-2][0-2][0-4][0-9]",replacement = "",s)
  s = gsub("[0-9][0-9][0][0]",replacement = "",s)
  s = gsub("[億萬元年]",replacement = "",s)
  segment <- cc[s]
  code <- grep("^([1-9][0-9][0-9][0-9])$",segment, value = T)
  codelist[[i]] <- code
}
codelist <- t(sapply(codelist, '[', seq(max(lengths(codelist)))))
write.csv(codelist,file = "D:/論文/SNA/companycode.csv")
lines=scan(file="D:/論文/SNA/companycode.csv",what="character",sep="\n",skip=1) # read the csv file (skipping the header), line-by-line as character string.
lines=gsub(","," ",lines) # replace commas with spaces
lines=gsub("[ ]+$","",gsub("[ ]+"," ",lines)) # remove trailing and multiple spaces.
adjlist=strsplit(lines," ") # splits the character strings into list with different vector for each line
col1=unlist(lapply(adjlist,function(x) rep(x[1],length(x)-1))) # establish first column of edgelist by replicating the 1st element (=ID number) by the length of the line minus 1 (itself)
col2=unlist(lapply(adjlist,"[",-1)) # the second line I actually don't fully understand this command, but it takes the rest of the ID numbers in the character string and transposes it to list vertically
el=cbind(col1,col2) # creates the edgelist by combining column 1 and 2.
#graph_from_edgelist(el, directed = TRUE)
g=graph.data.frame(el,directed=T)
plot(g)
plot(g, vertex.label = NA, vertex.shape="sphere", vertex.size=3,edge.arrow.size=.2, edge.color="gray80",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label.color="black")
#plot(g, layout=layout_with_fr(g))
vb <- betweenness(g,directed = T,weights = NULL)
eb <- edge_betweenness(g,directed = T,weights = NULL)
A <- as_adjacency_matrix(g,type="both",names=TRUE,sparse=FALSE,attr=NULL)
write.csv(el,file = "D:/論文/SNA/edgelist.csv")
write.csv(A,file = "D:/論文/SNA/adjacency_matrix.csv")
write.csv(vb,file = "D:/論文/SNA/vertexbetweenness.csv")
write.csv(eb,file = "D:/論文/SNA/edgebetweenness.csv")