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
library(sna)
all_code <- read.csv("D:/論文/上市櫃電子工業代碼.csv")
all_code <- all_code$Code
all_code <- c(all_code,"3291","4962","3411","3474","3061","8172","6416","5466")
dir.list = list.files("D:/論文/yahoo_news" , full.name = TRUE)
cc1 = worker()
codelist1 <- list()
for(i in 1:length(dir.list)){
  file1 = dir.list[i]
  s1 =readLines(file1,encoding="ASCII")
  s1 = toString(s1)
  s1 = gsub("[0-1][0][0-9]",replacement = "",s1)
  s1 = gsub("[1-2][0-2][0-4][0-9]",replacement = "",s1)
  s1 = gsub("[0-9][0-9][0][0]",replacement = "",s1)
  s1 = gsub("[億萬元年]",replacement = "",s1)
  segment <- cc1[s1]
  temp <- which(segment %in% all_code)
  codelist1[[i]] <- segment[temp]
}
codelist1 <- t(sapply(codelist1, '[', seq(max(lengths(codelist1)))))
write.csv(codelist1,file = "D:/論文/SNA/companycode.csv")
input <- read.csv("D:/論文/SNA/companycode.csv")
#input <- sapply(input, as.character)
input[is.na(input)] <- " "
#input <- as.data.frame(df)
write.csv(input,file = "D:/論文/SNA/companycode_input.csv")
lines=scan(file="D:/論文/SNA/companycode_input.csv",what="character",sep="\n",skip=1) # read the csv file (skipping the header), line-by-line as character string.
lines=gsub(","," ",lines) # replace commas with spaces
lines=gsub("[ ]+$","",gsub("[ ]+"," ",lines)) # remove trailing and multiple spaces.
adjlist=strsplit(lines," ") # splits the character strings into list with different vector for each line
col1=unlist(lapply(adjlist,function(x) rep(x[1],length(x)-1))) # establish first column of edgelist by replicating the 1st element (=ID number) by the length of the line minus 1 (itself)
col2=unlist(lapply(adjlist,"[",-1)) # the second line I actually don't fully understand this command, but it takes the rest of the ID numbers in the character string and transposes it to list vertically
el1=cbind(col1,col2) # creates the edgelist by combining column 1 and 2.
#graph_from_edgelist(el, directed = TRUE)
g1=graph.data.frame(el1,directed=F)
plot(g1)
plot(g1, vertex.label = NA, vertex.shape="sphere", vertex.size=3,edge.arrow.size=.2, edge.color="gray80",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label.color="black")
#plot(g, layout=layout_with_fr(g))
d <- degree(g1)
# outd <- degree(g1,cmode="outdegree")
# ind <- degree(g1,cmode="indegree")

vb <- betweenness(g1,directed = F,weights = NULL)
eb <- edge_betweenness(g1,directed = F,weights = NULL)

clo <- closeness(g1)

A1 <- as_adjacency_matrix(g1,type="both",names=TRUE,sparse=FALSE,attr=NULL)

d1 <- centr_degree(g1, mode = "all", loops = T, normalized = F)
vb1 <- centr_betw(g1, directed = F, nobigint = T, normalized = F)
clo1 <- centr_clo(g1, mode = "all", normalized = F)

write.csv(el1,file = "D:/論文/SNA/edgelist.csv")
write.csv(A1,file = "D:/論文/SNA/adjacency_matrix.csv")
write.csv(d,file = "D:/論文/SNA/alldegree_igraph.csv")
write.csv(vb,file = "D:/論文/SNA/vertexbetweenness_igraph.csv")
write.csv(eb,file = "D:/論文/SNA/edgebetweenness_igraph.csv")
write.csv(clo,file = "D:/論文/SNA/closeness_igraph.csv")

write.csv(d1,file = "D:/論文/SNA/alldegree_i.csv")
write.csv(vb1,file = "D:/論文/SNA/vertexbetweenness_i.csv")
write.csv(clo1,file = "D:/論文/SNA/closeness_i.csv")

