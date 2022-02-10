library(rvest)
library(dplyr)
library(xtable)
library(stringr)
library(edgebundleR)
library(igraph)
library(visNetwork)

##referenced from https://rpubs.com/brandonkopp/creating-a-network-plot-in-R

master_data_unique<-read.csv("master_cleaned_unique.csv")

preProg.data = master_data_unique[master_data_unique$Publication.Year < 2015, ]
postProg.data = master_data_unique[master_data_unique$Publication.Year >= 2015, ]
authors<-preProg.data
authors<-postProg.data

#change n according to the article with the largest number of coauthors
#my guess is 15, seems to be an overestimate but it's fine
sp <- str_split_fixed(authors$Authors,"\\;",n=15)
#replace empty cells with NA
sp[nchar(sp)==0] <- NA

#Trim sp matrix down to width of maximum number of authors
x <- matrix(ncol = 1,nrow = 15)
for(i in 1:15){
  x[i,1] <- sum(!is.na(sp[ ,i]))
}
minrow <- which.min(x[,1]) -1
sp <- sp[,1:minrow]

#This function loops through each row of sp and creates all of the unique combinations of x authors.
#Creates x!/2 rows in the new dataset for each row, where x equals the number of authors.
combo <- function(df) {
  x <- data.frame(matrix(ncol = 3, nrow = 1))
  names(x) <- c("Var1","Var2","V3")
  
  for(i in 1:nrow(df)){
    if(sum(!is.na(df[i, ])) > 1){
      temp <- expand.grid(df[i, ], df[i,],stringsAsFactors = FALSE)
      temp <- temp[complete.cases(temp), ]
      temp <- temp[temp$Var1 != temp$Var2, ]
      temp<-as.data.frame(t(apply(temp, 1, sort)))
      for (j in 1:nrow(temp))
      {
        temp[j, ] = sort(temp[j, ])
      }
      temp <- unique(temp[duplicated(temp),])
      temp[,3] <- i
      x <- rbind(x,temp)
    }
  }
  x <- x[2:nrow(x), ]
  names(x) <- c("a1","a2","id")
  return(x)
}
pairs <- combo(sp)
nodes <- t(cbind(t(pairs[,1]), t(pairs[,2])))
nodes <- data.frame(nodes) %>% group_by(nodes) %>%
  summarise(count=n())


author_data<-read.csv("Sloan_CIE_Author_List.xlsx - Last Names.csv",header = FALSE)
add_missing_authors<-function(df){
  for (i in 1:nrow(author_data)){
    if (author_data[i,"V1"] %in% unlist(df)==FALSE){
      print(author_data[i,"V1"])
      temp<-c(author_data[i,"V1"],0)
      df<-rbind(df,temp)
    }
  }
  return (df)
}
nodes<-add_missing_authors(nodes)

# Create the graph object
g <- graph.data.frame(pairs[,1:2], directed=F, vertices=nodes)

fig<-edgebundle( g )
fig

visSave(fig, "coauthor_network_after2015.html", selfcontained = T)
################################################################################################
##produce matrix
coauthors_1 = sapply(as.character(master_data_unique$Authors), strsplit, ";")
coauthors_2 = unlist(coauthors_1)
coauthors_3 = lapply(coauthors_2, trimws)
coauthors.unique = unique(unlist(coauthors_3))[order(unique(unlist(coauthors_3)))]

bipartite.edges = lapply(coauthors_1, function(x) {coauthors.unique %in% x})
bipartite.edges = do.call("cbind", bipartite.edges) # dimension is number of authors x number of papers
rownames(bipartite.edges) =coauthors.unique

mat = bipartite.edges %*% t(bipartite.edges) #bipartite to unimode
mat = mat[order(rownames(mat)), order(rownames(mat))]
#write.csv(mat,'matrix.csv')

