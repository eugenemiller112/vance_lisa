library(scholar)
library(networkDynamic)
library(ndtv)
require(igraph)
library(statnet)
library(intergraph)
library(visNetwork)
library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)

##procedures
##for each author in sloan list (source)
#1, filter sloan list to get the other collaborators
#2, filter articles to only keep collaboration articles
#3, manually check collaboration articles are relevant 
#4, clean author names to be consistent in the kept articles

##after doing the steps for all authors
#1, combine the articles left to one dataframe (main R script that calls the source)
#2, remove duplicates in dataframe (available)
#3, divide into two dataframe based on years (available)
#4, create plots

########################################################################################################################

##function to get Sloan list that removes the primary author
#sloan<-read.csv("Sloan_CIE_Author_List.xlsx - Last Names.csv",header=FALSE)
filter_SL<-function(lastname, author_list){
  filtered_author_list =  subset(author_list, V1!=lastname)
  return (filtered_author_list)
}
#sloan_filtered<-filter_SL(lastname = 'Ault',author_list = sloan)

########################################################################################################################
##function to extract rows whose authors contain Sloan list authors
filter_AL<-function(author_data, sloan_list){
  data_R <- data.frame(matrix(ncol = 5, nrow = 0))
  x <- c("Title", "Authors", "Source.Title","Publication.Year","Total.Citations")
  colnames(data_R) <- x
  for (i in 1:nrow(author_data)){
    for (j in 1:nrow(sloan_list)){
      s = str_detect(string=author_data[i,"Authors"],pattern=sloan_list[j,],)
      
      tryCatch(
      if (s==TRUE){
        data_R=rbind(data_R,author_data[i,])
        break
      },
      error = function(loop){
        print(error)
        print(author_data[i,"Authors"])
        print(sloan_list[j,])
        print(s)
        break
      }
      )
      
    }
  }
  data_R<-data_R%>%select(c(1:5))
  return (data_R)
}

#modify sheet number based on which author
#AultAP<-read_excel("Cleaned Data by Author.xlsx", sheet = 3)
#author_data_extract = filter_AL(author_data = AultAP)

########################################################################################################################
##function to clean all names


########################################################################################################################
filter_name<-function(author_data,primary_author_name){
  for (i in 1:nrow(author_data)){
    rv1<-primary_author_name
    for (j in 1:nrow(sloan_filtered)){
      s = str_detect(string=author_data[i,"Authors"],pattern=sloan_filtered[j,],)
      if (s==TRUE){
        
        rv1<-c(rv1,sloan_filtered[j,])
      }
    }
    comma_vec <- paste(rv1, collapse = ";")
    author_data[i,"Authors"]<-comma_vec[1]
  }
  return (author_data)
}

#filtered_name_df = filter_name(author_data = author_data_extract,primary_author_name = 'Ault')
########################################################################################################################

#a<-master_data%>%filter(!duplicated(master_data$Title))



