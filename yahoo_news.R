library(httr)
library(jsonlite)
library(rvest)
library(stringr)
library(dplyr)
library(magrittr)
library(xml2)
#library(tmcn)
company_code <- read.csv("D:/論文/上市櫃電子工業代碼.csv", header = T)
num = c(1:80)

for (i in 2:834){
  url <- "https://tw.stock.yahoo.com/q/h?s=code&pg="
  targeturl <- gsub("code",replacement = company_code$Code[i] ,url)
  target_url <- paste0(targeturl,num)
  for (j in 1:80){
    yahoo_html <- read_html(target_url[j], encoding = 'CP950')
    article_url <- html_nodes(yahoo_html, ".yui-text-left") %>% html_nodes("a") %>% html_attr("href")
    yahoo <- paste0("https://tw.stock.yahoo.com",article_url)
    if (length(yahoo)<=1){
      break
    }
    for (k in 1:10){
      tryCatch({
        article <- read_html(yahoo[k], encoding = 'CP950') %>% html_nodes("#aritcletable p") %>% html_text()
        #title <- read_html(yahoo[k]) %>% html_nodes("br+ .t1") %>% html_text()
        #title <- gsub("[[:punct:]]",replacement = "_",title)
        Sys.sleep(runif(1,2,4))
        a = "D:/論文/yahoo_news/"
        b = paste(company_code$Code[i],j,k,sep = "_")
        c = ".txt"
        d = paste0(a,b,c)
        write(article, file = d )
      },error = function(err){
        print(paste("MY_ERROR: ",err))
      })
    }
  }
}



for (i in 1:834){
  url <- "https://tw.stock.yahoo.com/q/h?s=code&pg="
  targeturl <- gsub("code",replacement = company_code$Code[i] ,url)
  target_url <- paste0(targeturl,num)
  for (j in 1:80){
    yahoo_html <- read_html(target_url[j], encoding = 'CP950')
    url1 <- html_nodes(yahoo_html, ".yui-text-left") %>% html_nodes("a") %>% html_attr("href")
    Sys.sleep(runif(1,2,4))
    yahoo <- paste0("https://tw.stock.yahoo.com",url1)
    if (length(yahoo)<=1){
      break
    }
    for (k in 1:10){
      if (k>(length(yahoo))){
        break
      }
      else{
        article <- read_html(yahoo[k], encoding = 'CP950') %>% html_nodes("#aritcletable p") %>% html_text()
        Sys.sleep(runif(1,2,4))
        a = "D:/論文/yahoo_news/"
        b = paste(company_code$Code[i],j,k,sep = "_")
        c = ".txt"
        d = paste0(a,b,c)
        write(article, file = d )
      }  
    }
  }
}