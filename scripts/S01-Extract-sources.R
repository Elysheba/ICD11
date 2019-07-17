library(RJSONIO)
library(here)
source("../00-Utils/downloadSourceFiles.R")

desc <- readJSONStream("../DESCRIPTION.json")

sourceFiles <- desc$"source files"
urls <- unlist(lapply(
   sourceFiles,
   function(sf){
      toRet <- sf$"URL template"
      names(toRet) <- sf$"name"
      return(toRet)
   }
))
srcDir <- "../sources"

downloadSourceFiles(urls, srcDir)
