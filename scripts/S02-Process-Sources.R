rm(list = ls())
gc()

library(here)
library(XML)
library(parallel)
library(git2r)
library(RJSONIO)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(readxl)

##
mc.cores <- 55
sdir <- "./sources"
ddir <- "./data"
source(here("..","00-Utils/writeLastUpdate.R"))

###############################################################################@
## Source information ----
###############################################################################@

desc <- RJSONIO::readJSONStream("./DESCRIPTION.json")

sourceFiles <- desc$"source files"
sfi_name <- unlist(lapply(
  sourceFiles,
  function(sf){
    toRet <- sf$"name"
    return(toRet)
  }
))

sfi <- read.table(
  file.path(sdir, "ARCHIVES/ARCHIVES.txt"),
  sep="\t",
  header=T,
  stringsAsFactors=FALSE
)
ICD10_sourceFiles <- sfi[which(sfi$inUse), c("url", "current")]


###############################################################################@
## Data from mapping.xlsx ----
## ICD10
###############################################################################@
## Unzip mapping files
unzip(zipfile = here("sources/mapping.zip"), 
      exdir = here("sources"))
## Read table
icd10 <- read_xlsx("./sources/10To11MapToOneCategory.xlsx") %>%
  select("10ClassKind",
         "10DepthInKind",
         icd10Code,
         icd10Chapter, 
         icd10Title) %>%
  distinct()

## Empty definition to NA
nc <- nchar(icd10$icd10Title)
head(table(nc), n = 20)
icd10[which(nc < 5),]
## Check characters for \t, \n, \r and put to ASCII
icd10$icd10Title <- iconv(x = icd10$icd10Title,to="ASCII//TRANSLIT")
icd10$icd10Title <- gsub(paste("\n","\t","\r", sep = "|")," ", icd10$icd10Title)
## Change " to '
icd10$icd10Title <- gsub("\"","'",icd10$icd10Title)
icd10$icd10Title <- gsub("\\\\","",icd10$icd10Title)
table(unlist(sapply(icd10$icd10Title, strsplit, split = "")))

######################################
## entryId
ICD10_entryId <- icd10 %>%
  select(id = icd10Code,
         def = icd10Title) %>%
  mutate(DB = "ICD10") 

######################################
## idNames
ICD10_idNames <- icd10 %>%
  select(id = icd10Code,
         syn = icd10Title) %>%
  mutate(DB = "ICD10",
         canonical = TRUE)

######################################
## parentId
## block to category
# lblock <- do.call(rbind,
#                   lapply(icd10 %>% filter(`10ClassKind` == "block") %>% pull(icd10Code), 
#                          function(x){
#                            df <- tibble(icd10Code = x) %>%
#                              mutate(cat = stringr::str_sub(icd10Code, start = 1, end = 1),
#                                     cat.min = str_replace(icd10Code, "-.*", "") %>% 
#                                       str_replace("[[:alpha:]]", "") %>% 
#                                       as.numeric(),
#                                     cat.max = str_replace(icd10Code, ".*-", "") %>% 
#                                       str_replace("[[:alpha:]]", "") %>% 
#                                       as.numeric()) 
#                            a <- paste0(df$cat, str_pad(df$cat.min:df$cat.max,2, pad = "0"))
#                            return(data.frame(block = x,
#                                              category = a,
#                                              stringsAsFactors = F))
#                          }))
# ## Define parents for level 2 category
# lcat_lv2 <- icd10 %>% 
#   filter(`10ClassKind` %in% c("category", "modifiedcategory")) %>% 
#   filter(grepl("[.]", icd10Code)) %>%
#   mutate(parent = str_replace(icd10Code,"[.].*","")) %>%
#   distinct()
# 
# ICD10_parentId <- icd10 %>% filter(`10ClassKind` == "block") %>% select(parent = icd10Chapter,
#                                                                       id = icd10Code) %>%
#   bind_rows(lblock %>% select(parent = block,
#                               id = category)) %>%
#   bind_rows(lcat_lv2 %>% select(parent,
#                                 id = icd10Code)) %>%
#   mutate(DB = "ICD10",
#          pDB = "ICD10")
# table(ICD10_parentId$id %in% ICD10_entryId$id)
# table(ICD10_parentId$parent %in% ICD10_entryId$id)
# 
# ICD10_parentId[!ICD10_parentId$id %in% ICD10_entryId$id,]
# ## Remaining IDs are unknown in ICD10 so we remove
# ICD10_parentId <- ICD10_parentId %>%
#   filter(id %in% ICD10_entryId$id)

readChapters <- function(file, n=-1L){
  starts <- grep("chapter", file$`10ClassKind`)
  ends <- c(starts[2:length(starts)]-1, nrow(file))
  chList <- apply(
    cbind(starts,ends),
    1,
    function(x){ 
      # print(x)
      toParse <- file[x[1]:x[2],]
      starts <- grep("block", toParse$`10ClassKind`)
      ends <- c(starts[2:length(starts)]-1, nrow(toParse))
      blList <- do.call(rbind,
                        apply(
        cbind(starts,ends),
        1,
        function(y){
          # print(y)
          blParse <- toParse[y[1]:y[2],]
          lv1 <- blParse[2:nrow(blParse),] %>% filter(nchar(icd10Code) == 3) %>% pull(icd10Code)
          lv1 <- tibble(parent = blParse$icd10Code[1],
                        id = lv1)
          ##
          lv2 <- blParse %>%
            filter(grepl("[.]", icd10Code)) %>%
            mutate(parent = str_replace(icd10Code,"[.].*","")) %>%
            distinct() %>%
            select(parent,
                   id = icd10Code)
          toRet <- bind_rows(lv1,
                             lv2)
        })
      )
      ## block
      bl <- toParse %>% filter(`10ClassKind` == "block") %>% pull(icd10Code)
      lv0 <- tibble(parent = toParse$icd10Code[1],
                    id = bl)
      return(bind_rows(lv0,
                       blList))
    }
  )
  # attr(orList, "encoding") <- encoding
  return(do.call(rbind, chList))
}
## Add levels
getAncestors <- function(id){
  direct <- termParents[[id]]
  parents <- direct
  level <- 0
  dLev <- c()
  for(d in direct){
    dPar <- getAncestors(d)
    dLev <- c(dLev, dPar$level)
    parents <- c(parents, dPar$parents)
  }
  if(length(dLev)>0){
    level <- max(dLev)+1
  }
  return(list(parents=unique(parents), level=level))
}

###########################
## Get parent info
parentId <- readChapters(icd10)
parentId$origin <- "ICD10"

## level info
parentList <- group_split(parentId , id)
names(parentList) <- sapply(parentList, function(x) x$id)
parentList <- lapply(parentList, function(x) x$parent)

termParents <- parentList
library(BiocParallel)
bpparam <- MulticoreParam(workers = 30)

termAncestors <- bplapply(
  parentId$id,
  getAncestors,
  BPPARAM = bpparam
)
names(termAncestors) <- parentId$id

ICD10_parentId <- parentId %>%
  mutate(pDB = "ICD10",
         DB = "ICD10") %>%
  select(pDB, parent, DB, id, origin)

ICD10_entryId <- ICD10_entryId %>%
  mutate(
    level=unlist(lapply(termAncestors, function(x) x$level))[ICD10_entryId$id]
  ) %>%
  mutate(level = case_when(is.na(level) ~ 0,
                           TRUE ~ level)) %>%
  select(DB, id, def, level)

ICD10_idNames <- ICD10_idNames %>%
  select(DB, id, syn, canonical)

############################
toSave <- grep("^ICD10[_]", ls(), value=T)
for(f in toSave){
  message(paste("Saving", f))
  print(file.path(ddir, paste(f, ".txt", sep="")))
  ## Ensure unicity
  assign(f, get(f))
  if(length(names(f))==0){
    f <- unique(f)
  }
  ##
  write.table(
    get(f),
    file=file.path(ddir, paste(f, ".txt", sep="")),
    sep="\t",
    row.names=FALSE, col.names=TRUE,
    quote=TRUE,
    qmethod = "double"
  )
}
writeLastUpdate()

##############################################################
## Check model
source("../00-Utils/autoCheckModel.R")
