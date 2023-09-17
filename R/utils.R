library(dplyr)
bold = function(x) strong(x, .noWS = "outside")
ital = function(x) em(x, .noWS = "outside")
link = function(s, href = s) a(s, href = href, .noWS = "outside")

BUILTIN_PEDS = c(Choose = "", "Trio", "Siblings", "Sibship of 3", "Half sibs, maternal", "Half sibs, paternal",
                 "3/4-siblings", "Grandparent (female line)", "Grandparent (male line)",
                 "Great grandparent (female line)", "Great grandparent (male line)",
                 "1st cousins", "2nd cousins",
                 "Half 1st cousins", "Half 2nd cousins", 
                 "Double 1st cousins", "Quad half 1st cousins",
                 "Full sib mating", "Half sib stack")

errModal = function(...) {
  mess = paste(lapply(list(...), toString), collapse = "")
  showModal(modalDialog(mess))
}

loadBuiltin = function(choice) {
  switch(choice,
    "Trio" = nuclearPed(1),
    "Siblings" = nuclearPed(2),
    "Sibship of 3" = nuclearPed(3, sex = c(1,2,1)),
    "Half sibs, maternal" = halfSibPed(1, 1, type = "maternal"),
    "Half sibs, paternal" = halfSibPed(1, 1),
    "3/4-siblings" = addChildren(addChildren(nuclearPed(2), 3, mother = 5, 1), 4, mother = 5, nch = 1),
    "Grandparent (female line)" = linearPed(2, sex = 2),
    "Grandparent (male line)" = linearPed(2),
    "Great grandparent (female line)" = linearPed(3, sex = 2),
    "Great grandparent (male line)" = linearPed(3),
    "1st cousins"   = cousinPed(1),
    "2nd cousins"  = cousinPed(2),
    "Half 1st cousins"   = halfCousinPed(1),
    "Half 2nd cousins"  = halfCousinPed(2),
    "Double 1st cousins" = doubleFirstCousins(),
    "Quad half 1st cousins" = quadHalfFirstCousins(),
    "Full sib mating" = fullSibMating(1),
    "Half sib stack" = halfSibStack(2),
  )
}

loadPed = function(file) {
  if(is.null(file))
    return()
  
  if(!file.exists(file)) 
    stop("File not found")
  
  df = read.table(file, header = TRUE, sep = "\t", colClasses = "character",
                  check.names = FALSE)
  names(df) = nms = tolower(names(df))

  cls = c("id", "fid", "mid", "sex")
  if(!all(cls %in% nms))
    stop("Column not found: ", toString(setdiff(cls, nms)))
  
  as.ped(df[cls])
}

checkSimInput = function(ped, ids) {
  if(is.null(ped)) 
    return("No pedigree indicated")
  if(length(ids) == 0) 
    return("No pedigree members indicated")
  if(!all(unique(unlist(ids)) %in% labels(ped)))
    return(paste("Unknown ID label:", toString(setdiff(ids, labels(ped)))))
  "ok"
}

# Not used
generateRandomPed = function() {
  while(T) {
    fou = rpois(1, 3) + 1
    g = rpois(1, fou) + fou
    x = randomPed(g, fou, selfing = F)
    if(is.ped(x))  break
  }
  
  suppressWarnings(relabel(x, "asPlot"))
}

#Get alelos de leapdna:
indice.lugares.alelos <- data.frame(lugar = c("Argentina 22",
                                              "Africa 22 STRs",
                                              "Asia 22 STRs",
                                              "Colombia 22 STRs",
                                              "Colombia 15 STRs",
                                              "Brazil 15 STRs",
                                              "Europe 22 STRs"),
                                    
                                    urls = c('Data/Base_freq.csv',
                                             'Data/Africa.csv',
                                             'Data/Asia.csv',
                                             'Data/Colombia22.csv',
                                             'Data/Colombia15.csv',
                                             'Data/Brazil.csv',
                                             'Data/Europe.csv')
                                    ) %>% 
  arrange(lugar)


get.freq.alelos <- function(lugar, indice = indice.lugares.alelos){
  
  leap.url <- indice[indice$lugar == lugar, "urls"]
  
  #if(lugar == "Argentina"){

    prueba <- read.csv(file = leap.url)
    prueba.lista <- as.list(prueba)
    for(i in 2:length(prueba.lista)){
      names(prueba.lista[[i]]) <- prueba.lista[[1]]
    }
    prueba.lista$Allele <- NULL
    
    return(prueba.lista)
    
#  }else{
#    leap <- GET(leap.url)
#    df.leap <- fromJSON(rawToChar(leap$content))$loci
    
#    lista.alelos <- list()
#    for(i in unique(df.leap$name)){
#      df.alelo <- df.leap[df.leap$name == i, "alleles"][[1]]
#      vector.alelo <- df.alelo$frequency
#      names(vector.alelo) <- df.alelo$name
#      lista.alelos[[i]] <- vector.alelo
#    }
    
    return(lista.alelos)
    
  }
