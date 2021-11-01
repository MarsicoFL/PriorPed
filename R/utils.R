library(dplyr)
bold = function(x) strong(x, .noWS = "outside")
ital = function(x) em(x, .noWS = "outside")
link = function(s, href = s) a(s, href = href, .noWS = "outside")

BUILTIN_PEDS = c(Choose = "", "Trio", "Siblings", "Sibship of 3", "Half sibs, maternal", "Half sibs, paternal",
                 "3/4-siblings", "3/4-siblings + child", "Grandparent (female line)", "Grandparent (male line)",
                 "Great grandparent (female line)", "Great grandparent (male line)",
                 "1st cousins", "1st cousins + child", "2nd cousins", "2nd cousins + child",
                 "Half 1st cousins", "Half 1st cousins + child", "Half 2nd cousins", "Half 2nd cousins + child",
                 "Double 1st cousins", "Double 1st cousins + child", "Quad half 1st cousins",
                 "Full sib mating", "Half sib stack", "Father-daughter incest", "Mother-son incest")

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
    "3/4-siblings + child" = addChildren(addChildren(addChildren(nuclearPed(2), 3, mother = 5, 1), 4, mother = 5, nch = 1, sex = 2), 6, 7, 1),
    "Grandparent (female line)" = linearPed(2, sex = 2),
    "Grandparent (male line)" = linearPed(2),
    "Great grandparent (female line)" = linearPed(3, sex = 2),
    "Great grandparent (male line)" = linearPed(3),
    "1st cousins"   = cousinPed(1),
    "1st cousins + child"   = cousinPed(1, child = TRUE),
    "2nd cousins"  = cousinPed(2),
    "2nd cousins + child"  = cousinPed(2, child = TRUE),
    "Half 1st cousins"   = halfCousinPed(1),
    "Half 1st cousins + child"   = halfCousinPed(1, child = TRUE),
    "Half 2nd cousins"  = halfCousinPed(2),
    "Half 2nd cousins + child"  = halfCousinPed(2, child = TRUE),
    "Double 1st cousins" = doubleFirstCousins(),
    "Double 1st cousins + child" = doubleCousins(1, 1, child = TRUE),
    "Quad half 1st cousins" = quadHalfFirstCousins(),
    "Full sib mating" = fullSibMating(1),
    "Half sib stack" = halfSibStack(2),
    "Father-daughter incest" = addChildren(nuclearPed(1, sex = 2), 1, 3, 1),
    "Mother-son incest" = addChildren(nuclearPed(1, sex = 1), 3, 2, 1),
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
indice.lugares.alelos <- data.frame(lugar = c("Argentina",
                                              "Asia",
                                              "Algeria (M’zab) – Mozabite",
                                              "Austria",
                                              "Belgium",
                                              "Bosnia and Herzegowina",
                                              "Brazil – Karitiana",
                                              "Brazil – Suruí",
                                              "Bugainville – NAN Melanesian",
                                              "Cambodia – Cambodian",
                                              "Central African Republic – Biaka Pygmies",
                                              "China – Dai",
                                              "China – Lahu",
                                              "China – Yizu",
                                              "China – Miaozu",
                                              "China – Han",
                                              "China – Tu", 
                                              "China – Oroqen",
                                              "China – Hezhen",
                                              "China – Uygur",
                                              "China – Daur",
                                              "China – She",
                                              "China – Mongola",
                                              "China – Xibo",
                                              "China – Naxi",
                                              "China – Tujia",
                                              "Colombia – Colombian",
                                              "Czech Republic",
                                              "Democratic Republic of the Congo – Mbuti Pygmies",
                                              "Denmark",
                                              "Dominican Republic",
                                              "Europe",
                                              "Finland",
                                              "France",
                                              "Germany",
                                              "Greece",
                                              "Hungary",
                                              "Ireland",
                                              "Israel (Negev) – Bedouin",
                                              "Israel (Central) – Palestinian",
                                              "Israel (Carmel) – Druze",
                                              "Japan – Japanese",
                                              "Kenya – Bantu (North East)",
                                              "Mexico – Maya",
                                              "Mexico – Pima",
                                              "Montenegro",
                                              "Namibia – San",
                                              "New Guinea – Papuan",
                                              "Nigeria – Yoruba",
                                              "Norway",
                                              "Pakistan – Burusho",
                                              "Pakistan – Sindhi",
                                              "Pakistan – Balochi",
                                              "Pakistan – Kalash",
                                              "Pakistan – Pathan",
                                              "Pakistan – Makrani",
                                              "Pakistan – Hazara",
                                              "Poland",
                                              "Saudi Arabia",
                                              "Senegal – Mandenka",
                                              "Siberia - Yakut",
                                              "Slovenia",
                                              "Somalia",
                                              "South Africa – Bantu",
                                              "Spain",
                                              "Spain (North-West)",
                                              "Sweden",
                                              "Switzerland",
                                              "Thailand",
                                              "U.S. All (length-based)",
                                              "U.S. All (sequence-based)",
                                              "U.S. African American (length-based)",
                                              "U.S. African American (sequence-based)",
                                              "U.S. Asian (sequence-based)",
                                              "U.S. Asian (length-based)",
                                              "U.S. Caucasian (sequence-based)",
                                              "U.S. Caucasian (length-based)",
                                              "U.S. Hispanic (length-based)",
                                              "U.S. Hispanic (sequence-based)"
                                              ),
                                    
                                    urls = c('Data/Base_freq.csv',
                                             "https://api.leapdna.org/studies/strider_asia_all.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_algeria.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_austria.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_belgium.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_bosina_and_herzegowina.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_brazil_karitiana.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_brazil_surui.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_bougainville.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_cambodia.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_central_african_republic.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_dai.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_lahu.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_yizu.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_miaozu.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_han.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_tu.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_oroqen.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_hezhen.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_uygur.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_daur.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_she.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_mongola.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_xibo.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_naxi.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_china_tujia.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_colombia.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_czech_republic.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_dpcongo_mbuti.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_denmark.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_dominican_republic.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_europe_all.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_finland.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_france.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_germany.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_greece.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_hungary.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_ireland.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_israel_bedouin.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_israel_palestinian.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_israel_druze.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_japan.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_kenya_bantu.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_mexico_maya.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_mexico_pima.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_montenegro.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_namibia_san.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_new_guinea_papuan.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_nigeria_yoruba.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_norway.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_pakistan_burusho.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_pakistan_sindhi.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_pakistan_balochi.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_pakistan_kalash.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_pakistan_pathan.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_pakistan_makrani.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_pakistan_hazara.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_poland.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_saudi_arabia.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_senegal_mandenka.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_siberia_yakut.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_slovenia.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_somalia.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_south_africa_bantu.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_spain.leapdna.json",
                                             "https://api.leapdna.org/studies/popstr_spain_nw.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_sweden.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_switzerland.leapdna.json",
                                             "https://api.leapdna.org/studies/strider_thailand.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_us.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_seq_us.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_us_african_american.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_seq_us_african_american.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_seq_us_asian.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_us_asian.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_seq_us_caucasian.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_us_caucasian.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_us_hispanic.leapdna.json",
                                             "https://api.leapdna.org/studies/nist1036_seq_us_hispanic.leapdna.json")
                                    ) %>% 
  arrange(lugar)


get.freq.alelos <- function(lugar, indice = indice.lugares.alelos){
  
  leap.url <- indice[indice$lugar == lugar, "urls"]
  
  if(lugar == "Argentina"){
    prueba <- read.csv(file = leap.url)
    prueba.lista <- as.list(prueba)
    for(i in 2:length(prueba.lista)){
      names(prueba.lista[[i]]) <- prueba.lista[[1]]
    }
    prueba.lista$Allele <- NULL
    
    return(prueba.lista)
  
  }else{
    leap <- GET(leap.url)
    df.leap <- fromJSON(rawToChar(leap$content))$loci
    
    lista.alelos <- list()
    for(i in unique(df.leap$name)){
      df.alelo <- df.leap[df.leap$name == i, "alleles"][[1]]
      vector.alelo <- df.alelo$frequency
      names(vector.alelo) <- df.alelo$name
      lista.alelos[[i]] <- vector.alelo
    }
    
    return(lista.alelos)
    
  }
  
  
}
