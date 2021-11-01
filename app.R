suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(glue)
  library(ibdsim2)
  library(ribd)
  library(lubridate)
  library(dplyr)
  library(pedprobr)
  library(pedtools)
  library(pedmut)
  library(forrel)
  library(httr)
  library(jsonlite)
  library(poibin)
  library(igraph)
  library(shinycssloaders)
  library(purrr)
})


#analysis.form-group { margin-top: 15px; margin-bottom: 3px;}
#nsims.form-group {margin-bottom: 0px;}


###spinner:
options(spinner.color = "#5661f4", spinner.type = 6, spinner.color.background = "#ffffff", spinner.size = 0.5)

VERSION = list(shinyapp = "1.3.0", 
               ibdsim2 = packageVersion("ibdsim2"))

.MODELS = c(Haldane = "haldane", chi2 = "chi")
.MAPS = c("Decode (1-22)" = "decode19", "Single (26M/42M)" = "onechrom")

# User interface
ui = fluidPage(
  
  useShinyjs(),  # Set up shinyjs
  
  tags$head(
    tags$style(type = "text/css", "
      .body {font-size: small}
      .well {padding-top: 10px;}
      .selectize-dropdown {width: 250px !important;}
      .fa-check { font-size:xx-large; color:Lime}
      
  ")),
  
  # Application title
  h2(id = "title-h2", "IBD sharing by family members"),
  tags$style(HTML("#title-h2 {background-color: gray; color: white; padding: 15px}")),
  
  p(bold("Purpose: "),
    "Estimate and visualise distributions of genomic segments shared identical-by-descent (IBD) between related individuals, or within inbred individuals (autozygosity). This is done by simulating the recombination process through the pedigree."),
  
  p(bold("More information: "),
    "This program is a frontend for the R package ", link("ibdsim2", "https://github.com/magnusdv/ibdsim2"), 
    ", which is part of the ", link("ped suite", "https://magnusdv.github.io/pedsuite"), " ecosystem for pedigree analysis.", 
    "Details about the simulations and the various parameters can be found in the documentation of ibdsim2 (and also in the book ",
    link("Pedigree analysis in R", "https://www.elsevier.com/books/pedigree-analysis-in-r/vigeland/978-0-12-824430-2"), ")."), 
    
  p(bold("Tip: "),
    "If you want to load a custom pedigree, you can use ", link("QuickPed", "https://magnusdv.shinyapps.io/ibdsim2-shiny/"), 
    " to create the required ped file."),

# Widgets --------------------------------------------------------------
fluidRow(
  
  # Left sidebar
  sidebarPanel(width = 2, style = "min-width: 185px",
               
    h4("Pedigree 1"),
    selectizeInput("builtin1", "Built-in pedigree", selected = "Siblings", choices = BUILTIN_PEDS, size = 10),
    fileInput("loadped1", "Load ped file", buttonLabel = icon("folder-open"),
              accept = c(".ped", ".txt"), width = "100%", placeholder = NULL),
    selectizeInput("lista_paises1", "Select region for allele frequency", selected = "Argentina", choices = indice.lugares.alelos$lugar, size = 10),
    textInput("ids1", "Individuals", value = "", width = "100%"),
    textInput("id_missing1", "Missing individual", value = "", width = "100%"),
    textInput("label1", "Label", value = "Ped 1", width = "100%"),
  
    # Simulate!
    br(),
    fluidRow(
      column(8, style = "font-size: larger;", actionButton("simulate1", "Simulate!", width = "100%", class = "btn btn-primary")),
      column(4, style = "padding-left:10px;", uiOutput("icon1"))
    ),
  ),
  
  # Middle region: Plots
  mainPanel(width = 8, 
    fluidRow(
      column(6, align = "center", plotOutput("pedplot1", width = "300px", height = "300px")),
      #column(6, align = "center", plotOutput("pedplot2", width = "300px", height = "300px"))
    ),
    fluidRow(
      column(6, align = "center", plotOutput("ibdplot1", width = "100%") %>% withSpinner(hide.ui = FALSE)),
      #column(6, align = "center", plotOutput("ibdplot2", width = "100%") %>% withSpinner(hide.ui = FALSE))
    )
    ,
  ),
  
  # Right sidebar
  #sidebarPanel(width = 2, style = "min-width: 185px",
  #  h4("Pedigree 2"),
  #  selectizeInput("builtin2", "Built-in pedigree", selected = "", choices = BUILTIN_PEDS, size = 10),
  #  fileInput("loadped2", "Load ped file", buttonLabel = icon("folder-open"),
  #            accept = c(".ped", ".txt"), width = "100%", placeholder = NULL),
  #  selectizeInput("lista_paises2", "Select region for allele frequency", selected = "Argentina", choices = indice.lugares.alelos$lugar, size = 10),
  #  textInput("ids2", "Individuals", value = "", width = "100%"),
  #  textInput("id_missing2", "Missing individual", value = "", width = "100%"),
  #  textInput("label2", "Label", value = "Ped 2", width = "100%"),
    
    # Simulate!
  #  br(),
  ##  fluidRow(
  #    column(8, style = "font-size: larger;", actionButton("simulate2", "Simulate!", width = "100%", class = "btn btn-primary")),
  #    column(4, style = "padding-left:10px;", uiOutput("icon2"))
  #  ),
  #),
  
),
  
# Bottom panel
fluidRow(
  column(6, wellPanel(id = "bottomwell1",
    h4("Options"),
    fluidRow(
      column(4, numericInput("nProfiles", "nProfiles:", value = 10, min = 1, max = 100)),
      column(4, numericInput("lrSims", "lrSims:", value = 100, min = 1, max = 1000)),
      column(4, style="margin-top: 25px", align = "center", downloadButton("download", "Download data", class="btn btn"))
  )))),
  #column(6, wellPanel(id = "bottomwell2",
  #  fluidRow(
  #    column(8, h4("Observed data"),
  #      fluidRow(
  #        column(6, numericInput("obs-total", "Total length", value = "")),
  #        column(6, numericInput("obs-nseg", "Count", value = "")),
  #    )),
  #    column(4,textAreaInput("obs-segs", "Segments", value = "", rows = 2)),
  #  ))),
  #),
  #tags$style(HTML("#bottomwell1, #bottomwell2 {padding-bottom: 1px}")),
)

#p(style = "font-size:small", "This is version", VERSION$shinyapp, "of ibdsim2-shiny (",
#link("changelog", "https://github.com/magnusdv/ibdsim2-shiny/blob/master/NEWS.md"), ").",
#"Bug reports, feature requests and other comments are most welcome, for instance by filing an issue ", 
#link("here", "https://github.com/magnusdv/ibdsim2-shiny/issues"), "."),
#)


# Server logic
server = function(input, output, session) {

  SEED = 1234
  
  ped1 = reactiveVal(NULL)
  ped2 = reactiveVal(NULL)
  
  ids1 = reactive(as.list(strsplit(input$ids1, '-')[[1]]) %>%
                    map(., function(x){
                      setdiff(trimws(strsplit(x, ",")[[1]]), "")
                    })
                  )
  #ids2 = reactive(as.list(strsplit(input$ids2, '-')[[1]]) %>%
  #                  map(., function(x){
  #                    setdiff(trimws(strsplit(x, ",")[[1]]), "")
  #                  })
  #                )
  
  observeEvent(input$builtin1, {
    ped1(loadBuiltin(req(input$builtin1)))
    updateTextInput(session, "ids1", value = toString(founders(ped1())))
  })
  
  observeEvent(input$builtin1, {
    ped1(loadBuiltin(req(input$builtin1)))
    updateTextInput(session, "id_missing1", value = toString(leaves(ped1())[1]))
  })
  
#  observeEvent(input$builtin2, {
#    ped2(loadBuiltin(req(input$builtin2)))
#    updateTextInput(session, "ids2", value = toString(founders(ped2())))
#  })
  
#  observeEvent(input$builtin2, {
#    ped2(loadBuiltin(req(input$builtin2)))
#    updateTextInput(session, "id_missing2", value = toString(leaves(ped2())[1]))
#  })
  
  observeEvent(input$loadped1, {
    ped = tryCatch(loadPed(input$loadped1$datapath),
                   error = function(e) errModal(conditionMessage(e)),
                   warning = function(e) errModal(conditionMessage(e)))
    ped1(req(ped))
    updateSelectizeInput(session, "builtin1", selected = "")
    updateTextInput(session, "ids1", value = "")
  })
  
  #observeEvent(input$loadped2, {
  #  ped = tryCatch(loadPed(input$loadped2$datapath),
  #                 error = function(e) errModal(conditionMessage(e)),
  #                 warning = function(e) errModal(conditionMessage(e)))
  #  ped2(req(ped))
  #  updateSelectizeInput(session, "builtin2", selected = "")
  #  updateTextInput(session, "ids2", value = "")
  #})
  
  #sim1 = reactiveVal(NULL)
  #sim2 = reactiveVal(NULL)
      
  map1 = reactive({
    dMap = loadMap("decode19", uniform = TRUE, sexAverage = !input$sexspec1)
    switch(input$map1, decode19 = dMap, onechrom = uniformMap(Mb = physRange(dMap), cM = mapLen(dMap)))
  })
  #map2 = reactive({
  #  dMap = loadMap("decode19", uniform = TRUE, sexAverage = !input$sexspec2)
  #  switch(input$map2, decode19 = dMap, onechrom = uniformMap(Mb = physRange(dMap), cM = mapLen(dMap)))
  #})
  

# Simulations -------------------------------------------------------------

  # Reset if anything changes
  observe({ped1(); ids1(); input$nProfiles; input$lrSims; enable("simulate1")})
#  observe({ped2(); ids2(); input$nProfiles; input$lrSims; enable("simulate2")})
  
  # Icons
  output$icon1 = renderUI(icon(name = if(is.null(sim1())) "arrow-left" else "check"))
#  output$icon2 = renderUI(icon(name = if(is.null(sim2())) "arrow-left" else "check"))
  
  #Lista de alelos:
  
  lista.alelos <- reactive({
    get.freq.alelos(input$lista_paises1)
  })
  
  
  # Simulate!
  sim1 <- eventReactive(input$simulate1, {
    chk = checkSimInput(ped1(), ids1())
    if(chk != "ok")
      return(errModal(chk))
    disable("simulate1")
      
      MPPsims(setMarkers(ped1(), locusAttributes = lista.alelos()),
              missing = input$id_missing1,
              selections =  ids1(),
              addBaseline = FALSE,
              ep = TRUE,
              ip=TRUE,
              nProfiles = input$nProfiles,
              lrSims = input$lrSims,
              #numCores = 10, 
              seed = 1900)
  })
  
  # Simulate!
 # sim2 <- eventReactive(input$simulate2, {
#    chk = checkSimInput(ped2(), ids2())
#    if(chk != "ok")
#      return(errModal(chk))
#    disable("simulate1")
    
#    MPPsims(setMarkers(ped2(), locusAttributes = lista.alelos()),
##            missing = input$id_missing2,
#            selections =  ids2(),
#            addBaseline = FALSE,
#            ep = TRUE,
#            ip=TRUE,
#            nProfiles = input$nProfiles,
#            lrSims = input$lrSims,
            #numCores = 10, 
#            seed = 1900)
#  })
  

# Observed data -----------------------------------------------------------

  observedSegs = reactive({
    lenStr = input$`obs-segs` %>% strsplit("\n") %>% unlist() %>% strsplit(",") %>% unlist() %>% trimws()
    lenStr = lenStr[lenStr != ""]
    lens = suppressWarnings(as.numeric(lenStr))
    if(anyNA(lens))
      return(errModal(paste("Non-numeric segment length:", toString(lenStr[is.na(lens)]))))
    lens
  })
  
  observeEvent(input$`obs-segs`, {
    lens = observedSegs()
    if(!length(lens)) {
      enable("obs-nseg"); enable("obs-total")
      updateNumericInput(session, "obs-nseg", value = "")
      updateNumericInput(session, "obs-total", value = "")
    }
    else {
      updateNumericInput(session, "obs-nseg", value = length(lens))
      updateNumericInput(session, "obs-total", value = sum(lens))
      disable("obs-nseg"); disable("obs-total")
    }
  })
  
  observed = reactive({
    nseg = input$`obs-nseg`
    total = input$`obs-total`
    if(is.na(nseg) || is.na(total))
      return(NULL)
    list(nseg = nseg, total = total, mean = total/nseg, lengths = observedSegs())
  })
  
# Plots ----------------------------------------------------------

  COLS = c(3, 4)
  
  ploterrs = reactiveValues(err1 = NULL, err2 = NULL)
  
  output$pedplot1 = renderPlot({
    ped = req(ped1())
    lab = input$label1
    
    tryCatch(
      plotped(ped, ids = ids1(), id.missing = input$id_missing1, col = COLS[1], title = lab, pedname = input$builtin1),
      error = function(e) {
        plot.new(); box(which = "outer", col = 1); title(lab); 
        msg = if(grepl("reduce cex", conditionMessage(e))) "(Too big for plot region)" else conditionMessage(e)
        mtext(msg, line = 0, col = 2)
      })
  })

  #output$pedplot2 = renderPlot({
  #  ped = req(ped2())
  #  lab = input$label2
  #  
  #  tryCatch(
  #    plotped(ped, ids = ids2(), id.missing = input$id_missing2, col = COLS[2], title = lab, pedname = input$builtin2),
  #    error = function(e) {
  #      plot.new(); box(which = "outer", col = 1); title(lab); 
  #      msg = if(grepl("reduce cex", conditionMessage(e))) "(Too big for plot region)" else conditionMessage(e)
  #      mtext(msg, line = 0, col = 2)
  #    })
  #})
  
  output$ibdplot1 = renderPlot({
    req(sim1())
    req(input$simulate1)
    powerPlot(sim1(), type = 3)
  })
  
  #output$ibdplot2 = renderPlot({
  #  req(sim2())
  #  req(input$simulate2)
  #  powerPlot(sim2(), type = 3)
  #})
  

# Download data -----------------------------------------------------------

  allParams1 = reactive(list(ped = ped1(), label = input$label1, builtin = input$builtin1, loadped = input$loadped1$name, ids = ids1(), seed = SEED))
  
#  allParams2 = reactive(list(ped = ped2(), label = input$label2, builtin = input$builtin2, loadped = input$loadped2$name, ids = ids2(), seed = SEED))
  
  output$download = downloadHandler(
    filename = "ibdsim2-output.zip",
    content = function(con) {
      files = saveData(params1 = allParams1(), version = VERSION)
      if(!length(files)) return(errModal("No data to save"))
      zip(con, files, flags = "-jq9X") # j = junkpaths; q = quiet
    }, 
    contentType = "application/zip"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
