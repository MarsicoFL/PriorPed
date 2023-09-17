#####CARGO E INSTALO PAQUETES (agrego stringr)
packages<-c("patchwork","shiny", "shinyjs", "glue","lubridate","dplyr", "pedprobr", 
            "pedtools", "pedmut", "forrel", "httr", "jsonlite", "poibin", "igraph", "shinycssloaders", "purrr", "stringr", "magrittr" , "shinyjs")
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
#####

#analysis.form-group { margin-top: 15px; margin-bottom: 3px;}
#nsims.form-group {margin-bottom: 0px;}


###spinner:
options(spinner.color = "#5661f4", spinner.type = 6, spinner.color.background = "#ffffff", spinner.size = 0.5)

VERSION = list(shinyapp = "1.3.0")

# User interface
ui = fluidPage(
  
  shinyjs::useShinyjs(),  # Set up shinyjs
  
  tags$head(
    tags$style(type = "text/css", "
      .body {font-size: small}
      .well {padding-top: 10px;}
      .selectize-dropdown {width: 250px !important;}
      .fa-check { font-size:xx-large; color:Lime}
      
  ")),
  
  # Application title
  h2(id = "title-h2", "PriodPed: Selecting the best candidate to incorporate"),
  tags$style(HTML("#title-h2 {background-color: gray; color: white; padding: 15px}")),
  
  p(tags$b("Purpose: "),
    "This Shiny application is designed to assist in decision-making during kinship testing. It enables the calculation of statistical power based on available information for performing identifications. Additionally, it can be utilised to analyse which potential new reference family member could enhance the results obtained in the identification process."),
  
  p(tags$b("More information: "),
    "More information could be found in the reference paper: ", tags$a(href="https://doi.org/10.1016/j.fsigen.2020.102376", "prioritising family members"),
    ". Details about the simulations and the various parameters can be found in the paper. Also, for Spanish speakers, chapter 5 of the doctoral thesis entitled: Diseño de Herramientas Matemático-Computacionales para la búsqueda de personas desaparecidas, deeply explain the motivations, methods and implementation with several examples. It could be found here: ",tags$a(href="https://github.com/MarsicoFL/TesisDoctoral/blob/main/Tesis_Marsico.pdf", "Doctoral Thesis")),
 
  p(tags$b("Tip: "),
    "If you want to load a custom pedigree, you can use ", tags$a(href="https://magnusdv.shinyapps.io/quickped/", "QuickPed"),
    " to create the required ped file. Also, allele frequency databases were obtained from", tags$a(href=" https://leapdna.org/explore/studies/", "Leapdna.")),
  
  p(tags$b("Contact: "),
    "Do not hesitate in contact the developer: Dr. Franco Marsico, franco.lmarsico@gmail, or though the ", tags$a(href="https://github.com/MarsicoFL/", "Github account"),
    ", for further information."),
  
  
  # Widgets --------------------------------------------------------------
  fluidRow(
    
    # Left sidebar
    sidebarPanel(width = 2, style = "min-width: 185px",
                 
                 h4("Pedigree 1"),
                 selectizeInput("builtin1", "Built-in pedigree", selected = "Siblings", choices = BUILTIN_PEDS, size = 10),
                 fileInput("loadped1", "Load ped file", buttonLabel = icon("folder-open"),
                           accept = c(".ped", ".txt",".fam"), width = "100%", placeholder = NULL),
                 selectizeInput("lista_paises1", "Select region for allele frequency", selected = "Argentina", choices = indice.lugares.alelos$lugar, size = 10),
                 #textInput("ids1", "Individuals", value = "", width = "100%"),
                 #CAMBIO POR LISTA DESPLEGABLE CON MULTIPLES OPCIONES
                 selectizeInput("ids1","Reference individual", choices="", selected="", multiple=T),
                 #PARA AGREGAR NUEVOS INDIVIDUOS A SIMULAR
                 actionButton("add", "Add Group of individuals"),
                 #CAMBIO POR LISTA DESPLEGABLE CON UNICA OPCION
                 selectizeInput("id_missing1", "Missing Person", choices="", selected=""),
                 textInput("label1", "Label", width = "100%"),
                 
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
              ),
              ##### CAMBIE EL NOMBRE DEL OUTPUT A POWERPLOT
              fluidRow(
                column(6, align = "center", plotOutput("powerplot", width = "100%")), # %>% withSpinner(hide.ui = FALSE)),
              ),
    ),
    
  ),
  
  # Bottom panel
  fluidRow(
    column(6, wellPanel(id = "bottomwell1",
                        h4("Options"),
                        fluidRow(
                          column(4, numericInput("nProfiles", "nProfiles:", value = 1, min = 1, max = 100)),
                          column(4, numericInput("lrSims", "lrSims:", value = 20, min = 1, max = 1000)),
                          column(4, style="margin-top: 25px", align = "center", downloadButton("download", "Download data", class="btn btn"))
                        )))),
)



# Server logic
server = function(input, output, session) {
  
  
  SEED = 1234
  
  ped1 = reactiveVal(NULL)
  

  fams_input <- reactiveValues(btn = 1)
  observeEvent(input$builtin1, {
    ped1(loadBuiltin(req(input$builtin1)))
    updateSelectizeInput(session, "ids1", selected = founders(ped1()), choices = ped1()$ID)
  })
  
  observeEvent(input$add, {
    fams_input$btn<-fams_input$btn+1
    insertUI(
      selector = "#add",
      where = "beforeBegin",
      ui = selectizeInput(paste0("ids", fams_input$btn), "Other individuals", choices = ped1()$ID, multiple=T)
    )
  })
  
  ids1=reactive(compact(list(input$ids1, input$ids2, input$ids3,input$ids4,input$id5)))
  
  observeEvent(input$builtin1, {
    ped1(loadBuiltin(req(input$builtin1)))
    updateSelectizeInput(session, "id_missing1", selected = toString(leaves(ped1())[1]), choices = ped1()$ID)
  })
  

  observeEvent(input$loadped1, {
    if (grepl("(txt|ped)$",input$loadped1)) {
      ped = tryCatch(loadPed(input$loadped1$datapath),
                     error = function(e) errModal(conditionMessage(e)),
                     warning = function(e) errModal(conditionMessage(e)))
      ped1(req(ped))
      updateSelectizeInput(session, "builtin1", selected = "")
      updateSelectizeInput(session, "ids1", choices=ped$ID, selected=founders(ped))
    } else if (grepl("fam$", input$loadped1)){
      ped=tryCatch(readFam(input$loadped1$datapath, useDVI = T)[[1]][['Reference pedigree']],
                   error = function(e) errModal(conditionMessage(e)),
                   warning = function(e) errModal(conditionMessage(e)))
      
      ped$ID<-str_remove(ped$ID, "\\s\\[.+\\]") 
      ped1(req(ped))
      updateSelectizeInput(session, "builtin1", selected = "")
      updateSelectInput(session, "ids1", choices=ped$ID, selected = typedMembers(ped))
      updateSelectInput(session, "id_missing1", choices=ped$ID)
    } 
  })
  
  
  
  map1 = reactive({
    dMap = loadMap("decode19", uniform = TRUE, sexAverage = !input$sexspec1)
    switch(input$map1, decode19 = dMap, onechrom = uniformMap(Mb = physRange(dMap), cM = mapLen(dMap)))
  })
  
  
  observe({ped1(); ids1(); input$nProfiles; input$lrSims; enable("simulate1")})
  
  output$icon1 = renderUI(icon(name = if(is.null(sim1())) "arrow-left" else "check"))
  

  lista.alelos <- reactive({
    get.freq.alelos(input$lista_paises1)
  })
  
  
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
  
  
  
  power_plot=reactive({powerPlot(sim1(), type = 3)})
  output$powerplot=renderPlot({
    req(sim1())
    req(input$simulate1)
    power_plot()
  })
  
  allParams1 = reactive(list(ped = ped1(), label = input$label1, builtin = input$builtin1, loadped = input$loadped1$name, ids = ids1(), seed = SEED))
  
  output$download = downloadHandler(
    filename=paste0(input$label1, "_powerPlot.png"),
    content= function(file){
      png(file)
      print(power_plot())
      dev.off()},
    contentType = "image/png"
  )
}


shinyApp(ui = ui, server = server)
