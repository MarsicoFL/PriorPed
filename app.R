
#####CARGO E INSTALO PAQUETES (agrego stringr)
packages<-c("patchwork","shiny", "shinyjs", "glue", "ibdsim2","ribd","lubridate","dplyr", "pedprobr", 
            "pedtools", "pedmut", "forrel", "httr", "jsonlite", "poibin", "igraph", "shinycssloaders", "purrr", "stringr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
#####

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
  h2(id = "title-h2", "PriodPed: Selecting the best candidate to incorporate"),
  tags$style(HTML("#title-h2 {background-color: gray; color: white; padding: 15px}")),
  
  p(bold("Purpose: "),
    "This shiny app aims to help in decision making during a kinship test case. It allows calculating which family member is the best option to incorpore into the reference pedigree."),
  
  p(bold("More information: "),
    "This program is a frontend for the R package ", link("mispitools", "https://github.com/MarsicoFL/mispitools"), 
    ". Details about the simulations and the various parameters can be found in the documentation of mispitools."), 
  
  p(bold("Tip: "),
    "If you want to load a custom pedigree, you can use ", link("QuickPed", "https://magnusdv.shinyapps.io/quickped/"), 
    " to create the required ped file."),
  
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
                 selectizeInput("ids1","Individuals", choices="", selected="", multiple=T),
                 #PARA AGREGAR NUEVOS INDIVIDUOS A SIMULAR
                 actionButton("add", "Add Group of individuals"),
                 #CAMBIO POR LISTA DESPLEGABLE CON UNICA OPCION
                 selectizeInput("id_missing1", "Missing individual", choices="", selected=""),
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
                column(6, align = "center", plotOutput("powerplot", width = "100%") %>% withSpinner(hide.ui = FALSE)),
              ),
    ),
    
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
  )



# Server logic
server = function(input, output, session) {
  

  SEED = 1234
  
  ped1 = reactiveVal(NULL)

  ###### CAMBIE LA SEPARACION DE LISTAS DE - A // (MENOS COMUN EN UN ID)    
  # ids1 = reactive(as.list(strsplit(input$ids1, '//')[[1]]) %>%
  #                   map(., function(x){
  #                     setdiff(trimws(strsplit(x, ",")[[1]]), "")
  #                   })
  # )
  
  #CONTADOR DE CUANTAS CONFORMACIONES FAMILIARES SE HACEN (PARA REFERENCIAR LOS INPUT ID)
  fams_input <- reactiveValues(btn = 1)
  #ACTUALIZACION DE FAMILIARES EN BASE A BUILTIN PEDIGREE ELEGIDO (AHORA CON SELECTIZEINPUT)
  observeEvent(input$builtin1, {
    ped1(loadBuiltin(req(input$builtin1)))
    updateSelectizeInput(session, "ids1", selected = founders(ped1()), choices = ped1()$ID)
  })
  
  #CADA VEZ Q SE AGREGA EL ADD (Other Individuals") SE GENERA UN NUEVO input$ids, IDENTIFICADO POR EL CONTADOR FAMS_INPUT QUE SE ACTUALIZA.
  observeEvent(input$add, {
    fams_input$btn<-fams_input$btn+1
    insertUI(
      selector = "#add",
      where = "beforeBegin",
      ui = selectizeInput(paste0("ids", fams_input$btn), "Other individuals", choices = ped1()$ID, multiple=T)
        )
    })
  
  #SE LISTAN TODOS LOS IDS (HASTA 5 POSIBLES) Y SE COMPACTA PARA ELIMINAR LOS NULL (NO USADOS)
  ids1=reactive(compact(list(input$ids1, input$ids2, input$ids3,input$ids4,input$id5)))
  
  observeEvent(input$builtin1, {
    ped1(loadBuiltin(req(input$builtin1)))
    updateSelectizeInput(session, "id_missing1", selected = toString(leaves(ped1())[1]), choices = ped1()$ID)
  })
  
  ##### SI EL FILENAME TERMINA EN TXT O PED, USA loadPed.  
  ##### SINO USA readFam, SE QUEDA CON LA REFERENCE FAMILY. Y A LOS ID SACALES EL CORCHETE QUE SE USA PARA LA CARGA AUTOMATICA DE EXPORTABLES A FAMILIAS  
  ##### problema: no se actualiza lo q pongo en fam_name (queda el valor que esta cuando se carga el archivo)
  
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
  
  
  # Simulations -------------------------------------------------------------
  
  # Reset if anything changes
  observe({ped1(); ids1(); input$nProfiles; input$lrSims; enable("simulate1")})

  # Icons
  output$icon1 = renderUI(icon(name = if(is.null(sim1())) "arrow-left" else "check"))

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
  
  
  
  ##### PASE LA FUNCION A REACTIVE PARA USARLA EN LA VISUALIZACION Y EN LA DESCARGA  
  power_plot=reactive({powerPlot(sim1(), type = 3)})
  output$powerplot=renderPlot({
    req(sim1())
    req(input$simulate1)
    power_plot()
  })
  
  
  
  
  # Download data -----------------------------------------------------------
  
  allParams1 = reactive(list(ped = ped1(), label = input$label1, builtin = input$builtin1, loadped = input$loadped1$name, ids = ids1(), seed = SEED))
  
  #  allParams2 = reactive(list(ped = ped2(), label = input$label2, builtin = input$builtin2, loadped = input$loadped2$name, ids = ids2(), seed = SEED))
  
  #   output$download = downloadHandler(
  #     filename = "ibdsim2-output.zip",
  #     content = function(con) {
  #       files = saveData(params1 = allParams1(), version = VERSION)
  #       if(!length(files)) return(errModal("No data to save"))
  #       zip(con, files, flags = "-jq9X") # j = junkpaths; q = quiet
  #     }, 
  #     contentType = "application/zip"
  #   )
  #   
  
  ##### DESCARGO EL POWER PLOT (USO EL LABEL COMO PREFIJO DEL FILENAME) 
  output$download = downloadHandler(
    filename=paste0(input$label1, "_powerPlot.png"),
    content= function(file){
      png(file)
      print(power_plot())
      dev.off()},
    contentType = "image/png"
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
