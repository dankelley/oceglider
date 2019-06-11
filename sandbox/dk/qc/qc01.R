## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2

debug <- 1
version <- "0.1.1"

library(shiny)
library(shinythemes)
library(oce)
library(oceanglider)
##library(shinycssloaders)

options(oceEOS="gsw")

## Discover available gliders and their mission
basedir <- "/data/glider/delayedData/"
gliders <- list.files(basedir)
missions <- list()
for (glider in gliders) {
  missions[glider] <- list(list.files(paste(basedir, glider, "Data", sep="/")))
}

var <- list()
src <- ""
g <- NULL
SA <- NULL
CT <- NULL
p <- NULL
t <- NULL

ui <- fluidPage(theme=shinytheme("simplex"),
                sidebarPanel(width=3,
                             h5(paste0("gliderQC", "_", version)),
                             ##strong("1. Select glider & mission"),
                             uiOutput(outputId="glider"),
                             uiOutput(outputId="mission"),
                             ##OLD hr(),
                             uiOutput(outputId="read"),
                             uiOutput(outputId="listRda"),
                             uiOutput(outputId="loadRda"),
                             ##OLD hr(),
                             ##OLD tags$b("3. Save analysis"),
                             hr(),
                             conditionalPanel(condition="output.gliderExists",
                                              uiOutput(outputId="saveRda")),
                             hr(),
                             conditionalPanel(condition="output.gliderExists",
                                              uiOutput(outputId="plotChoice")),
                             conditionalPanel(condition="output.gliderExists",
                                              uiOutput(outputId="colorBy")),
                             ##uiOutput(outputId="flagAction"),
                             ##OLD if (!is.null(g))
                             ##OLD radioButtons(inputId="flagAction",
                             ##OLD              label="Flagged action (broken)",
                             ##OLD              choices=c("omit", "highlight"),
                             ##OLD              selected=c("omit"),
                             ##OLD              inline=TRUE),
                             uiOutput(outputId="deleteYo")),
                mainPanel(uiOutput(outputId="navState"),
                          uiOutput(outputId="status"),
                          plotOutput("plot",
                                                 hover="hover",
                                                 click="click",
                                                 width="100%",
                                                 height="650px",
                                                 brush=brushOpts(id="brush", resetOnNew=!TRUE))))


server <- function(input, output) {

  plotExists <- FALSE
  state <- reactiveValues(rda="", flag=NULL, yoSelected=NULL, gliderExists=FALSE, usr=NULL)

  relevantRdaFiles <- function(glider=NULL, mission=NULL)
  {
    Sys.glob(paste(tolower(glider), "_", tolower(mission), "*.rda", sep=""))
  }

  # flag=1 if ok; =3 if bad

  edits <- list()

  dataName <- function(basedir="/data/glider/delayedData") {
    if (nchar(input$glider) && nchar(input$mission)) {
      ## Start with what seems to be the most common directory structure.
      ## /data/glider/delayedData/SEA019/Data/M28/Payload/logs/
      glider <- input$glider
      mission <- input$mission
      if (tolower(glider) == "sea019") {
        if (tolower(mission) == "m28")
          return(paste(basedir, glider, "Data", mission, "Payload/logs", sep="/")) # no .pld1. files
        if (tolower(mission) == "m29" | tolower(mission) == "m30")
          return(paste(basedir, glider, "Data", mission, "Payload/logs/logs", sep="/"))
        if (tolower(mission) == "m31" | tolower(mission) == "m43")
          return(paste(basedir, glider, "Data", mission, mission, "Payload/logs", sep="/"))
        if (tolower(mission) == "m32")
          return(paste(basedir, glider, "Data", mission, mission, "Pld/logs", sep="/"))
        if (tolower(mission) == "m33")
          return(paste(basedir, glider, "Data", mission, mission, "pld/logs", sep="/"))
        if (tolower(mission) == "m34" | tolower(mission) == "m35")
          return(paste(basedir, glider, "Data", mission, mission, "PLD/logs", sep="/"))
        if (tolower(mission) == "m36")
          return(paste(basedir, glider, "Data", mission, mission, "Payload/logs/logs", sep="/"))
        if (tolower(mission) == "m49")
          return(paste(basedir, glider, "Data", mission, "PLD/logs/logs", sep="/"))
        if (tolower(mission) == "m54")
          return(paste(basedir, glider, "Data", mission, "PLD/logs/logs", sep="/"))
        if (tolower(mission) == "m58")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
        if (tolower(mission) == "m60")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
      }
      if (tolower(glider) == "sea021") {
        if (tolower(mission) == "m13")
          return(paste(basedir, glider, "Data", mission, "Payload/logs", sep="/"))
        if (tolower(mission) == "m14")
          return(paste(basedir, glider, "Data", mission, "Pld/logs", sep="/"))
        if (tolower(mission) == "m15")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
        if (tolower(mission) == "m16")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
        if (tolower(mission) == "m30")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
        if (tolower(mission) == "m31")
          return(paste(basedir, glider, "Data", mission, "PLD/logs", sep="/"))
        if (tolower(mission) == "m33")
          return(paste(basedir, glider, "Data", mission, "pld/logs/logs", sep="/"))
        if (tolower(mission) == "m30")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
      }
      if (tolower(glider) == "sea022") {
        if (tolower(mission) == "m12")
          return(paste(basedir, glider, "Data", mission, "Payload/logs", sep="/"))
        if (tolower(mission) == "m13")
          return(paste(basedir, glider, "Data", mission, "Payload/logs", sep="/"))
        if (tolower(mission) == "m14")
          return(paste(basedir, glider, "Data", mission, "Pld/logs", sep="/"))
        if (tolower(mission) == "m15")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
        if (tolower(mission) == "m16")
          return(paste(basedir, glider, "Data", mission, "Pld/logs", sep="/"))
        if (tolower(mission) == "m17")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
        if (tolower(mission) == "m18")
          return(paste(basedir, glider, "Data", mission, "PLD/logs", sep="/"))
        if (tolower(mission) == "m19")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
        if (tolower(mission) == "m32")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
      }
      if (tolower(glider) == "sea024") {
        if (tolower(mission) == "m11" | tolower(mission) == "m12" | tolower(mission) == "m13" |
            tolower(mission) == "m15" | tolower(mission) == "m16" | tolower(mission) == "m17")
          return(paste(basedir, glider, "Data", mission, "Payload/logs", sep="/"))
        if (tolower(mission) == "m14")
          return(paste(basedir, glider, "Data", mission, "payload/logs", sep="/"))
        if (tolower(mission) == "m18")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
        if (tolower(mission) == "M2017001")
          return(paste(basedir, glider, "Data", mission, "Payload/logs/logs", sep="/"))
      }
      if (tolower(glider) == "sea032") {
        if (tolower(mission) == "m22" | tolower(mission) == "m23")
          return(paste(basedir, glider, "Data", mission, "PLD/logs/logs", sep="/"))
        if (tolower(mission) == "m26" | tolower(mission) == "m30")
          return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
      }
      return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
    }
    NULL
  }

  varName <- function() {
    tolower(paste(input$glider, "_", input$mission, sep=""))
  }

  rdaName <- function(time=TRUE) { # timestamp does not give seconds, saving 3 chars in pulldown menu
    tolower(paste0(varName(), "_", format(Sys.time(),"%Y-%m-%d_%H:%M:%S"), ".rda", sep=""))
  }

  ##UNUSED dataStatus <- function() {
  ##UNUSED   cat(file=stderr(), "dataStatus...\n")
  ##UNUSED   cat(file=stderr(), "  names(var): ", paste(names(var), collapse=","), "\n")
  ##UNUSED   "read" # "read" "load" ""
  ##UNUSED }

  output$gliderExists <- reactive({
    cat(file=stderr(), "***gliderExists... (will return ", state$gliderExists, ")***\n")
    state$gliderExists
  })

  output$glider <- renderUI({
    state$gliderExists <- FALSE
    selectInput(inputId="glider",
                label="",
                choices=gliders,
                selected=gliders[1])
  })

  output$mission <- renderUI({
    state$gliderExists <- FALSE
    selectInput(inputId="mission",
                label="",
                choices=missions[input$glider],
                selected=missions[input$glider][1])
  })

  output$read <- renderUI({
    actionButton(inputId="readData", label="Read original pld1 data")
  })

  output$listRda <- renderUI({
    if (debug > 1)
      cat(file=stderr(), "output$listRda\n", sep="")
    ## files <- Sys.glob(paste0(varName(), "*.rda"))
    ## cat(file=stderr(), "  input$glider='", input$glider, "'\n", sep="")
    ## cat(file=stderr(), "  input$mission='", input$mission, "'\n", sep="")
    files <- relevantRdaFiles(input$glider, input$mission)
    ## cat(file=stderr(), "  files=", paste(files, collapse=","), "\n")
    if (length(files)) {
      filedates <- gsub("([a-z0-9]*)_([a-z0-9]*)_(.*).rda", "\\3", files)
      ##cat(file=stderr(), "  filedates='", paste(filedates, collapse="','"), "'\n", sep="")
      selectInput(inputId="rdaInputFile", label="...OR resume saved analysis", choices=filedates, selected=filedates[1])
    }
  })

  output$loadRda <- renderUI({
    if (debug > 1)
      cat(file=stderr(), "output$loadRda\n", sep="")
    files <- relevantRdaFiles(input$glider, input$mission)
    ## cat(file=stderr(), "  files=", paste(files, collapse=","), "\n")
    if (length(files)) {
      actionButton(inputId="loadRdaAction", label="Load previous analysis")#h5(paste0("Load ", rda)))
    }
  })

  output$saveRda <- renderUI({
    if (debug > 1)
      cat(file=stderr(), "output$saveRda\n", sep="")
    hr()
    actionButton(inputId="saveRda", label="Save")
  })

  output$plotChoice <- renderUI({
    if (debug > 1)
      cat(file=stderr(), "output$plotChoice\n", sep="")
    ##cat(file=stderr(), "  plotExists=", plotExists, "\n", sep="")
    selectInput(inputId="plotChoice",
                label="Plot type",
                ## FIXME: add yoNumber, time, badness
                choices=if (is.null(state$yoSelected)) c("none", "pt", "TS", "hist(S)", "hist(p)") else c("none", "pt", "TS", "hist(S)", "hist(p)", "yo"),
                selected=c("none"))
  })

  output$colorBy <- renderUI({
    selectInput(inputId="colorBy",
                label="Colour by",
                choices=c("latitude", "longitude", "pressure", "temperature", "salinity", "navState", "(none)"),
                selected="latitude")
  })

  ##OLD output$flagAction <- renderUI({
  ##OLD   if (!is.null(g))
  ##OLD     radioButtons(inputId="flagAction",
  ##OLD                  label="Flagged action (broken)",
  ##OLD                  choices=c("omit", "highlight"),
  ##OLD                  selected=c("omit"),
  ##OLD                  inline=TRUE)
  ##OLD  })

  output$deleteYo <- renderUI({
    if (!is.null(state$yoSelected)) {
      actionButton(inputId="deleteYo", label=paste("Delete yo #", state$yoSelected, sep=""))
    }
  })

  output$navState <- renderUI({
    ns <- navStateCodes("seaexplorer")
    checkboxGroupInput(inputId="navState", label="Show navState",
                       choices=ns, selected=ns, inline=TRUE)
  })

  ##UNUSED observeEvent(input$navState, {
  ##UNUSED              cat(file=stderr(), "input$navState observed...\n")
  ##UNUSED              cat(file=stderr(), "  navState=c(", paste(input$navState, collapse=","), ")\n", sep="")
  ##UNUSED })

  output$status <- renderText({
    x <- input$hover$x
    y <- input$hover$y
    res <- if (is.null(g)) "Status: no glider exists. Please read data or load previous analysis." else paste(input$glider, input$mission)
    res <- "-"
    distThreshold <- 0.05
    flagged <- state$flag == 3
    if (!is.null(x) && plotExists) {
      ## note scaling of the x and y, dependent on plot type. The scales are a rough guess.
      if (input$plotChoice == "pt") {
        dist <- sqrt(((x-t)/(state$usr[2]-state$usr[1]))^2 + ((y-p)/(state$usr[4]-state$usr[3]))^2)
        dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
        disti <- which.min(dist)
        d <- g[["payload1"]][disti,]
        ## FIXME: maybe hide if dist[disti] exceeds some number
        res <- sprintf("dist=%.4f %s yo=%d %s p=%.1f\n",
                       dist[disti],
                       if (dist[disti] < distThreshold) " (click to select) " else {
                         if (!is.null(state$yoSelected)) " (click to unselect)" else ""},
                       d$yoNumber, format(d$time, "%Y-%m-%dT%H:%M:%S"), d$pressure)
      } else if (input$plotChoice == "TS") {
        dist <- sqrt(((x-SA)/(state$usr[2]-state$usr[1]))^2 + ((y-CT)/(state$usr[4]-state$usr[3]))^2)
        dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
        disti <- which.min(dist)
        d <- g[["payload1"]][disti,]
        ## FIXME: maybe hide if dist[disti] exceeds some number
        res <- sprintf("dist=%.4f %s yo=%d %s p=%.1f S=%.4f T=%.4f\n",
                       dist[disti],
                       if (dist[disti] < distThreshold) " (click to select) " else {
                         if (!is.null(state$yoSelected)) " (click to unselect)" else ""},
                       d$yoNumber, format(d$time, "%Y-%m-%dT%H:%M:%S"), d$pressure, d$salinity, d$temperature)
      }
    }
    res
    ##paste0("x=", x, " y=", y, " '", input$plotChoice, "'")
  })

  observeEvent(input$resetPlot, {
               cat(file=stderr(), "resetPlot\n")
               cat(file=stderr(), "  state$Slim=", paste(state$Slim, collapse=","), "\n")
               cat(file=stderr(), "  state$Tlim=", paste(state$Tlim, collapse=","), "\n")
               cat(file=stderr(), "  state$plim=", paste(state$plim, collapse=","), "\n")
               if (!is.null(g)) {
                 state$Slim <- range(g[["SA"]], na.rm=TRUE)
                 state$Tlim <- range(g[["CT"]], na.rm=TRUE)
                 state$plim <- range(g[["pressure"]], na.rm=TRUE)
               }
               cat(file=stderr(), "  state$Slim=", paste(state$Slim, collapse=","), "\n")
               cat(file=stderr(), "  state$Tlim=", paste(state$Tlim, collapse=","), "\n")
               cat(file=stderr(), "  state$plim=", paste(state$plim, collapse=","), "\n")
  })

  observeEvent(input$deleteYo, {
               cat(file=stderr(), "  DELETE yo=", state$yoSelected, "\n")
               yo <- g[["yoNumber"]]
               bad <- yo == state$yoSelected
               oldFlag <- state$flag
               state$flag[bad] <- 3
               newFlag <- state$flag
               index <- which(newFlag != oldFlag)
               old <- oldFlag[index]
               new <- newFlag[index]
               edits[[1+length(edits)]] <<- list(category=paste("delete yo", state$yoSelected),
                                                 time=presentTime(),
                                                 index=index, oldFlag=old, newFlag=new)
               cat(file=stderr(), "  updated edits; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               state$yoSelected <<- NULL
  })

  observeEvent(input$click, {
               cat(file=stderr(), "click\n", sep="")
               distThreshold <- 0.05
               x <- input$click$x
               y <- input$click$y
               flagged <- state$flag == 3
               if (input$plotChoice == "pt") {
                 dist <- sqrt(((x-t)/(state$usr[2]-state$usr[1]))^2 + ((y-p)/(state$usr[4]-state$usr[3]))^2)
                 dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
                 disti <- which.min(dist)
                 d <- g[["payload1"]][disti,]
                 res <- sprintf("dist=%.4f x=%.4f y=%.4f (yo=%d, t=%s, p=%.1f)\n",
                                dist[disti], x, y, d$yoNumber, d$time, d$pressure)
                 state$yoSelected <- if (dist[disti] < distThreshold) d$yoNumber else NULL
                 cat(file=stderr(), res)
               } else if (input$plotChoice == "TS") {
                 dist <- sqrt(((x-SA)/(state$usr[2]-state$usr[1]))^2 + ((y-CT)/(state$usr[4]-state$usr[3]))^2)
                 dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
                 disti <- which.min(dist)
                 d <- g[["payload1"]][disti,]
                 res <- sprintf("dist=%.4f x=%.4f y=%.4f (yo=%d, t=%s, p=%.1f, S=%.4f, T=%.4f)\n",
                                dist[disti], x, y, d$yoNumber, d$time, d$pressure, d$salinity, d$temperature)
                 state$yoSelected <- if (dist[disti] < distThreshold) d$yoNumber else NULL
                 cat(file=stderr(), res)
               }
  })

  observeEvent(input$brush, {
               xmin <- input$brush$xmin
               xmax <- input$brush$xmax
               ymin <- input$brush$ymin
               ymax <- input$brush$ymax
               bad <- NULL
               if (debug > 0) {
                 cat(file=stderr(), "brush\n", sep="")
                 cat(file=stderr(), "  xmin=", xmin, ", xmax=", xmax, "\n", sep="")
                 cat(file=stderr(), "  ymin=", ymin, ", ymax=", ymax, "\n", sep="")
                 cat(file=stderr(), "  count of flag==3: ", sum(state$flag==3), " (before)\n", sep="")
               }
               state$yoSelected <- NULL # brushing turns off yo selection
               if (input$plotChoice == "pt") {
                 cat(file=stderr(), "  pt\n", sep="")
                 t <- g[["time"]]
                 tnumeric <- as.numeric(t)
                 p <- g[["pressure"]]
                 bad <- xmin <= tnumeric & tnumeric <= xmax & ymin <= p & p <= ymax
                 bad[is.na(bad)] <- TRUE
                 state$flag[bad] <- 3
                 edits[[1+length(edits)]] <<- list(category="brush pt", time=presentTime(), bad=bad)
                 cat(file=stderr(), "  updated edits for brushed pt; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               } else if (input$plotChoice == "TS") {
                 cat(file=stderr(), "  TS\n", sep="")
                 SA <- g[["SA"]] # FIXME: allow oceEOS=="unesco"
                 CT <- g[["CT"]]
                 bad <- xmin <= SA & SA <= xmax & ymin <= CT & CT <= ymax
                 bad[is.na(bad)] <- TRUE
                 cat(file=stderr(), " sum(bad)=", sum(bad), "\n")
                 ## bad[is.na(bad)] <- TRUE
                 edits[[1+length(edits)]] <<- list(category="brush TS", time=presentTime(), bad=bad)
                 cat(file=stderr(), "  updated edits for brushed TS; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               } else if (input$plotChoice == "hist(S)") {
                 cat(file=stderr(), "  hist(S)\n", sep="")
                 SA <- g[["SA"]] # FIXME: allow oceEOS=="unesco"
                 bad <- xmin <= SA & SA <= xmax
                 bad[is.na(bad)] <- TRUE
                 cat(file=stderr(), " sum(bad)=", sum(bad), "\n")
                 ## bad[is.na(bad)] <- TRUE
                 edits[[1+length(edits)]] <<- list(category="brush hist(S)", time=presentTime(), bad=bad)
                 cat(file=stderr(), "  updated edits for brushed hist(S; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               } else if (input$plotChoice == "hist(p)") {
                 cat(file=stderr(), "  hist(p)\n", sep="")
                 p <- g[["pressure"]]
                 bad <- xmin <= p & p <= xmax
                 bad[is.na(bad)] <- TRUE
                 cat(file=stderr(), " sum(bad)=", sum(bad), "\n")
                 #if (any(is.na(bad))) browser()
                 edits[[1+length(edits)]] <<- list(category="brush hist(p)", time=presentTime(), bad=bad)
                 cat(file=stderr(), "  updated edits for brushed hist(p); new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               }
               if (sum(bad)) {
                 bad <- which(bad)
                 cat(file=stderr(), "  total number of bad points: ", length(bad), "\n", sep="")
                 state$flag[bad] <- 3 # HERE HERE HERE
               }
  })

  observeEvent(input$readData, {
               cat(file=stderr(), "readData...\n")
               dir <- dataName()
               cat(file=stderr(), "  about to read '", dir, "' .", sep="")
               t <- try(read.glider.seaexplorer.delayed(dir))
               if (inherits(t, "try-error")) {
                 cat(file=stderr(), " no data FIXME: put up a dialog box\n")
                 showModal(modalDialog("", paste0("no .pld1. files in directory '", dir, "'")))
               } else {
                 g <<- t
                 state$flag <- rep(1, length(g[["pressure"]]))
                 showModal(modalDialog("", "Reading of pld1 files is complete. Next, select a plot type, colour scheme, navState limitations, etc. You may save your work at any time, for later loading by timestamp.", easyClose=TRUE))
               }
               SA <<- g[["SA"]]
               CT <<- g[["CT"]]
               p <<- g[["pressure"]]
               t <<- as.numeric(g[["time"]]) # in seconds, for hover operations
               state$gliderExists <- TRUE
               cat(file=stderr(), ".. done\n", sep="")
  })

  observeEvent(input$loadRdaAction, {
               cat(file=stderr(), "loadRda...\n")
               filename <- paste(tolower(input$glider), "_", tolower(input$mission),
                                 "_", input$rdaInputFile, ".rda", sep="")
               ##cat(file=stderr(), "  input$glider '", input$glider, "' ...\n", sep="")
               ##cat(file=stderr(), "  input$mission '", input$mission, "' ...\n", sep="")
               cat(file=stderr(), "  load from '", filename, "' ..", sep="")
               load(filename)
               g <<- g
               SA <<- g[["SA"]]
               CT <<- g[["CT"]]
               p <<- g[["pressure"]]
               t <<- as.numeric(g[["time"]]) # in seconds, for hover operations
               state$flag <- g[["pressureFlag"]]
               cat(file=stderr(), ". done\n", sep="")
               state$rda <- filename
               state$gliderExists <- TRUE
               showModal(modalDialog("", "Loading of previous analysis is complete. Next, select a plot type, colour scheme, navState limitations, etc. You may save your work at any time, for later loading by timestamp.", easyClose=TRUE))

  })

  observeEvent(input$saveRda, {
               cat(file=stderr(), "saveRda...\n")
               rda <- rdaName()
               cat(file=stderr(), "  save 'g' and 'edits' to '", rda, "' ...\n", sep="")
               visible <- g[["navState"]] %in% input$navState
               g@metadata$flags$payload1$pressure <<- ifelse(visible, state$flag, 3)
               edits[[1+length(edits)]] <<- list(category="navState", time=presentTime(), bad=!visible)
               cat(file=stderr(), "  updated edits; new length is ", length(edits), "; sum(!visible)=", sum(!visible), "\n", sep="")
               save(edits, g, file=rda)
               cat(file=stderr(), "  ... done\n", sep="")
  })


  output$plot <- renderPlot({
    cat(file=stderr(), "plot with input$plotChoice='", paste(input$plotChoice, collapse=","), "'\n", sep="")
    ##cat(file=stderr(), "  length(edits)=", length(edits), "\n", sep="")
    ##cat(file=stderr(), "  plotExists=", plotExists, "\n", sep="")
    ##cat(file=stderr(), "  colorBy=", input$colorBy, "\n", sep="")
    if (!is.null(g))  {
      ##cat(file=stderr(), "  have 'g' so can plot\n", sep="")
      n <- length(g[["pressure"]])
      ##cat(file=stderr(), "  n=", n, "\n", sep="")
      ## if (is.null(state$flag)) {
      ##   state$flag <- rep(1, n)
      ##   cat(file=stderr(), "  setting flag\n", sep="")
      ## }
      ##cat(file=stderr(), "  state$flag[1:5]=", paste(state$flag[1:5],collapse=","), "\n", sep="")
      visible <- g[["navState"]] %in% input$navState
      flagged <- state$flag == 3
      ##cat(file=stderr(), "  flagged[1:5]: ", paste(flagged[1:5],collapse=","), "\n", sep="")
      ##cat(file=stderr(), "  colorBy: \"", input$colorBy, "\"\n", sep="")
      if (input$plotChoice == "pt") {
        cat(file=stderr(), "  pt\n")
        t <- g[["time"]]
        p <- g[["pressure"]]
        ##OLD if (input$flagAction == "highlight") {
        ##OLD   cat(file=stderr(), "  highlight\n")
        ##OLD   oce.plot.ts(t[visible], p[visible], type="p", ylab="Pressure [dbar]", pch=".",
        ##OLD               cex=3, col=ifelse(flagged, 1, 2))
        ##OLD   points(t[visible & flagged], p[visible & flagged], pch=1, cex=1.4)
        ##OLD } else if (input$flagAction == "omit") {
          cat(file=stderr(), "  omit\n")
          oce.plot.ts(t[!flagged & visible], p[!flagged & visible], type="p", ylab="Pressure [dbar]", pch=".", cex=3)
        ##OLD} else {
        ##OLD  ##stop("unhandled flagAction='", input$flagAction, "' detected by plot")
        ##OLD}
        if (!is.null(state$yoSelected)) {
          yo <- g[["yoNumber"]]
          show <- yo == state$yoSelected & visible
          lines(t[show], p[show], lwd=2, type="o")
          mtext(paste("line is yo", state$yoSelected), adj=1)
        }
        plotExists <<- TRUE
      } else if (input$plotChoice == "TS") {
        cat(file=stderr(), "  TS\n", sep="")
        visible <- g[["navState"]] %in% input$navState
        gg <- g
        ##OLD if (input$flagAction == "omit") {
        gg@data$payload1 <- g@data$payload1[!flagged & visible,] # FIXME: use subset() instead?
        if (input$colorBy != "(none)") {
          cm <- colormap(gg[[input$colorBy]])
          drawPalette(colormap=cm, zlab=input$colorBy)
          plotTS(gg, pch=1, cex=0.3, col=cm$zcol, mar=c(3, 3, 2, 5.5))
        } else {
          plotTS(gg, pch=1, cex=0.3)
        }
        ##OLD } else {
        ##OLD   gg@data$payload1 <- g@data$payload1[visible,] # FIXME: use subset() instead?
        ##OLD   if (input$colorBy != "(none)") {
        ##OLD     cm <- colormap(gg[[input$colorBy]])
        ##OLD     drawPalette(colormap=cm, zlab=input$colorBy)
        ##OLD     plotTS(gg, pch=1, cex=ifelse(flagged[visible], 1, 0.3), col=cm$zcol, mar=c(3, 3, 2, 5.5))
        ##OLD   } else {
        ##OLD     plotTS(gg, pch=1, cex=ifelse(flagged[visible], 1, 0.3))
        ##OLD   }
        ##OLD }
        if (!is.null(state$yoSelected)) {
          yo <- g[["yo", state$yoSelected]]
          visible <- yo[["navState"]] %in% input$navState
          lines(yo[["SA"]][visible], yo[["CT"]][visible], lwd=3)
          points(yo[["SA"]][visible], yo[["CT"]][visible], pch=20, cex=1.4)
          mtext(paste("line is yo ", state$yoSelected))
        }
        plotExists <<- TRUE
      } else if (input$plotChoice == "hist(p)") {
        cat(file=stderr(), "  hist(p)\n", sep="")
        p <- g[["pressure"]]
        ##OLD if (input$flagAction == "colourize") {
        ##OLD   hist(p[visible], breaks=100, main="p")
        ##OLD } else if (input$flagAction == "omit") {
          hist(p[!flagged & visible], breaks=100, main="p trimmed to unflagged values")
        ##OLD } else {
        ##OLD   stop("unhandled flagAction='", input$flagAction, "' detected by plot")
        ##OLD }
      } else if (input$plotChoice == "hist(S)") {
        cat(file=stderr(), "  hist(S)\n", sep="")
        SA <- g[["SA"]]
        ##OLD if (input$flagAction == "colourize") {
        ##OLD   hist(SA[visible], breaks=100, main="NOTE: colorization not sensible here")
        ##OLD } else if (input$flagAction == "omit") {
          hist(SA[!flagged & visible], breaks=100, main="SA trimmed to unflagged values")
        ##OLD } else {
        ##OLD   stop("unhandled flagAction='", input$flagAction, "' detected by plot")
        ##OLD }
        cat(file=stderr(), "summary(SA):", summary(SA), "\n")
        cat(file=stderr(), "summary(SA[!flagged]):", summary(SA[!flagged]), "\n")
      } else if (input$plotChoice == "yo") {
        cat(file=stderr(), "  yo=", state$yoSelected, "\n")
        yo <- g[["yoNumber"]]
        look <- yo == state$yoSelected
        visible <- g[["navState"]] %in% input$navState
        yoData <- g[["payload1"]][look & visible, ]
        ctd <- with(yoData, as.ctd(salinity=salinity, temperature=temperature, pressure=pressure, longitude=longitude, latitude=latitude))
        par(mfrow=c(2, 2))
        plotProfile(ctd, xtype="salinity+temperature", type="o", pch=20, cex=3/4)
        plotProfile(ctd, xtype="density", type="o", pch=20, cex=3/4)
        plotTS(ctd, type="o", pch=20, cex=3/4)
        ## plotProfile(ctd, xtype="index", type="o", pch=20, cex=3/4)
        plotScan(ctd, type="o", pch=20, cex=3/4)
        par(mfrow=c(1, 1)) # return to normal state
      }
      plotExists <<- TRUE
      state$usr <<- par("usr")
    } else {
      state$usr <<- NULL
    }
  }) # renderPlot
  outputOptions(output, "gliderExists", suspendWhenHidden = FALSE)
} # server

shinyApp(ui=ui, server=server)
