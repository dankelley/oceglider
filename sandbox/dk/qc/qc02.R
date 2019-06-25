## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2


debug <- "Yes"
version <- "0.1.1"
pressureThreshold <- 0.5
pch <- 1
pch <- "." # 1
cex <- 3 # 0.75

library(shiny)
##library(shinythemes)
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

navStateColors <- function(navState)
{
  ## use str(navStateCodes("seaexplorer")) to see codes
  ## descending          : num 100
  ## not_navigating      : num 105
  ## inflecting_downwards: num 110
  ## surfacing           : num 115
  ## at_surface          : num 116
  ## ascending           : num 117
  ## inflecting_upwards  : num 118
  n <- length(navState)
  res <- rep("-", length=n)            # will create an error for an unknown navState
  res[navState == 100] <- "pink"       # descending
  res[navState == 105] <- "black"      # not_navigating
  res[navState == 110] <- "gold"       # inflecting_upwards
  res[navState == 115] <- "purple"     # surfacing
  res[navState == 116] <- "red"        # at_surface
  res[navState == 117] <- "green"      # ascending
  res[navState == 118] <- "blue"       # inflecting_upwards
  res
}

navStateLegend <- function()
{
  nsc <- navStateCodes("seaexplorer")
  legend("top", pch=20, bg="transparent", x.intersp=0.5, cex=0.8, pt.cex=1.5,
         col=navStateColors(nsc), legend=names(nsc), ncol=length(nsc))
}

var <- list()
src <- ""
g <- NULL
SA <- NULL
CT <- NULL
p <- NULL
t <- NULL
maxYo <- 0

ui <- fluidPage(tags$style(HTML("body {font-family: 'Arial'; font-size: 12px; margin-left:1ex} hr {size: '50'}")),
                fluidRow(column(2, radioButtons("debug", "Debug", choices=c("Yes", "No"), selected="Yes", inline=TRUE)),
                         column(2, radioButtons("instructions", "Instructions", choices=c("Hide", "Show"), selected="Hide", inline=TRUE))),
                hr(),
                conditionalPanel(condition="input.instructions=='Show'", fluidRow(includeMarkdown("qc02_help.md"))),
                fluidRow(column(2,
                                ##h5("Read original data"),
                                uiOutput(outputId="glider"),
                                uiOutput(outputId="mission"),
                                uiOutput(outputId="read")),
                         column(3,
                                ##h5("Continue previous analysis"),
                                uiOutput(outputId="listRda"),
                                uiOutput(outputId="loadRda"),
                                conditionalPanel(condition="output.gliderExists",
                                                 uiOutput(outputId="saveRda"))),
                         column(2,
                                conditionalPanel(condition="output.gliderExists",
                                                 uiOutput(outputId="plotChoice")),
                                conditionalPanel(condition="output.gliderExists",
                                                 uiOutput(outputId="colorBy")),
                                conditionalPanel(condition="output.gliderExists",
                                                 uiOutput(outputId="plotType"))),
                         column(2,
                                conditionalPanel(condition="output.gliderExists",
                                                 uiOutput(outputId="focus")),
                                conditionalPanel(condition="input.focus == 'yo'",
                                                 uiOutput(outputId="focusYo")),
                                conditionalPanel(condition="input.focus == 'yo'",
                                                 uiOutput(outputId="previousYo")),
                                conditionalPanel(condition="input.focus == 'yo'",
                                                 uiOutput(outputId="nextYo")))
                         ),
                fluidRow(uiOutput(outputId="navState"),
                         uiOutput(outputId="despikePressure")),
                fluidRow(uiOutput(outputId="status")),
                fluidRow(plotOutput("plot",
                                    hover="hover",
                                    click="click",
                                    width="100%",
                                    height="600px",
                                    brush=brushOpts(id="brush", resetOnNew=!TRUE))))


server <- function(input, output) {
  msg <- function(...) {
    if (debug == "Yes")
      cat(file=stderr(), ...)
  }


  plotExists <- FALSE
  ##state <- reactiveValues(rda="", flag=NULL, focusYo="1", yoSelected=NULL, gliderExists=FALSE, usr=NULL)
  state <- reactiveValues(rda="", flag=NULL, focusYo="1", gliderExists=FALSE, usr=NULL)

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
      ## msg("dataName() called with input$glider='", glider, "' and input$mission='", mission, "'\n", sep="")
      if (tolower(glider) == "sea019") {
        if (tolower(mission) == "m28")
          return(paste(basedir, glider, "Data", mission, "Payload/logs", sep="/")) # no .pld1. files
        if (tolower(mission) == "m29")
          return(paste(basedir, glider, "Data", mission, "Payload/logs/logs", sep="/"))
        if (tolower(mission) == "m30")
          return(paste(basedir, glider, "Data", mission, "Payload/logs/logs", sep="/"))
        if (tolower(mission) == "m31")
          return(paste(basedir, glider, "Data", mission, mission, "Payload/logs", sep="/"))
        if (tolower(mission) == "m32")
          return(paste(basedir, glider, "Data", mission, mission, "Pld/logs", sep="/"))
        if (tolower(mission) == "m33")
          return(paste(basedir, glider, "Data", mission, mission, "pld/logs", sep="/"))
        if (tolower(mission) == "m34")
          return(paste(basedir, glider, "Data", mission, mission, "PLD/logs", sep="/"))
        if (tolower(mission) == "m35")
          return(paste(basedir, glider, "Data", mission, mission, "PLD/logs", sep="/"))
        if (tolower(mission) == "m36")
          return(paste(basedir, glider, "Data", mission, mission, "Payload/logs/logs", sep="/"))
        if (tolower(mission) == "m43")
          return(paste(basedir, glider, "Data", mission, "Payload/logs", sep="/"))
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
    tolower(paste0(varName(), "_", format(presentTime(), "%Y-%m-%d_%H:%M:%S"), ".rda", sep=""))
  }

  output$gliderExists <- reactive({
    msg("***gliderExists... (will return ", state$gliderExists, ")***\n")
    state$gliderExists
  })

  output$glider <- renderUI({
    state$gliderExists <- FALSE
    selectInput(inputId="glider",
                label="Read raw data",
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
    actionButton(inputId="readData", label="Read")
  })

  output$listRda <- renderUI({
    ##msg("output$listRda\n")
    files <- relevantRdaFiles(input$glider, input$mission)
    if (length(files)) {
      filedates <- gsub("([a-z0-9]*)_([a-z0-9]*)_(.*).rda", "\\3", files)
      selectInput(inputId="rdaInputFile", label="Continue", choices=filedates, selected=filedates[1])
    }
  })

  output$loadRda <- renderUI({
    msg("output$loadRda\n")
    files <- relevantRdaFiles(input$glider, input$mission)
    if (length(files)) {
      actionButton(inputId="loadRdaAction", label="Load previous")#h5(paste0("Load ", rda)))
    }
  })

  output$saveRda <- renderUI({
    msg("output$saveRda\n")
    ## hr()
    actionButton(inputId="saveRda", label="Save")
  })

  output$focus <- renderUI({
    msg("output$focus\n")
    selectInput(inputId="focus",
                label="Focus",
                choices=c("mission", "yo"),
                selected=c("mission"))
  })

  output$plotChoice <- renderUI({
    selectInput(inputId="plotChoice",
                label="Plot",
                choices=c("p(t)", "C(t)", "S(t)", "T(t)", "TS", "S profile", "T profile", "density profile", "hist(C)", "hist(S)", "hist(p)"),
                selected="p(t)")
  })

  output$plotType <- renderUI({
    selectInput(inputId="plotType", label="type", choices= c("l", "p", "o"), selected=c("p"))
  })

  output$colorBy <- renderUI({
    selectInput(inputId="colorBy",
                label="Colour by",
                choices=c("latitude", "longitude", "pressure", "temperature", "salinity", "navState", "(none)"),
                selected="latitude")
  })

  output$focusYo <- renderUI({
    textInput("focusYo", "Yo number", value=if (is.null(state$focusYo)) "1" else state$focusYo)
  })

  observeEvent(input$focusYo, {
               msg("'focusYo' text area altered\n")
               state$focusYo <- as.numeric(input$focusYo)
  })

  output$previousYo <- renderUI({
#    if (input$focus == 'yo' && as.numeric(input$yoSelected) > 1)
      actionButton(inputId="previousYo", label="Previous yo")
  })

  observeEvent(input$previousYo, {
               msg("'Previous Yo' button clicked; state$focusYo=", state$focusYo, "\n")
               if (state$focusYo > 1)
                 state$focusYo <<- state$focusYo - 1
  })

  output$nextYo <- renderUI({
#    if (input$focus == 'yo' && as.numeric(input$yoSelected) < maxYo)
      actionButton(inputId="nextYo", label="Next yo")
  })

  observeEvent(input$nextYo, {
               msg("'Next Yo' button clicked; state$focusYo=", state$focusYo, "\n")
               if (state$focusYo < maxYo)
                 state$focusYo <<- state$focusYo + 1
  })

  ##deleteYo output$deleteYo <- renderUI({
  ##deleteYo   if (!is.null(state$yoSelected)) {
  ##deleteYo     actionButton(inputId="deleteYo", label=paste("Delete yo #", state$yoSelected, sep=""))
  ##deleteYo   }
  ##deleteYo })

  output$navState <- renderUI({
    ns <- navStateCodes("seaexplorer")
    checkboxGroupInput(inputId="navState", label="Show navState",
                       choices=ns, selected=ns, inline=TRUE)
  })

  output$status <- renderText({
    x <- input$hover$x
    y <- input$hover$y
    res <- if (is.null(g)) "Status: no glider exists. Please read data or load previous analysis." else paste(input$glider, input$mission)
    res <- "(Move mouse into plot window to see properties)"
    distThreshold <- 0.05
    flagged <- state$flag == 3
    if (!is.null(x) && plotExists) {
      ## note scaling of the x and y, dependent on plot type. The scales are a rough guess.
      if (input$plotChoice == "p(t)") {
        dist <- sqrt(((x-t)/(state$usr[2]-state$usr[1]))^2 + ((y-p)/(state$usr[4]-state$usr[3]))^2)
        dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
        disti <- which.min(dist)
        d <- g[["payload1"]][disti,]
        res <- sprintf("yo=%d p=%.1f SA=%.4f CT=%.4f navState=%d (%.3fE %.3fN %s)\n",
                       d$yoNumber, d$pressure, d$SA, d$CT, d$navState,
                       d$longitude, d$latitude, format(d$time, "%Y-%m-%dT%H:%M:%S"))
      } else if (input$plotChoice == "S(t)") {
        dist <- sqrt(((x-t)/(state$usr[2]-state$usr[1]))^2 + ((y-SA)/(state$usr[4]-state$usr[3]))^2)
        dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
        disti <- which.min(dist)
        d <- g[["payload1"]][disti,]
        res <- sprintf("yo=%d p=%.1f SA=%.4f CT=%.4f navState=%d (%.3fE %.3fN %s)\n",
                       d$yoNumber, d$pressure, d$SA, d$CT, d$navState,
                       d$longitude, d$latitude, format(d$time, "%Y-%m-%dT%H:%M:%S"))
      } else if (input$plotChoice == "T(t)") {
        dist <- sqrt(((x-t)/(state$usr[2]-state$usr[1]))^2 + ((y-CT)/(state$usr[4]-state$usr[3]))^2)
        dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
        disti <- which.min(dist)
        d <- g[["payload1"]][disti,]
        res <- sprintf("yo=%d p=%.1f SA=%.4f CT=%.4f navState=%d (%.3fE %.3fN %s)\n",
                       d$yoNumber, d$pressure, d$SA, d$CT, d$navState,
                       d$longitude, d$latitude, format(d$time, "%Y-%m-%dT%H:%M:%S"))
       } else if (input$plotChoice == "TS") {
        dist <- sqrt(((x-SA)/(state$usr[2]-state$usr[1]))^2 + ((y-CT)/(state$usr[4]-state$usr[3]))^2)
        dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
        disti <- which.min(dist)
        d <- g[["payload1"]][disti,]
        res <- sprintf("yo=%d p=%.1f SA=%.4f CT=%.4f navState=%d (%.3fE %.3fN %s)\n",
                       d$yoNumber, d$pressure, d$SA, d$CT, d$navState,
                       d$longitude, d$latitude, format(d$time, "%Y-%m-%dT%H:%M:%S"))
      }
    }
    res
  })

  observeEvent(input$debug, {
               cat(file=stderr(), "input$debug=", input$debug, "\n")
               debug <<- input$debug
  })


  observeEvent(input$deleteYo, {
               msg("  DELETE yo ***IGNORED***\n")
               ##msg("  DELETE yo=", state$yoSelected, " ***IGNORED***\n")
               ## yo <- g[["yoNumber"]]
               ## bad <- yo == state$yoSelected
               ## oldFlag <- state$flag
               ## state$flag[bad] <- 3
               ## newFlag <- state$flag
               ## index <- which(newFlag != oldFlag)
               ## old <- oldFlag[index]
               ## new <- newFlag[index]
               ## edits[[1+length(edits)]] <<- list(category=paste("delete yo", state$yoSelected),
               ##                                   time=presentTime(),
               ##                                   index=index, oldFlag=old, newFlag=new)
               ## msg("  updated edits; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               ## state$yoSelected <<- NULL
  })

  observeEvent(input$click, {
               msg("click-to-select-yo **IGNORED**\n")
               ## distThreshold <- 0.05
               ## x <- input$click$x
               ## y <- input$click$y
               ## flagged <- state$flag == 3
               ## if (input$plotChoice == "p(t)") {
               ##   dist <- sqrt(((x-t)/(state$usr[2]-state$usr[1]))^2 + ((y-p)/(state$usr[4]-state$usr[3]))^2)
               ##   dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
               ##   disti <- which.min(dist)
               ##   d <- g[["payload1"]][disti,]
               ##   res <- sprintf("dist=%.4f x=%.4f y=%.4f (yo=%d, t=%s, p=%.1f)\n",
               ##                  dist[disti], x, y, d$yoNumber, d$time, d$pressure)
               ##   state$yoSelected <- if (dist[disti] < distThreshold) d$yoNumber else NULL
               ##   msg(res)
               ## } else if (input$plotChoice == "TS") {
               ##   dist <- sqrt(((x-SA)/(state$usr[2]-state$usr[1]))^2 + ((y-CT)/(state$usr[4]-state$usr[3]))^2)
               ##   dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
               ##   disti <- which.min(dist)
               ##   d <- g[["payload1"]][disti,]
               ##   res <- sprintf("dist=%.4f x=%.4f y=%.4f (yo=%d, t=%s, p=%.1f, S=%.4f, T=%.4f)\n",
               ##                  dist[disti], x, y, d$yoNumber, d$time, d$pressure, d$salinity, d$temperature)
               ##   state$yoSelected <- if (dist[disti] < distThreshold) d$yoNumber else NULL
               ##   msg(res)
               ## }
  })

  observeEvent(input$brush, {
               xmin <- input$brush$xmin
               xmax <- input$brush$xmax
               ymin <- input$brush$ymin
               ymax <- input$brush$ymax
               bad <- NULL
               msg("brush\n")
               msg("  xmin=", xmin, ", xmax=", xmax, "\n", sep="")
               msg("  ymin=", ymin, ", ymax=", ymax, "\n", sep="")
               msg("  count of flag==3: ", sum(state$flag==3), " (before)\n", sep="")
               ## state$yoSelected <- NULL # brushing turns off yo selection
               if (input$plotChoice == "p(t)") {
                 msg("  pt\n")
                 x <- as.numeric(g[["time"]])
                 y <- g[["pressure"]]
                 bad <- xmin <= x & x <= xmax & ymin <= y & y <= ymax
                 bad[is.na(bad)] <- TRUE
                 state$flag[bad] <- 3
                 edits[[1+length(edits)]] <<- list(category="brush pt", time=presentTime(), bad=bad)
                 msg("  updated edits for brushed pt; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               } else if (input$plotChoice == "TS") {
                 msg("  TS\n")
                 x <- g[["SA"]] # FIXME: allow oceEOS=="unesco"
                 y <- g[["CT"]]
                 bad <- xmin <= x & x <= xmax & ymin <= y & y <= ymax
                 bad[is.na(bad)] <- TRUE
                 msg(" sum(bad)=", sum(bad), "\n")
                 edits[[1+length(edits)]] <<- list(category="brush TS", time=presentTime(), bad=bad)
                 msg("  updated edits for brushed TS; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               } else if (input$plotChoice == "S profile") {
                 msg("  S profile\n")
                 x <- g[["SA"]] # FIXME: allow oceEOS=="unesco"
                 y <- g[["pressure"]]
                 bad <- xmin <= x & x <= xmax & ymin <= y & y <= ymax
                 bad[is.na(bad)] <- TRUE
                 msg(" sum(bad)=", sum(bad), "\n")
                 edits[[1+length(edits)]] <<- list(category="brush Sprofile", time=presentTime(), bad=bad)
                 msg("  updated edits for brushed Sprofile; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               } else if (input$plotChoice == "T profile") {
                 msg("  T profile\n")
                 x <- g[["CT"]] # FIXME: allow oceEOS=="unesco"
                 y <- g[["pressure"]]
                 bad <- xmin <= x & x <= xmax & ymin <= y & y <= ymax
                 bad[is.na(bad)] <- TRUE
                 msg(" sum(bad)=", sum(bad), "\n")
                 edits[[1+length(edits)]] <<- list(category="brush Tprofile", time=presentTime(), bad=bad)
                 msg("  updated edits for brushed Tprofile; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               } else if (input$plotChoice == "density profile") {
                 msg("  density profile\n")
                 x <- g[["sigma0"]] # FIXME: allow oceEOS=="unesco"
                 y <- g[["pressure"]]
                 bad <- xmin <= x & x <= xmax & ymin <= y & y <= ymax
                 bad[is.na(bad)] <- TRUE
                 msg(" sum(bad)=", sum(bad), "\n")
                 edits[[1+length(edits)]] <<- list(category="brush density profile", time=presentTime(), bad=bad)
                 msg("  updated edits for brushed density profile; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               } else if (input$plotChoice == "hist(C)") {
                 msg("  hist(C)\n")
                 x <- g[["conductivity"]]
                 bad <- xmin <= x & x <= xmax
                 bad[is.na(bad)] <- TRUE
                 msg(" sum(bad)=", sum(bad), "\n")
                 ## bad[is.na(bad)] <- TRUE
                 edits[[1+length(edits)]] <<- list(category="brush hist(C)", time=presentTime(), bad=bad)
                 msg("  updated edits for brushed hist(C; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
                } else if (input$plotChoice == "hist(S)") {
                 msg("  hist(S)\n")
                 x <- g[["SA"]] # FIXME: allow oceEOS=="unesco"
                 bad <- xmin <= x & x <= xmax
                 bad[is.na(bad)] <- TRUE
                 msg(" sum(bad)=", sum(bad), "\n")
                 ## bad[is.na(bad)] <- TRUE
                 edits[[1+length(edits)]] <<- list(category="brush hist(S)", time=presentTime(), bad=bad)
                 msg("  updated edits for brushed hist(S; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               } else if (input$plotChoice == "hist(p)") {
                 msg("  hist(p)\n")
                 x <- g[["pressure"]]
                 bad <- xmin <= x & x <= xmax
                 bad[is.na(bad)] <- TRUE
                 msg(" sum(bad)=", sum(bad), "\n")
                 #if (any(is.na(bad))) browser()
                 edits[[1+length(edits)]] <<- list(category="brush hist(p)", time=presentTime(), bad=bad)
                 msg("  updated edits for brushed hist(p); new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
               }
               if (sum(bad)) {
                 bad <- which(bad)
                 msg("  total number of bad points: ", length(bad), "\n", sep="")
                 state$flag[bad] <- 3 # HERE HERE HERE
               }
  })

  observeEvent(input$readData, {
               msg("readData...\n")
               dir <- dataName()
               msg("  about to read '", dir, "' .", sep="")
               tmp <- try(read.glider.seaexplorer.delayed(dir))
               if (inherits(tmp, "try-error")) {
                 msg(" no data FIXME: put up a dialog box\n")
                 showModal(modalDialog("", paste0("no .pld1. files in directory '", dir, "'")))
               } else {
                 g <<- tmp
                 state$flag <- rep(1, length(g[["pressure"]]))
                 showModal(modalDialog("", "Reading of pld1 files is complete. Next, select a plot type, colour scheme, navState limitations, etc. You may save your work at any time, for later loading by timestamp.", easyClose=TRUE))
               }
               g@data$payload1[["SA"]] <<- g[["SA"]]
               g@data$payload1[["CT"]] <<- g[["CT"]]
               g@data$payload1[["sigma0"]] <<- g[["sigma0"]]
               g@data$payload1[["spiciness"]] <<- g[["spiciness"]]
               g@data$payload1[["navStateColor"]] <<- navStateColors(g[["navState"]])
               p <<- g[["pressure"]]
               t <<- as.numeric(g[["time"]]) # in seconds, for hover operations
               state$gliderExists <- TRUE
               msg("... done reading data\n")
  })

  observeEvent(input$loadRdaAction, {
               msg("loadRda...\n")
               filename <- paste(tolower(input$glider), "_", tolower(input$mission),
                                 "_", input$rdaInputFile, ".rda", sep="")
               msg("  load from '", filename, "' ..", sep="")
               load(filename)
               g <<- g
               SA <<- g[["SA"]]
               CT <<- g[["CT"]]
               p <<- g[["pressure"]]
               maxYo <<- max(g[["yoNumber"]])
               t <<- as.numeric(g[["time"]]) # in seconds, for hover operations
               state$flag <- g[["pressureFlag"]]
               msg(". done\n")
               state$rda <- filename
               state$gliderExists <- TRUE
               ##showModal(modalDialog("", "Loading of previous analysis is complete. Next, select a plot type, colour scheme, navState limitations, etc. You may save your work at any time, for later loading by timestamp.", easyClose=TRUE))

  })

  observeEvent(input$saveRda, {
               msg("saveRda...\n")
               rda <- rdaName()
               msg("  save 'g' and 'edits' to '", rda, "' ...\n", sep="")
               visible <- g[["navState"]] %in% input$navState
               g@metadata$flags$payload1$pressure <<- ifelse(visible, state$flag, 3)
               edits[[1+length(edits)]] <<- list(category="navState", time=presentTime(), bad=!visible)
               msg("  updated edits; new length is ", length(edits), "; sum(!visible)=", sum(!visible), "\n", sep="")
               mission <- input$mission
               glider <- input$glider
               sourceDirectory <- dataName()
               save(glider, mission, sourceDirectory, edits, g, file=rda)
               msg("  ... done\n", sep="")
  })

  output$plot <- renderPlot({
    msg("plot with input: focus='", input$focus, "'",
        ", plotChoice='", input$plotChoice, "'",
        ", despikePressure=", input$despikePressure,
        ", selectYo='", input$selectYo, "'",
        ", focusYo=", state$focusYo,
        ", plotType=", input$plotType,
        "\n", sep="")
    if (!is.null(g))  {
      n <- length(g[["pressure"]])

      if (input$despikePressure) {
        p <- g[["pressure"]]
        msg("calculating badPressure with pressureThreshold=", pressureThreshold, "\n")
        msg("  head(p)=", paste(head(p), collapse=" "), "\n")
        badp <- is.na(p)
        if (any(badp))
          p[badp] <- mean(p, na.rm=TRUE) # will trim later anyhow
        pressureShift <- abs(p - runmed(p, k=11))
        msg("  head(pressureShift)=", paste(head(pressureShift), collapse=" "), "\n")
        badPressure <- pressureShift > pressureThreshold
        if (any(badp))
          badPressure[badp] <- TRUE
        msg("  head(badPressure)=", paste(head(badPressure), collapse=" "), "\n")
      } else {
        badPressure <- rep(FALSE, n)
      }

      ## flagged and visible yield 'look', which is used in many plot types
      flagged <- state$flag == 3
      visible <- (g[["navState"]] %in% input$navState) & !badPressure
      ##msg("  sum(visible)=", sum(visible), " before looking at input$focus\n", sep="")
      if (input$focus == "yo")
        visible <- visible & (g[["yoNumber"]] == as.numeric(state$focusYo))
      ##msg("  sum(visible)=", sum(visible), " after looking at input$focus\n", sep="")
      look <- !flagged & visible
      gg <- g
      gg@data$payload1 <- g@data$payload1[look, ]

      if (input$plotChoice == "p(t)") {
        t <- gg[["time"]]
        p <- gg[["pressure"]]
        if (input$colorBy != "(none)") {
          if (input$colorBy == "navState") {
            oce.plot.ts(t, p,
                        type=input$plotType,
                        col=gg[["navStateColor"]],
                        ylab="Pressure [dbar]", pch=pch, cex=cex, flipy=TRUE)
            navStateLegend()
          } else {
            cm <- colormap(g[[input$colorBy]][look])
            drawPalette(colormap=cm, zlab=input$colorBy)
            omar <- par("mar")
            par(mar=c(3, 3, 2, 5.5), mgp=c(2, 0.7, 0))
            oce.plot.ts(t, p,
                        type=input$plotType,
                        col=cm$zcol,
                        ylab="Pressure [dbar]", pch=pch, cex=cex, flipy=TRUE)
            par(mar=omar)
          }
        } else {
          oce.plot.ts(t, p,
                      type=input$plotType,
                      ylab="Pressure [dbar]", pch=pch, cex=cex, flipy=TRUE)
        }
        plotExists <<- TRUE
      } else if (input$plotChoice == "C(t)") {
        x <- g[["time"]][look]
        y <- g[["conductivity"]][look]
        if (input$colorBy != "(none)") {
          if (input$colorBy == "navState") {
            oce.plot.ts(x, y, type=input$plotType, pch=pch, cex=cex,
                        ylab=resizableLabel("conductivity S/m"),
                        col=g[["navStateColor"]][look])
            navStateLegend()
          } else {
            cm <- colormap(g[[input$colorBy]][look])
            drawPalette(colormap=cm, zlab=input$colorBy)
            omar <- par("mar")
            par(mar=c(3, 3, 2, 5.5), mgp=c(2, 0.7, 0))
            oce.plot.ts(x, y, type=input$plotType, pch=pch, cex=cex,
                        ylab=resizableLabel("conductivity S/m"),
                        col=cm$zcol)
            par(mar=omar)
          }
        } else {
          oce.plot.ts(x, y, type=input$plotType, pch=pch, cex=cex,
                      ylab=resizableLabel("conductivity S/m"))
        }
        plotExists <<- TRUE
      } else if (input$plotChoice == "S(t)") {
        x <- g[["time"]][look]
        y <- g[["SA"]][look]
        if (input$colorBy != "(none)") {
          if (input$colorBy == "navState") {
            oce.plot.ts(x, y, type=input$plotType, pch=pch, cex=cex,
                        ylab=resizableLabel("absolute salinity"),
                        col=g[["navStateColor"]][look])
            navStateLegend()
          } else {
            cm <- colormap(g[[input$colorBy]][look])
            drawPalette(colormap=cm, zlab=input$colorBy)
            omar <- par("mar")
            par(mar=c(3, 3, 2, 5.5), mgp=c(2, 0.7, 0))
            oce.plot.ts(x, y, type=input$plotType, pch=pch, cex=cex,
                        ylab=resizableLabel("absolute salinity"),
                        col=cm$zcol)
            par(mar=omar)
          }
        } else {
          oce.plot.ts(x, y, type=input$plotType, pch=pch, cex=cex,
                      ylab=resizableLabel("absolute salinity"))
        }
        plotExists <<- TRUE
      } else if (input$plotChoice == "T(t)") {
        x <- g[["time"]][look]
        y <- g[["CT"]][look]
        if (input$colorBy != "(none)") {
          if (input$colorBy == "navState") {
            oce.plot.ts(x, y,
                        type=input$plotType, pch=pch, cex=cex,
                        col=g[["navStateColor"]][look],
                        ylab=resizableLabel("conservative temperature"))
            navStateLegend()
          } else {
            cm <- colormap(g[[input$colorBy]][look])
            drawPalette(colormap=cm, zlab=input$colorBy)
            omar <- par("mar")
            par(mar=c(3, 3, 2, 5.5), mgp=c(2, 0.7, 0))
            oce.plot.ts(x, y, type=input$plotType, pch=pch, cex=cex,
                        col=cm$zcol,
                        ylab=resizableLabel("conservative temperature"))
            par(mar=omar)
          }
        } else {
          oce.plot.ts(x, y, type=input$plotType, pch=pch, cex=cex,
                      ylab=resizableLabel("conservative temperature"))
        }
        plotExists <<- TRUE
      } else if (input$plotChoice == "TS") {
        gg <- g
        gg@data$payload1 <- g@data$payload1[look, ]
        if (input$colorBy != "(none)") {
          if (input$colorBy == "navState") {
            if (FALSE) {
              ## timing test with random numbers
              n <- length(look)
              x <- rnorm(n)
              y <- rnorm(n)
              cm <- colormap(x^2+y^2)
              timing <- system.time({
                plot(x, y, col=cm$zcol, cex=cex, pch=pch)
              })
              msg("plot() with ", n, " random points) took time ", paste(timing, collapse=" "), sep="")
            } else {
              ## Actual plot
              timing <- system.time({
                plotTS(gg, pch=pch, cex=cex, col=gg[["navStateColor"]],
                       mar=c(3, 3, 2, 5.5), type=input$plotType)
              })
              msg("plotTS (coloured by navState) took time ", paste(timing, collapse=" "), sep="")
            }
            navStateLegend()
          } else {
            cm <- colormap(gg[[input$colorBy]])
            drawPalette(colormap=cm, zlab=input$colorBy)
            timing <- system.time({
              plotTS(gg, pch=pch, cex=cex, col=cm$zcol, mar=c(3, 3, 2, 5.5), type=input$plotType)
            })
            msg("plotTS (coloured by ", input$colorBy, ") took time ", paste(timing, collapse=" "), sep="")
          }
        } else {
          timing <- system.time({
            plotTS(gg, pch=pch, cex=cex, type=input$plotType)
          })
          msg("plotTS (with no colours) took time ", paste(timing, collapse=" "), sep="")
        }
        plotExists <<- TRUE
      } else if (input$plotChoice == "S profile") {
        gg <- g
        gg@data$payload1 <- g@data$payload1[!flagged & visible,] # FIXME: use subset() instead?
        if (input$colorBy != "(none)") {
          omar <- par("mar")
          if (input$colorBy == "navState") {
            par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
            plot(gg[["SA"]], gg[["pressure"]], ylim=rev(range(gg[["pressure"]])),
                 type=input$plotType, pch=pch, cex=cex, col=g[["navStateColor"]][look],
                 xlab=resizableLabel("absolute salinity"),
                 ylab=resizableLabel("p"))
            navStateLegend()
          } else {
            cm <- colormap(gg[[input$colorBy]])
            drawPalette(colormap=cm, zlab=input$colorBy)
            par(mar=c(3, 3, 2, 5.5), mgp=c(2, 0.7, 0))
            plot(gg[["SA"]], gg[["pressure"]], ylim=rev(range(gg[["pressure"]])),
                 type=input$plotType, pch=pch, cex=cex, col=cm$zcol,
                 xlab=resizableLabel("absolute salinity"),
                 ylab=resizableLabel("p"))
          }
          par(mar=omar)
        } else {
          plot(gg[["SA"]], gg[["pressure"]], ylim=rev(range(gg[["pressure"]])),
               type=input$plotType, pch=pch, cex=cex,
               xlab=resizableLabel("absolute salinity"),
               ylab=resizableLabel("p"))
        }
      } else if (input$plotChoice == "T profile") {
        gg <- g
        gg@data$payload1 <- g@data$payload1[!flagged & visible,] # FIXME: use subset() instead?
        if (input$colorBy != "(none)") {
          omar <- par("mar")
          if (input$colorBy == "navState") {
            par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
            plot(gg[["CT"]], gg[["pressure"]], ylim=rev(range(gg[["pressure"]])),
                 type=input$plotType, pch=pch, cex=cex, col=gg[["navStateColor"]],
                 xlab=resizableLabel("conservative temperature"),
                 ylab=resizableLabel("p"))
            navStateLegend()
          } else {
            cm <- colormap(gg[[input$colorBy]])
            drawPalette(colormap=cm, zlab=input$colorBy)
            par(mar=c(3, 3, 2, 5.5), mgp=c(2, 0.7, 0))
            plot(gg[["CT"]], gg[["pressure"]], ylim=rev(range(gg[["pressure"]])),
                 type=input$plotType, pch=pch, cex=cex, col=cm$zcol,
                 xlab=resizableLabel("conservative temperature"),
                 ylab=resizableLabel("p"))
          }
          par(mar=omar)
        } else {
          plot(gg[["CT"]], gg[["pressure"]], ylim=rev(range(gg[["pressure"]])),
               type=input$plotType, pch=pch, cex=cex,
               xlab=resizableLabel("conservative temperature"),
               ylab=resizableLabel("p"))
        }
      } else if (input$plotChoice == "density profile") {
        gg <- g
        gg@data$payload1 <- g@data$payload1[!flagged & visible,] # FIXME: use subset() instead?
        if (input$colorBy != "(none)") {
          omar <- par("mar")
          if (input$colorBy == "navState") {
            par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
            plot(gg[["sigma0"]], gg[["pressure"]], ylim=rev(range(gg[["pressure"]])),
                 type=input$plotType, pch=pch, cex=cex, col=gg[["navStateColor"]],
                 xlab=resizableLabel("sigma0"),
                 ylab=resizableLabel("p"))
            navStateLegend()
          } else {
            cm <- colormap(gg[[input$colorBy]])
            drawPalette(colormap=cm, zlab=input$colorBy)
            omar <- par("mar")
            par(mar=c(3, 3, 2, 5.5), mgp=c(2, 0.7, 0))
            plot(gg[["sigma0"]], gg[["pressure"]], ylim=rev(range(gg[["pressure"]])),
                 type=input$plotType, pch=pch, cex=cex,
                 xlab=resizableLabel("sigma0"),
                 ylab=resizableLabel("p"))
          }
          par(mar=omar)
        } else {
          plot(gg[["sigma0"]], gg[["pressure"]], ylim=rev(range(gg[["pressure"]])),
               type=input$plotType, pch=pch, cex=cex,
               xlab=resizableLabel("sigma0"),
               ylab=resizableLabel("p"))
        }
      } else if (input$plotChoice == "hist(p)") {
        p <- g[["pressure"]]
        hist(p[!flagged & visible], breaks=100, main="p trimmed to unflagged values")
      } else if (input$plotChoice == "hist(C)") {
        C <- g[["conductivity"]]
        hist(C[!flagged & visible], breaks=100, main="C trimmed to unflagged values")
        msg("summary(C):", summary(C), "\n")
        msg("summary(C[!flagged]):", summary(C[!flagged]), "\n")
       } else if (input$plotChoice == "hist(S)") {
        SA <- g[["SA"]]
        hist(SA[!flagged & visible], breaks=100, main="SA trimmed to unflagged values")
        msg("summary(SA):", summary(SA), "\n")
        msg("summary(SA[!flagged]):", summary(SA[!flagged]), "\n")
      }
      plotExists <<- TRUE
      state$usr <<- par("usr")
    } else {
      state$usr <<- NULL
    }
  }, type="Xlib") # renderPlot. (The Xlib is to try to speed up.)
  outputOptions(output, "gliderExists", suspendWhenHidden = FALSE)

  output$despikePressure <- renderUI({
    checkboxInput(inputId="despikePressure", label="Despike pressure")
  })

} # server

shinyApp(ui=ui, server=server)

