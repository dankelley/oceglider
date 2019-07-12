## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2


debug <- "Yes"
version <- "0.1.1"
pressureThreshold <- 0.5
## Development Notes
## Circles for points look nice, but they are 5X to 10X slower than dots.
## NOTE: we could add pulldown menus for pch and cex, at the expense of some interface space.
if (FALSE) {
  pch <- 1
  cex <- 0.75
} else {
  pch <- "."
  cex <- 2
}
mgp <- c(2, 0.7, 0)
fullmar <- 3                           # space for margins with axes in them
## In the next sequence, note that the palette values have to match main values in 1st and 3rd arg
marPaletteProfile <- c(1, 1, fullmar, 1)
marProfile <- c(1, fullmar, fullmar, fullmar+2.5)
marPaletteTimeseries <- c(fullmar, 1, 1 , 1)
marTimeseries <- c(fullmar, fullmar, 1, fullmar+2.5)
marPaletteTS <- c(fullmar, 1, 1, 1)
marTS <- c(fullmar, fullmar, 1, fullmar+2.5)

library(shiny)
##library(shinythemes)
library(oce)
library(oceanglider)
##library(shinycssloaders)

options(oceEOS="gsw")

## Discover available gliders and their mission
baseTrial <- c("~/Dropbox/data",
               "/data",
               ".")
basedir <- ""
for (base in baseTrial) {
  dir <- paste(base, "/glider/delayedData", sep="")
  cat("trial base directory '", dir, "'\n", sep="")
  if (file.exists(dir)) {
    cat("  This trial base directory is OK.\n")
    basedir <- dir
    break
  }
}
if (basedir == "") {
  chances <- 3
  cat('None of the following is a valid base directory: "', paste(baseTrial, collapse='", "'), '"\n', sep='')
  if (!interactive())
    stop("Try running this app interactively, so you can enter a base directory.\n")
  for (chance in seq_len(chances)) {
    cat("  Type the name of a base directory (chance", chance, "of", chances, "chances)\n")
    base <- readLines(n=1)
    basedir <- paste(base, "/glider/delayedData", sep="")
    if (file.exists(basedir)) {
      cat("That is a good base directory, thanks.\n")
      break
    } else {
      cat("Sorry, '", base, "' does not contain a directory glider/delayedData\n", sep="")
      if (chance > 3)
        stop("Sorry, you only get", chances, "to specify a base filename.\n")
    }
  }
}
gliders <- list.files(basedir)
missions <- list()
for (glider in gliders) {
  missions[glider] <- list(list.files(paste(basedir, glider, "Data", sep="/")))
}

## use str(navStateCodes("seaexplorer")) to see codes
## descending          : num 100
## not_navigating      : num 105
## inflecting_downwards: num 110
## surfacing           : num 115
## at_surface          : num 116
## ascending           : num 117
## inflecting_upwards  : num 118
navStateColors <- function(navState)
{
  n <- length(navState)
  res <- rep(0L, length=n)             # will create an error for an unknown navState
  res[navState == 100] <- 1L           # descending
  res[navState == 105] <- 7L           # not_navigating
  res[navState == 110] <- 2L           # inflecting_upwards
  res[navState == 115] <- 3L           # surfacing
  res[navState == 116] <- 4L           # at_surface
  res[navState == 117] <- 5L           # ascending
  res[navState == 118] <- 6L           # inflecting_upwards
  if (any(res == 0))
    stop("a navState is not in the allowed list of 100, 105, 110, 115, 116, 117 or 118")
  res
}

navStateLegend <- function()
{
  codes <- navStateCodes("seaexplorer")
  ## legend("top", pch=20, bg="transparent", x.intersp=0.5, cex=0.8, pt.cex=1.5,
  ##        col=navStateColors(codes), legend=names(codes), ncol=length(codes))
  usr <- par("usr")
  x <- usr[2] + 0.01*(usr[2]-usr[1])
  dy <- (usr[4]-usr[3])/(1+length(codes))
  y <- usr[3] + dy/2
  for (icode in seq_along(codes)) {
    ##cat(file=stderr(), "code=", codes[[icode]], " (", names(codes)[icode], "), x=", x, ", y=", y, ", dy=", dy, "\n")
    points(x, y, pch=21, cex=1.4, bg=navStateColors(codes[icode]), xpd=NA)
    text(x, y, names(codes)[icode], xpd=NA, pos=4, cex=0.75, srt=60)
    y <- y + dy
  }
}

var <- list()
src <- ""
g <- NULL
SA <- NULL
CT <- NULL
p <- NULL
ndata <- 0
t <- NULL
maxYo <- NULL

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
                         column(2,
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
                                ## conditionalPanel(condition="input.focus == 'yo'",
                                ##                  uiOutput(outputId="previousYo")),
                                ## conditionalPanel(condition="input.focus == 'yo'",
                                ##                  uiOutput(outputId="nextYo"))
                                conditionalPanel(condition="input.focus == 'yo'",
                                                 uiOutput(outputId="flagYo"))
                                ),
                         column(2,
                                conditionalPanel(condition="output.gliderExists",
                                                 uiOutput(outputId="brushMode")))),
                fluidRow(conditionalPanel(condition="output.gliderExists",
                                          uiOutput(outputId="navState")),
                         conditionalPanel(condition="output.gliderExists",
                                          uiOutput(outputId="despikePressure"))),
                fluidRow(conditionalPanel(condition="output.gliderExists",
                                          uiOutput(outputId="status"))),
                fluidRow(conditionalPanel(condition="output.gliderExists",
                                          plotOutput("plot",
                                                     hover="hover",
                                                     click="click",
                                                     width="100%",
                                                     height="600px",
                                                     brush=brushOpts(id="brush", delay=1000, delayType="debounce", resetOnNew=!TRUE)))))


server <- function(input, output, session) {
  ## focusYo <- reactive({
  ##   if (is.null(input$focusYo)) NULL else as.numeric(input$focusYo)
  ## })
  focusYo <- reactive({
    if (is.null(input$focusYo)) NULL else as.numeric(input$focusYo)
  })

  msg <- function(...) {
    if (debug == "Yes")
      cat(file=stderr(), ...)
  }

  #' suport function for plotting and brushing
  lookIndices <- function() {
    if (input$despikePressure) {
      p <- g[["pressure"]]
      badp <- is.na(p)
      if (any(badp))
        p[badp] <- mean(p, na.rm=TRUE) # will trim later anyhow
      pressureShift <- abs(p - runmed(p, k=11))
      badPressure <- pressureShift > pressureThreshold
      if (any(badp))
        badPressure[badp] <- TRUE
    } else {
      badPressure <- rep(FALSE, ndata)
    }
    visible <- (g[["navState"]] %in% input$navState) & !badPressure
    if (input$focus == "yo")
      visible <- visible & (g[["yoNumber"]] == as.numeric(input$focusYo))
    visible <- visible & state$flag != 3
    visible
  }

  plotExists <- FALSE
  ##state <- reactiveValues(rda="", flag=NULL, focusYo="1", yoSelected=NULL, gliderExists=FALSE, usr=NULL)
  state <- reactiveValues(rda="", flag=NULL, focusYo=1, gliderExists=FALSE, usr=NULL)

  relevantRdaFiles <- function(glider=NULL, mission=NULL)
  {
    Sys.glob(paste(tolower(glider), "_", tolower(mission), "*.rda", sep=""))
  }

  # flag=1 if ok; =3 if bad

  edits <- list()

  dataName <- function() {
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
    tolower(paste0(varName(), "_", format(oce::presentTime(), "%Y-%m-%d_%H:%M:%S"), ".rda", sep=""))
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
    ##msg("output$loadRda\n")
    files <- relevantRdaFiles(input$glider, input$mission)
    if (length(files)) {
      actionButton(inputId="loadRdaAction", label="Load previous")#h5(paste0("Load ", rda)))
    }
  })

  output$saveRda <- renderUI({
    ##msg("output$saveRda\n")
    ## hr()
    actionButton(inputId="saveRda", label="Save")
  })

  output$focus <- renderUI({
    ##msg("output$focus\n")
    selectInput(inputId="focus",
                label="Focus",
                choices=c("mission", "yo"),
                selected=c("mission"))
  })

  output$plotChoice <- renderUI({
    selectInput(inputId="plotChoice",
                label="Plot",
                choices=c("p(t)", "C(t)", "S(t)", "T(t)", "TS",
                          "S profile", "T profile", "density profile", "C profile",
                          "hist(C)", "hist(S)", "hist(p)"),
                selected="p(t)")
  })

  output$plotType <- renderUI({
    selectInput(inputId="plotType", label="Plot Style", choices= c("l", "p", "o"), selected=c("p"))
  })

  output$colorBy <- renderUI({
    selectInput(inputId="colorBy",
                label="Colour by",
                choices=c("distance", "latitude", "longitude", "pressure", "temperature", "salinity", "navState", "(none)"),
                selected="distance")
  })

  output$focusYo <- renderUI({
    numericInput("focusYo",
                 ##if (is.null(maxYo)) "Yo number [enter value within 5s]" else paste("Yo number (in range 1 to ", maxYo, ") [enter value within 5s]", sep=""),
                 if (is.null(maxYo)) "Yo number" else paste("Yo number (in range 1 to ", maxYo, ")", sep=""),
                 value=if (is.null(state$focusYo)) "1" else state$focusYo)
  })

  observeEvent(focusYo, {
               msg("observeEvent(focusYo) ...\n")
               #focusYoRaw <- reactive({
               #  if (is.null(input$focusYo)) NULL else as.numeric(input$focusYo)
               #})
               #focusYo <- throttle(focusYoRaw, 5000)()
               fy <- debounce(focusYo, 5000)()
               state$focusYo <<- fy
               msg("  fy=", fy, ",  maxYo=", maxYo, "\n")
               if (!is.null(fy) && !is.null(maxYo)) {
                 if (is.finite(fy)) {
                   if (fy > maxYo) {
                     updateNumericInput(session, "focusYo", value=maxYo)
                   } else if (focusYo < 1) {
                     updateNumericInput(session, "focusYo", value=1)
                   }
                 }
               }
               msg("  after, state$focusYo=", state$focusYo, "\n")
  })

  ## output$previousYo <- renderUI({
  ##     actionButton(inputId="previousYo", label="Previous yo")
  ## })
  ##
  ## observeEvent(input$previousYo, {
  ##              msg("'Previous Yo' button clicked; state$focusYo=", state$focusYo, "\n")
  ##              if (state$focusYo > 1)
  ##                state$focusYo <<- state$focusYo - 1
  ## })

  ## output$nextYo <- renderUI({
  ##     actionButton(inputId="nextYo", label="Next yo")
  ## })

  ## observeEvent(input$nextYo, {
  ##              #msg("'Next Yo' button clicked; state$focusYo=", state$focusYo, ", maxYo=", maxYo, "\n")
  ##              if (state$focusYo < maxYo)
  ##                state$focusYo <<- state$focusYo + 1
  ## })

  output$flagYo <- renderUI({
      actionButton(inputId="flagYo", label="Flag yo")
  })

  observeEvent(input$flagYo, {
               msg("  input$focusYo=", input$focusYo, "\n")
               if (!is.null(input$focusYo)) {
                 yo <- g[["yoNumber"]]
                 bad <- yo == input$focusYo
                 oldFlag <- state$flag
                 state$flag[bad] <<- 3
                 newFlag <- state$flag
                 index <- which(newFlag != oldFlag)
                 old <- oldFlag[index]
                 new <- newFlag[index]
                 edits[[1+length(edits)]] <<- list(category=paste("delete yo", state$focusYo),
                                                   time=presentTime(),
                                                   index=index, oldFlag=old, newFlag=new)
                 msg("  updated edits; new length is ", length(edits), "; sum(bad)=", sum(bad), "\n", sep="")
                 ##state$focusYo <<- NULL
               }
  })


  output$brushMode <- renderUI({
      selectInput(inputId="brushMode",
                  label="Brush mode",
                  choices=c("flag", "highlight [broken]", "zoom [broken]"),
                  selected="flag")
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
        msg("state$usr=", paste(state$usr,collapse=" "), "\n")
        msg("x=", x, ", y=", y, "\n")
        msg("head(SA)==", paste(head(SA), collapse=" "), "\n")
        msg("head(CT)==", paste(head(CT), collapse=" "), "\n")
        dist[flagged] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
        disti <- which.min(dist)
        d <- g[["payload1"]][disti,]
        res <- sprintf("yo=%d p=%.1f SA=%.4f CT=%.4f navState=%d (%.3fE %.3fN %s)\n",
                       d$yoNumber, d$pressure, d$SA, d$CT, d$navState,
                       d$longitude, d$latitude, format(d$time, "%Y-%m-%dT%H:%M:%S"))
      }
    }
    ## res <- paste0(res, " brushMode=", input$brushMode)
    res
  })

  observeEvent(input$debug, {
               ##cat(file=stderr(), "input$debug=", input$debug, "\n")
               debug <<- input$debug
  })


  observeEvent(input$click, {
               msg("input$click **IGNORED**\n")
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
               ###msg("readData...\n")
               dir <- dataName()
               ###msg("  about to read files in the '", dir, "' directory\n", sep="")
               nfiles <- length(list.files(dir, "pld1.raw"))
               withProgress(message=paste0("Reading ", nfiles, " pld1.raw files in '", dir, "'"),
                            value=0,
                            { g <<- try(read.glider.seaexplorer.delayed(dir)) }
                            )
               if (inherits(g, "try-error")) {
                 showModal(modalDialog("", paste0("no .pld1. files in directory '", dir, "'")))
               } else {
                 state$flag <- rep(1, length(g[["pressure"]]))
                 ###showModal(modalDialog("", "Reading of pld1 files is complete. Next, select a plot type, colour scheme, navState limitations, etc. You may save your work at any time, for later loading by timestamp.", easyClose=TRUE))
               }
               g@data$payload1[["SA"]] <<- g[["SA"]]
               g@data$payload1[["CT"]] <<- g[["CT"]]
               g@data$payload1[["sigma0"]] <<- g[["sigma0"]]
               g@data$payload1[["spiciness"]] <<- g[["spiciness"]]
               g@data$payload1[["distance"]] <<- oce::geodDist(g[["longitude"]], g[["latitude"]], alongPath=FALSE)
               g@data$payload1[["navStateColor"]] <<- navStateColors(g[["navState"]])
               SA <<- g@data$payload1[["SA"]]
               CT <<- g@data$payload1[["CT"]]
               p <<- g@data$payload1[["pressure"]]
               ndata <<- length(p)
               t <<- as.numeric(g[["time"]]) # in seconds, for hover operations
               maxYo <<- max(g@data$payload[["yoNumber"]], na.rm=TRUE)
               state$gliderExists <- TRUE
               ###msg("... done reading data; got payload1 data of dimension ", paste(dim(g@data$payload1), collapse="X"), "\n")
               ###msg("maxYo=", maxYo, " after reading raw data\n")
  })

  observeEvent(input$loadRdaAction, {
               ###msg("loadRda...\n")
               filename <- paste(tolower(input$glider), "_", tolower(input$mission),
                                 "_", input$rdaInputFile, ".rda", sep="")
               ###msg("  load from '", filename, "' ..", sep="")
               withProgress(message=paste0("Loading '", filename, "'"), value=0, { load(filename) })
               g <<- g
               SA <<- g[["SA"]]
               CT <<- g[["CT"]]
               p <<- g[["pressure"]]
               ndata <<- length(p)
               maxYo <<- max(g[["yoNumber"]], na.rm=TRUE)
               t <<- as.numeric(g[["time"]]) # in seconds, for hover operations
               state$flag <- g[["pressureFlag"]]
               ###msg(". done\n")
               ###msg("maxYo=", maxYo, " after loading rda\n")
               state$rda <- filename
               state$gliderExists <- TRUE
  })

  observeEvent(input$saveRda, {
               ###msg("saveRda...\n")
               rda <- rdaName()
               ###msg("  save 'g' and 'edits' to '", rda, "' ...\n", sep="")
               visible <- g[["navState"]] %in% input$navState
               g@metadata$flags$payload1$pressure <<- ifelse(visible, state$flag, 3)
               edits[[1+length(edits)]] <<- list(category="navState", time=presentTime(), bad=!visible)
               ###msg("  updated edits; new length is ", length(edits), "; sum(!visible)=", sum(!visible), "\n", sep="")
               mission <- input$mission
               glider <- input$glider
               sourceDirectory <- dataName()
               withProgress(message=paste0("Saving '", rda, "'"),
                            value=0,
                            { save(glider, mission, sourceDirectory, edits, g, file=rda) }
                            )
               ###msg("  ... done\n", sep="")
  })

  output$plot <- renderPlot({
    msg("\nplot with input: focus='", input$focus, "'",
        ", plotChoice='", input$plotChoice, "'",
        ", despikePressure=", input$despikePressure,
        ", selectYo='", input$selectYo, "'",
        ", state$focusYo=", state$focusYo,
        ", input$focusYo=", input$focusYo,
        ", plotType=", input$plotType,
        "\n", sep="")
    if (!is.null(g))  {
      n <- length(g[["pressure"]])
      look <- lookIndices()
      gg <- g
      gg@data$payload1 <- g@data$payload1[look, ]
      msg("input$plotChoice: '", input$plotChoice, "'\n", sep="")
      ##msg("** sum(look)=", sum(look))
      if (sum(look)) {
        if (length(grep("\\(t\\)$", input$plotChoice))) {
          msg("* a time-series plot (input$plotChoice is '", input$plotChoice, "')\n", sep="")
          if (input$plotChoice == "p(t)") {
            dataName <- "pressure"
            axisName <- "p"
          } else if (input$plotChoice == "C(t)") {
            dataName <- "conductivity"
            axisName <- "conductivity S/m"
          } else if (input$plotChoice == "S(t)") {
            dataName <- "SA"
            axisName <- "absolute salinity"
          } else if (input$plotChoice == "T(t)") {
            dataName <- "CT"
            axisName <- "conservative temperature"
          } else {
            stop("programmer error: unhandled time-series name '", input$plotChoice, "'")
          }
          x <- g@data$payload1[look, "time"]
          y <- g@data$payload1[look, dataName]
          ## msg("LENGTH of x=", length(x), "\n")
          ## msg("LENGTH of y=", length(y), "\n")
          msg("time-series plot range of x (time):", paste(range(x, na.rm=TRUE), collapse=" to "), "\n")
          msg("time-series plot range of y:", paste(range(y, na.rm=TRUE), collapse=" to "), "\n")
          ylim <- range(y, na.rm=TRUE)
          if (input$plotChoice == "p(t)") 
            ylim <- rev(ylim)
          ylab <- resizableLabel(axisName)
          if (input$colorBy != "(none)") {
            if (input$colorBy == "navState") {
              timing <- system.time({
                oce.plot.ts(x, y, ylim=ylim,
                            type=input$plotType,
                            col=gg[["navStateColor"]],
                            mar=marTimeseries,
                            ylab=ylab, pch=pch, cex=cex, flipy=input$plotChoice=="p(t)")
              })
              msg(dataName, " time-series plot (coloured by navState) took elapsed time ", timing[3], "s\n", sep="")
              navStateLegend()
            } else {
              cm <- colormap(g[[input$colorBy]][look])
              par(mar=marPaletteTimeseries, mgp=mgp)
              drawPalette(colormap=cm, zlab=input$colorBy)
                                        #omar <- par("mar")
                                        #par(mar=c(3, 3, 2, 5.5), mgp=c(2, 0.7, 0))
              timing <- system.time({
                oce.plot.ts(x, y,
                            type=input$plotType,
                            col=cm$zcol,
                            mar=marTimeseries,
                            ylab=ylab, pch=pch, cex=cex, flipy=input$plotChoice=="p(t)")
              })
              msg(input$plotType, " time-series plot (coloured by ", input$colorBy, ") took elapsed time ", timing[3], "s\n", sep="")
            }
          } else {
            timing <- system.time({
              oce.plot.ts(x, y,
                          type=input$plotType,
                          mar=marTimeseries,
                          ylab=ylab, pch=pch, cex=cex, flipy=input$plotChoice=="p(t)")
            })
            msg(dataName, " time-series plot (not coloured) took elapsed time ", timing[3], "s\n", sep="")
          }
          plotExists <<- TRUE
        } else if (input$plotChoice == "TS") {
          gg <- g
          gg@data$payload1 <- g@data$payload1[look, ]
          if (input$colorBy != "(none)") {
            if (input$colorBy == "navState") {
              ##TESTING if (FALSE) {
              ##TESTING   ## timing test with random numbers
              ##TESTING   n <- length(look)
              ##TESTING   x <- rnorm(n)
              ##TESTING   y <- rnorm(n)
              ##TESTING   cm <- colormap(x^2+y^2)
              ##TESTING   timing <- system.time({
              ##TESTING     plot(x, y, col=cm$zcol, cex=cex, pch=pch)
              ##TESTING   })
              ##TESTING   msg("plot() with ", n, " random points) took elapsed time ", timing[3], "s\n", sep="")
              ##TESTING } else {
              ## Actual plot
              timing <- system.time({
                plotTS(gg, pch=pch, cex=cex, col=gg[["navStateColor"]], mar=marTS, type=input$plotType)
              })
              msg("plotTS (coloured by navState) took elapsed time ", timing[3], "s\n", sep="")
              ##TESTING }
              navStateLegend()
            } else {
              cm <- colormap(gg[[input$colorBy]])
              par(mar=marPaletteTS, mgp=mgp)
              drawPalette(colormap=cm, zlab=input$colorBy)
              timing <- system.time({
                plotTS(gg, pch=pch, cex=cex, col=cm$zcol, mar=marTS, type=input$plotType)
              })
              msg("plotTS (coloured by ", input$colorBy, ") took elapsed time ", timing[3], "s\n", sep="")
            }
          } else {
            timing <- system.time({
              plotTS(gg, pch=pch, cex=cex, mar=marTS, type=input$plotType)
            })
            msg("plotTS (with no colours) took elapsed time ", timing[3], "s\n", sep="")
          }
          plotExists <<- TRUE
        } else if (length(grep(" profile$", input$plotChoice))) {
          if ("S profile" == input$plotChoice) {
            dataName <- "SA"
            axisName <- "absolute salinity"
          } else if ("T profile" == input$plotChoice) {
            dataName <- "CT"
            axisName <- "conservative temperature"
          } else if ("density profile" == input$plotChoice) {
            dataName <- "sigma0"
            axisName <- "sigma0"
          } else if ("C profile" == input$plotChoice) {
            dataName <- "conductivity"
            axisName <- "conductivity S/m"
          } else {
            stop("programmer error: unhandled profile name '", input$plotChoice, "'")
          }
          x <- g@data$payload1[look, dataName]
          y <- g@data$payload1[look, "pressure"]
          ylim <- rev(range(y, na.rm=TRUE))
          omar <- par("mar")
          if (input$colorBy != "(none)") {
            if (input$colorBy == "navState") {
              par(mar=marProfile, mgp=mgp)
              plot(x, y, ylim=ylim,
                   type=input$plotType, pch=pch, cex=cex, col=g@data$payload1[look, "navStateColor"],
                   xlab="", ylab=resizableLabel("p"), axes=FALSE)
              navStateLegend() # FIXME: put on RHS
            } else {
              cm <- colormap(g@data$payload1[look, input$colorBy])
              par(mar=marPaletteProfile, mgp=mgp)
              drawPalette(colormap=cm, zlab=input$colorBy)
              par(mar=marProfile, mgp=mgp)
              plot(x, y, ylim=ylim,
                   type=input$plotType, pch=pch, cex=cex, col=cm$zcol,
                   xlab="", ylab=resizableLabel("p"), axes=FALSE)
            }
          } else {
            par(mar=marProfile, mgp=mgp)
            plot(x, y, ylim=ylim,
                 type=input$plotType, pch=pch, cex=cex,
                 xlab="", ylab=resizableLabel("p"), axes=FALSE)
          }
          box()
          axis(2)
          axis(3)
          mtext(resizableLabel(axisName), side=3, line=2)
          par(mar=omar)
        } else if (input$plotChoice == "hist(p)") {
          p <- g[["pressure"]]
          hist(p[!flagged & visible], breaks=100, main="p for whole dataset, trimmed to unflagged values")
        } else if (input$plotChoice == "hist(C)") {
          C <- g[["conductivity"]]
          hist(C[!flagged & visible], breaks=100, main="C for whole dataset, trimmed to unflagged values")
          msg("summary(C):", summary(C), "\n")
          msg("summary(C[!flagged]):", summary(C[!flagged]), "\n")
        } else if (input$plotChoice == "hist(S)") {
          SA <- g[["SA"]]
          hist(SA[!flagged & visible], breaks=100, main="SA for whole dataset, trimmed to unflagged values")
          msg("summary(SA):", summary(SA), "\n")
          msg("summary(SA[!flagged]):", summary(SA[!flagged]), "\n")
        } else {
          stop("unknown plot type (internal coding error)\n")
        }
        plotExists <<- TRUE
        state$usr <<- par("usr")
      } else {
        plot(c(0,1), c(0,1), xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, "No data to plot")
        state$usr <<- NULL
      }
    }
  }, type="cairo", antialias="none", res=200, pointsize=5) # renderPlot.
  ## g21 m51 TS/latitude res=200 takes ??1.9s elapsed time (DK home machine)
  ## g21 m51 TS/latitude res=100 takes 1.9s elapsed time (DK home machine)

  outputOptions(output, "gliderExists", suspendWhenHidden = FALSE)

  output$despikePressure <- renderUI({
    checkboxInput(inputId="despikePressure", label="Despike pressure")
  })

} # server

shinyApp(ui=ui, server=server)

