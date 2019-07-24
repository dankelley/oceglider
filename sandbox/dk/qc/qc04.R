## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2

debugFlag <- TRUE                      # a button lets user set this on and off
version <- "0.4"
pressureThreshold <- 0.5
## * Data-quality Flag Scheme
##
##     name    "IOOS"
##     mapping list(pass=1, not_evaluated=2, suspect=3, fail=4, missing=9)
badFlagValue <- 4                      # "fail": flagged as bad
initialFlagValue <- 2                  # "not evaluated": the default after reading data

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
fullmar <- 3.3                         # space for margins with axes in them
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
baseTrial <- c("~/Dropbox/data", "/data", "~/data")
basedir <- ""
for (base in baseTrial) {
  dir <- paste(base, "/glider/delayedData", sep="")
  cat("Directory '", dir, "' ", sep="")
  if (file.exists(dir)) {
    cat("holds a glider/delayedData subdirectory, and will be used to construct the glider and mission menus.\n")
    basedir <- dir
    break
  } else {
    cat("does not hold a glider/delayedData subdirectory.\n")
  }
}
if (basedir == "") {
  chances <- 3
  stop('None of the following is a valid base directory: "', paste(baseTrial, collapse='", "'), '"\n', sep='',
       call.=FALSE)
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
powerOnThreshold <- 60 # if no samples during this number of seconds, assume power was off
powerOffIndex <- NULL

ui <- fluidPage(tags$style(HTML("body {font-family: 'Arial'; font-size: 12px; margin-left:1ex} hr {size: '50'}")),
                fluidRow(column(2, checkboxInput("debug", "Debug", value=TRUE)),
                         column(2, checkboxInput("instructions", "Show Instructions", value=FALSE))),
                conditionalPanel(condition="input.instructions", fluidRow(includeMarkdown("qc03_help.md"))),
                fluidRow(column(2,
                                uiOutput(outputId="glider"),
                                uiOutput(outputId="mission"),
                                uiOutput(outputId="read")),
                         column(2,
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
                                                 uiOutput(outputId="flagYo"))
                                )
                         ),
                fluidRow(column(2,
                                conditionalPanel(condition="output.gliderExists",
                                                 uiOutput(outputId="hideInitialYos"))),
                         column(2,
                                conditionalPanel(condition="output.gliderExists",
                                                 uiOutput(outputId="hideTop"))),
                         column(2,
                                conditionalPanel(condition="output.gliderExists",
                                                 uiOutput(outputId="hideAfterPowerOn")))
                         ),
                fluidRow(conditionalPanel(condition="output.gliderExists",
                                          uiOutput(outputId="despikePressure")),
                         conditionalPanel(condition="output.gliderExists",
                                          uiOutput(outputId="navState"))
                         ),
                fluidRow(conditionalPanel(condition="output.gliderExists",
                                          uiOutput(outputId="status"))
                ),
                fluidRow(conditionalPanel(condition="output.gliderExists",
                                          plotOutput("plot",
                                                     hover="hover",
                                                     ##click="click",
                                                     dblclick="dblclick",
                                                     width="100%",
                                                     height="500px",
                                                     brush=brushOpts(id="brush",
                                                                     delay=2000,
                                                                     delayType="debounce",
                                                                     resetOnNew=TRUE)))
                )
                )

server <- function(input, output, session) {
  state <- reactiveValues(rda="",
                          flag=NULL,
                          focusYo=1,
                          gliderExists=FALSE,
                          usr=NULL,
                          yoDblclicked=NULL,
                          hideAfterPowerOn=0
                          )

  saveEditEvent <- function(category, bad)
  {
    msg("saveEditEvent with sum(bad)=", sum(bad), "\n")
    oldFlag <- state$flag
    msg(" sum(state$flag==3)=", sum(state$flag==3), " before setting some to bad\n")
    state$flag[bad] <<- badFlagValue
    msg(" sum(state$flag==3)=", sum(state$flag==3), " after setting some to bad\n")
    newFlag <- state$flag
    changedIndex <- which(newFlag != oldFlag)
    if (length(changedIndex) > 0) {
      ## Ignore null edits (which result from some event-handling magic, I suppose)
      edits[[1+length(edits)]] <<- list(category=category,
                                        time=presentTime(),
                                        changedIndex=changedIndex,
                                        oldFlag=oldFlag[changedIndex],
                                        newFlag=newFlag[changedIndex])
    }
  }

  focusYo <- reactive({
    if (is.null(input$focusYo)) NULL else as.numeric(input$focusYo)
  })

  hideAfterPowerOn <- reactive({
    if (is.null(input$hideAfterPowerOn)) 0 else as.numeric(input$hideAfterPowerOn)
  })

  msg <- function(...) {
    if (debugFlag)
      cat(file=stderr(), ...)
  }

  #' suport function for plotting and brushing, used in several spots
  visibleIndices <- function(message="") {
    msg("  visibleIndices(", message, ") {\n", sep="")
    msg("    input$focus='", input$focus,"', input$focusYo=", input$focusYo, ", ndata=",ndata, "\n", sep="")
    ## 1. start with data being displayed
    visible <- if (input$focus == "yo") g[["yoNumber"]] == as.numeric(input$focusYo) else rep(TRUE, ndata)
    msg("    windowed by focus, sum(visible) = ", sum(visible), "\n")
    ## 2. start with data in desired navStage
    visible <- visible & (g[["navState"]] %in% input$navState)
    msg("    after select navState, sum(visible) = ", sum(visible), "\n")
    ## 3. remove any pressure spikes
    if (input$despikePressure) {
      p <- g[["pressure"]]
      badp <- is.na(p)
      if (any(badp))
        p[badp] <- mean(p, na.rm=TRUE) # will trim later anyhow
      pressureShift <- abs(p - runmed(p, k=11))
      badPressure <- pressureShift > pressureThreshold
      if (any(badp))
        badPressure[badp] <- TRUE
      visible <- visible & !badPressure
    }
    msg("    after despikePressure, sum(visible) = ", sum(visible), "\n")
    ## 4. remove pressures less than a specified limit
    if (input$hideTop > 0) {
      tooNearSurface <- p < input$hideTop
      visible <- visible & !tooNearSurface
    }
    msg("    after hideTop, sum(visible) = ", sum(visible), "\n")
    ## 5. hide initial yos
    if (input$hideInitialYos > 0)
      visible <- visible & (g[["yoNumber"]] > as.numeric(input$hideInitialYos))
    msg("    after hideInitialYos, sum(visible) = ", sum(visible), "\n")
    ## 5. ignore for some time after powerup
    hapu <- debounce(hideAfterPowerOn, 2000)()
    poweringOn <- g[["tSincePowerOn"]] < hapu
    if (file.exists("stop")) browser()
    ## msg(vectorShow(t))
    ## msg(vectorShow(g[["tSincePowerOn"]]))
    ## msg(vectorShow(g[["tSincePowerOn"]]<hapu))
    visible <- visible & !poweringOn
    msg("    after hideAfterPowerup (first ", hapu, "s), sum(visible) = ", sum(visible), "\n")
    ## 6. ignore already-flagged data
    visible <- visible & (state$flag != badFlagValue)
    msg("    after flagging already-flagged data, sum(visible) = ", sum(visible), "\n")
    msg("  }  # visibleIndices(", message, ")\n", sep="")
    ## DEVELOPER: put new tests here, and be sure to use msg()
    visible
  }

  plotExists <- FALSE

  relevantRdaFiles <- function(glider=NULL, mission=NULL)
  {
    Sys.glob(paste(tolower(glider), "_", tolower(mission), "*.rda", sep=""))
  }

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
    #tolower(paste0(varName(), "_", format(oce::presentTime(), "%Y-%m-%d_%H:%M:%S"), ".rda", sep=""))
    tolower(paste0(varName(), "_", format(oce::presentTime(), "%Y%m%d_%H%M"), ".rda", sep=""))
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
                ## BOOKMARK_plot_type_1_of_4: note that 2, 3 and 4 must align with this
                choices=c("p(t)", "C(t)", "S(t)", "T(t)", "tSincePowerOn(t)", "TS",
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
                choices=c("distance", "latitude", "longitude", "pressure", "temperature", "salinity", "navState", "tSincePowerOn", "(none)"),
                selected="distance")
  })

  output$focusYo <- renderUI({
    numericInput("focusYo",
                 ##if (is.null(maxYo)) "Yo number [enter value within 5s]" else paste("Yo number (in range 1 to ", maxYo, ") [enter value within 5s]", sep=""),
                 if (is.null(maxYo)) "Yo number" else paste("Yo number (in range 1 to ", maxYo, ")", sep=""),
                 value=if (is.null(state$yoDblclicked)) "1" else state$yoDblclicked)
  })

  observeEvent(focusYo, {
               msg("observeEvent(focusYo) ...\n")
               ## fy <- throttle(focusYoRaw, 5000)()
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

  observeEvent(hideAfterPowerOn, {
               msg("+++ observeEvent(hideAfterPowerOn) ...\n")
               state$hideAfterPowerOn <<- debounce(hideAfterPowerOn, 5000)()
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
                 saveEditEvent(paste0("flag yo ", input$focusYo), bad)
               }
  })


  ###old output$brushMode <- renderUI({
  ###old     selectInput(inputId="brushMode",
  ###old                 label="Brush mode",
  ###old                 choices=c("flag", "highlight [broken]", "zoom [broken]"),
  ###old                 selected="flag")
  ###old })

  output$hideInitialYos <- renderUI({
    sliderInput("hideInitialYos", h6("Hide initial yos"), min=0, max=10, value=1)
  })

  output$hideTop <- renderUI({
    sliderInput("hideTop", h6("Hide top data [m]"), min=0, max=15, value=0)
  })

  output$hideAfterPowerOn <- renderUI({
    sliderInput("hideAfterPowerOn", h6("Hide after power-on [s]"), min=0, max=60, value=0)
  })

  output$despikePressure <- renderUI({
    checkboxInput(inputId="despikePressure", label="Despike pressure")
  })

  output$navState <- renderUI({
    ns <- navStateCodes("seaexplorer")
    checkboxGroupInput(inputId="navState", label="Show navState",
                       choices=ns, selected=ns, inline=TRUE)
  })

  output$status <- renderText({
    hoverx <- input$hover$x
    hovery <- input$hover$y
    res <- if (is.null(g)) "Status: no glider exists. Please read data or load previous analysis." else paste(input$glider, input$mission)
    res <- "(Move mouse into plot window to see properties)"
    if (!is.null(hoverx) && plotExists) {
      ## BOOKMARK_plot_type_2_of_4: note that 1, 3 and 4 must align with this
      if (input$plotChoice == "p(t)") {
        x <- as.numeric(g[["time"]])
        y <- g[["pressure"]]
      } else if (input$plotChoice == "C(t)") {
        x <- as.numeric(g[["time"]])
        y <- g[["conductivity"]]
      } else if (input$plotChoice == "S(t)") {
        x <- as.numeric(g[["time"]])
        y <- g[["SA"]]
      } else if (input$plotChoice == "T(t)") {
        x <- as.numeric(g[["time"]])
        y <- g[["CT"]]
      } else if (input$plotChoice == "tSincePowerOn(t)") {
        x <- as.numeric(g[["time"]])
        y <- x - g[["tSincePowerOn"]]
      } else if (input$plotChoice == "TS") {
        x <- g[["SA"]]
        y <- g[["CT"]]
      } else if (input$plotChoice == "S profile") {
        x <- g[["SA"]]
        y <- g[["pressure"]]
      } else if (input$plotChoice == "T profile") {
        x <- g[["CT"]]
        y <- g[["pressure"]]
      } else if (input$plotChoice == "density profile") {
        x <- g[["sigma0"]]
        y <- g[["pressure"]]
      } else if (input$plotChoice == "C profile") {
        x <- g[["conductivity"]]
        y <- g[["pressure"]]
      } else if (length(grep("^hist", input$plotChoice))) {
        return("(Status line is unavailable for histogram plots.)")
      }
      dist <- sqrt(((hoverx-x)/(state$usr[2]-state$usr[1]))^2 + ((hovery-y)/(state$usr[4]-state$usr[3]))^2)
      visible <- visibleIndices()
      dist[!visible] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
      disti <- which.min(dist)
      d <- g[["payload1"]][disti,]
      res <- sprintf("yo=%d p=%.1f SA=%.4f CT=%.4f navState=%d (%.3fE %.3fN %s)\n",
                     d$yoNumber, d$pressure, d$SA, d$CT, d$navState,
                     d$longitude, d$latitude, format(d$time, "%Y-%m-%dT%H:%M:%S"))
      ##res <- paste0(res, "[hide initial ", input$hideInitialYos, " yos] ")
      ##res <- paste0(res, "[hide top ", input$hideTop, "m] ")
      res <- paste0(res, "[hide ", state$hideAfterPowerOn, "s after powerup] ")
      res <- paste0(res, "[dblclicked yo=", state$yoDblclicked, "]")
    }
    res
  })

  observeEvent(input$debug, {
               if (!is.null(input$debug))
                 debugFlag <<- input$debug
  })

  observeEvent(input$dblclick, {
               dblclickx <- input$dblclick$x
               dblclicky <- input$dblclick$y
               msg("input$dblclick$x=", dblclickx," y=", dblclicky,"\n")
               if (!is.null(dblclickx) && plotExists) {
                 ## BOOKMARK_plot_type_3_of_4: note that 1, 2 and 4 must align with this
                 if (input$plotChoice == "p(t)") {
                   x <- as.numeric(g[["time"]])
                   y <- g[["pressure"]]
                 } else if (input$plotChoice == "C(t)") {
                   x <- as.numeric(g[["time"]])
                   y <- g[["conductivity"]]
                 } else if (input$plotChoice == "S(t)") {
                   x <- as.numeric(g[["time"]])
                   y <- g[["SA"]]
                 } else if (input$plotChoice == "T(t)") {
                   x <- as.numeric(g[["time"]])
                   y <- g[["CT"]]
                 } else if (input$plotChoice == "tSincePowerOn(t)") {
                   x <- as.numeric(g[["time"]])
                   y <- x - g[["tSincePowerOn"]]
                 } else if (input$plotChoice == "TS") {
                   x <- g[["SA"]]
                   y <- g[["CT"]]
                 } else if (input$plotChoice == "S profile") {
                   x <- g[["SA"]]
                   y <- g[["pressure"]]
                 } else if (input$plotChoice == "T profile") {
                   x <- g[["CT"]]
                   y <- g[["pressure"]]
                 } else if (input$plotChoice == "density profile") {
                   x <- g[["sigma0"]]
                   y <- g[["pressure"]]
                 } else if (input$plotChoice == "C profile") {
                   x <- g[["conductivity"]]
                   y <- g[["pressure"]]
                 } else if (length(grep("^hist", input$plotChoice))) {
                   return("(Status line is unavailable for histogram plots.)")
                 }
                 dist <- sqrt(((dblclickx-x)/(state$usr[2]-state$usr[1]))^2 + ((dblclicky-y)/(state$usr[4]-state$usr[3]))^2)
                 visible <- visibleIndices()
                 dist[!visible] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
                 disti <- which.min(dist)
                 d <- g[["payload1"]][disti,]
                 msg("yo=", d$yoNumber, "\n")
                 state$yoDblclicked <<- d$yoNumber
               }
  })

  observeEvent(input$brush, {
               msg("input$brush ...\n")
               xmin <- input$brush$xmin
               xmax <- input$brush$xmax
               ymin <- input$brush$ymin
               ymax <- input$brush$ymax
               ## msg("  xmin=", xmin, ", xmax=", xmax, "\n", sep="")
               ## msg("  ymin=", ymin, ", ymax=", ymax, "\n", sep="")
               bad <- state$flag == 3
               msg("  sum(bad)=", sum(bad), " of ", length(bad), " [based on present state$flag]\n", sep="")
               ## BOOKMARK_plot_type_4_of_4: note that 1, 2 and 3 must align with this
               if (input$plotChoice == "p(t)") {
                 x <- as.numeric(g[["time"]])
                 y <- g[["pressure"]]
               } else if (input$plotChoice == "C(t)") {
                 x <- as.numeric(g[["time"]])
                 y <- g[["conductivity"]]
               } else if (input$plotChoice == "S(t)") {
                 x <- as.numeric(g[["time"]])
                 y <- g[["SA"]]
                 msg("length(x)=", length(x), "\n")
               } else if (input$plotChoice == "T(t)") {
                 x <- as.numeric(g[["time"]])
                 y <- g[["CT"]]
               } else if (input$plotChoice == "tSincePowerOn(t)") {
                 x <- as.numeric(g[["time"]])
                 y <- x - g[["tSincePowerOn"]]
               } else if (input$plotChoice == "TS") {
                 x <- g[["SA"]]
                 y <- g[["CT"]]
               } else if (input$plotChoice == "S profile") {
                 x <- g[["SA"]]
                 y <- g[["pressure"]]
               } else if (input$plotChoice == "T profile") {
                 x <- g[["CT"]]
                 y <- g[["pressure"]]
               } else if (input$plotChoice == "density profile") {
                 x <- g[["sigma0"]]
                 y <- g[["pressure"]]
               } else if (input$plotChoice == "C profile") {
                 x <- g[["conductivity"]]
                 y <- g[["pressure"]]
               } else if (input$plotChoice == "hist(C)") {
                 x <- g[["conductivity"]]
                 bad <- xmin <= x & x <= xmax
                 bad[is.na(bad)] <- TRUE
                 bad <- bad & visibleIndices() # we do not invalidate data not in the present view.
                 saveEditEvent("brush hist(C)", bad)
                 return()
               } else if (input$plotChoice == "hist(S)") {
                 x <- g[["SA"]]
                 bad <- xmin <= x & x <= xmax
                 bad[is.na(bad)] <- TRUE
                 bad <- bad & visibleIndices() # we do not invalidate data not in the present view.
                 saveEditEvent("brush hist(S)", bad)
                 return()
               } else if (input$plotChoice == "hist(p)") {
                 x <- g[["pressure"]]
                 bad <- xmin <= x & x <= xmax
                 bad[is.na(bad)] <- TRUE
                 bad <- bad & visibleIndices() # we do not invalidate data not in the present view.
                 saveEditEvent("brush hist(p)", bad)
                 return()
               } else {
                 stop("programming error: brushing for plot type '", input$plotChoice, "' is not coded yet")
               }
               bad <- ifelse(is.na(x) | is.na(y), TRUE, xmin <= x & x <= xmax & ymin <= y & y <= ymax)
               msg("  sum(bad)=", sum(bad), ", length(bad)=", length(bad), " [after in-box test]\n")
               bad[is.na(bad)] <- TRUE
               msg("  sum(bad)=", sum(bad), ", length(bad)=", length(bad), " [after changing NA to bad]\n")
               bad <- bad & visibleIndices(paste("brush", input$plotChoice)) # restrict to relevant data
               msg("  sum(bad)=", sum(bad), ", length(bad)=", length(bad), " [after restriction to visible]\n")
               saveEditEvent(paste0("brush ", input$plotChoice), bad)
  })

  observeEvent(input$readData, {
               ###msg("readData...\n")
               dir <- dataName()
               ###msg("  about to read files in the '", dir, "' directory\n", sep="")
               nfiles <- length(list.files(dir, "pld1.raw"))
               withProgress(message=paste0("Reading ", nfiles, " pld1.raw files in '", dir, "'"),
                            value=0,
                            {
                              g <<- try(read.glider.seaexplorer.delayed(dir))
                            }
                            )
               if (inherits(g, "try-error")) {
                 showModal(modalDialog("", paste0("no .pld1. files in directory '", dir, "'")))
                 stop()
               }
               p <<- g@data$payload1[["pressure"]]
               ndata <<- length(p)
               ## FIXME why save SA,CT into payload1 if we have global?
               g@data$payload1[["SA"]] <<- g[["SA"]]
               g@data$payload1[["CT"]] <<- g[["CT"]]
               g@data$payload1[["sigma0"]] <<- g[["sigma0"]]
               g@data$payload1[["spiciness"]] <<- g[["spiciness"]]
               g@data$payload1[["distance"]] <<- oce::geodDist(g[["longitude"]], g[["latitude"]], alongPath=FALSE)
               g@data$payload1[["navStateColor"]] <<- navStateColors(g[["navState"]])
               SA <<- g@data$payload1[["SA"]]
               CT <<- g@data$payload1[["CT"]]
               state$flag <<- rep(initialFlagValue, ndata)
               ## Will use a single flag
               g@metadata$flags$payload1 <<- list(overall=state$flag)
               t <<- as.numeric(g[["time"]])
               dt <- diff(t)
               ## Compute time since power-on.  This takes just 0.03s elapsed time, so
               ## maybe we ought to do it when the slider is adjusted, instead of saving
               ## it globally. However, I like the idea of doing it here, because
               ## we might want to save it to the rda, for later use.
               poi <- 1 + which(dt > powerOnThreshold) # power-on index
               npoi <- length(poi)
               lastPowerOn <- rep(NA, ndata)
               lastPowerOn[seq(1L, poi[1]-1)] <- t[1]
               for (i in seq_len(npoi-1))
                 lastPowerOn[seq(poi[i], poi[i+1])] <- t[poi[i]]
               lastPowerOn[seq(poi[npoi], ndata)] <- t[poi[npoi]]
               g@data$payload1[["tSincePowerOn"]] <<- t - lastPowerOn
               ## par(mfrow=c(2,1),mar=c(3,3,1,1), mgp=c(2,0.7,0))
               ## look <- seq(1, 20e3)
               ## plot(t[look], (t-tSincePowerOn)[look], type="p", pch=20, cex=1/3)
               ## abline(h=10, col=2)
               ## look <- ndata + seq(-20e3, 0)
               ## plot(t[look], (t-tSincePowerOn)[look], type="p", pch=20, cex=1/3)
               ## abline(h=10, col=2)
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
               state$flag <<- g@metadata$flags$payload1$overall
               ###msg(". done\n")
               ###msg("maxYo=", maxYo, " after loading rda\n")
               state$rda <<- filename
               state$gliderExists <<- TRUE
  })

  observeEvent(input$saveRda, {
               ###msg("saveRda...\n")
               rda <- rdaName()
               ###msg("  save 'g' and 'edits' to '", rda, "' ...\n", sep="")
               visible <- g[["navState"]] %in% input$navState
               g@metadata$flags$payload1$pressure <<- ifelse(visible, state$flag, 3)
               saveEditEvent("on file save, ignoring hidden navState", !visible)
               ###msg("  updated edits; new length is ", length(edits), "; sum(!visible)=", sum(!visible), "\n", sep="")
               mission <- input$mission
               glider <- input$glider
               sourceDirectory <- dataName()
               ## Will use a single flag
               g@metadata$flags$payload1 <<- list(overall=state$flag)
               msg("saving with sum(2==g@metadata$flags$payload1$overall)/length(same) = ",
                   sum(2==g@metadata$flags$payload1$overall)/length(g@metadata$flags$payload1$overall), "\n")
               withProgress(message=paste0("Saving '", rda, "'"),
                            value=0,
                            {
                              save(g, glider, mission, sourceDirectory, edits,
                                   file=rda)
                            }
                            )
               msg("  ... finished saving to file '", rda, "'\n", sep="")
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
      look <- visibleIndices()
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
          } else if (input$plotChoice == "tSincePowerOn(t)") {
            dataName <- "tSincePowerOn"
            axisName <- "Time since powerup [s]"
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
          p <- gg[["pressure"]]
          hist(p, breaks=100, main="p for whole dataset, trimmed to unflagged values")
        } else if (input$plotChoice == "hist(C)") {
          C <- gg[["conductivity"]]
          hist(C, breaks=100, main="C for whole dataset, trimmed to unflagged values")
        } else if (input$plotChoice == "hist(S)") {
          SA <- gg[["SA"]]
          hist(SA, breaks=100, main="SA for whole dataset, trimmed to unflagged values")
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
  #}, type="Xlib", antialias="none", res=200, pointsize=5) # renderPlot.
  ## g21 m51 TS/latitude res=200 takes ??1.9s elapsed time (DK home machine)
  ## g21 m51 TS/latitude res=100 takes 1.9s elapsed time (DK home machine)

  outputOptions(output, "gliderExists", suspendWhenHidden = FALSE)


} # server

shinyApp(ui=ui, server=server)

