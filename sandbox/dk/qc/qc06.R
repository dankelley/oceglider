## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2

appName <- "glider QC"
appVersion <- "0.6"
debugFlag <- TRUE                      # a button lets user set this on and off
plotExists <- FALSE                    # used to control several menus and actions
pressureThreshold <- 0.5
## * Data-quality Flag Scheme
##
##     name    "IOOS"
##     mapping list(pass=1, not_evaluated=2, suspect=3, fail=4, missing=9)
badFlagValue <- 4                      # "fail": flagged as bad
initialFlagValue <- 2                  # "not evaluated": the default after reading data
N2L <- 1                               # used in N2 calculation

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
colHistMean <- "forestgreen"
colHist3SD <- "red"

library(shiny)
library(shinythemes)
library(oce)
library(oceanglider)
##library(shinycssloaders)

options(oceEOS="gsw")

N2profile <- function(pressure, sigma0, g=9.8, L=1, rho0=1025)
{
  z <- swZ(pressure)
  o <- order(z)
  oo <- order(o)
  if (L > 0) {
    m <- runlm(z[o], sigma0[o], L=L)
    sigma0 <- m$y[o]
    z <- m$x[o]
    rval <- -g / rho0 * m$dydx
  } else {
    rval <- -g / rho0 * diff(sigma0)/diff(z)
    rval <- c(rval[1], rval)
  }
  rval[!is.finite(rval)] <- NA
  rval[oo]
}


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
SA <- CT <- C <- p <- NULL
SAmean <- SAsd <- NA
CTmean <- CTsd <- NA
Cmean <- Csd <- NA
pmean <- psd <- NA
N2pmean <- N2psd <- N2nlmean <- N2nlsd <- NA # pos and neg log N2 mean and std dev

ndata <- 0
t <- NULL
maxYo <- NULL
powerOnThreshold <- 60 # if no samples during this number of seconds, assume power was off
powerOffIndex <- NULL

ui <- fluidPage(shinythemes::themeSelector(),
                tags$script('$(document).on("keypress",
                            function (e) {
                              Shiny.onInputChange("keypress", e.which);
                              Shiny.onInputChange("keypressTrigger", Math.random());
                            });'),
                fluidRow(column(1, h6(paste(appName, appVersion))),
                         column(2, checkboxInput("debug", h6("Debug"), value=TRUE)),
                         column(2, checkboxInput("instructions", h6("Show Instructions"), value=FALSE))),
                conditionalPanel(condition="input.instructions", fluidRow(includeMarkdown("qc06_help.md"))),
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
                                                 uiOutput(outputId="focusYo")))
                                ## conditionalPanel(condition="input.focus == 'yo'",
                                ##                  uiOutput(outputId="flagYo"))
                         ),
                fluidRow(column(2, conditionalPanel(condition="output.gliderExists",
                                                    uiOutput(outputId="trimOutliers"))),
                         column(2, conditionalPanel(condition="output.gliderExists",
                                                    uiOutput(outputId="hideInitialYos"))),
                         column(2, conditionalPanel(condition="output.gliderExists",
                                                    uiOutput(outputId="hideTop"))),
                         column(2, conditionalPanel(condition="output.gliderExists",
                                                    uiOutput(outputId="hideAfterPowerOn")))
                         ),
                ## fluidRow(#column(3,
                ##          #       conditionalPanel(condition="output.gliderExists",
                ##          #                        uiOutput(outputId="despikePressure"))),
                ##          column(3,
                ##                 conditionalPanel(condition="output.gliderExists",
                ##                                  uiOutput(outputId="trimOutliers")))
                ##          ),
                fluidRow(conditionalPanel(condition="output.gliderExists",
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

  global <- reactiveValues(yoSelected=NULL) # status line updats with this but not plots
  state <- reactiveValues(rda="",
                          flag=NULL,
                          focusYo=1,
                          gliderExists=FALSE,
                          usr=NULL,
                          ##yoSelected=NULL,
                          hideAfterPowerOn=0)

  saveYoAtMouse <- function(xmouse, ymouse)
  {
    if (plotExists && !is.null(xmouse) && !grepl("histogram", input$plotChoice)) {
      ## BOOKMARK_plot_type_3_of_4: note that 1, 2 and 4 must align with this
      if (input$plotChoice == "TS") {
        x <- g[["SA"]]
        y <- g[["CT"]]
      } else if (grepl("time-series$", input$plotChoice)) {
        x <- as.numeric(g[["time"]])
        if (input$plotChoice == "conductivity time-series") {
          y <- g[["conductivity"]]
        } else if (input$plotChoice == "pressure time-series") {
          y <- g[["pressure"]]
        } else if (input$plotChoice == "salinity time-series") {
          y <- g[["SA"]]
        } else if (input$plotChoice == "spiciness time-series") {
          y <- g[["spiciness0"]]
        } else if (input$plotChoice == "temperature time-series") {
          y <- g[["CT"]]
        } else if (input$plotChoice == "tSincePowerOn time-series") {
          y <- x - g[["tSincePowerOn"]]
        } else {
          stop("coding error: cannot saveYoAtMouse unknown time-series type '", input$plotChoice, "'", sep="")
        }
      } else if (grepl("profile$", input$plotChoice)) {
        y <- g[["pressure"]]
        if (input$plotChoice == "conductivity profile") {
          x <- g[["conductivity"]]
        } else if (input$plotChoice == "density profile") {
          x <- g[["sigma0"]]
        } else if (input$plotChoice == "salinity profile") {
          x <- g[["SA"]]
        } else if (input$plotChoice == "spiciness profile") {
          x <- g[["spiciness0"]]
        } else if (input$plotChoice == "temperature profile") {
          x <- g[["CT"]]
        } else {
          stop("coding error: cannot saveYoAtMouse unknown profile type '", input$plotChoice, "'", sep="")
        }
      } else if (length(grep("histogram$", input$plotChoice))) {
        return()
      }
      dist <- sqrt(((xmouse-x)/(state$usr[2]-state$usr[1]))^2 + ((ymouse-y)/(state$usr[4]-state$usr[3]))^2)
      visible <- visibleIndices()
      dist[!visible] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
      disti <- which.min(dist)
      d <- g[["payload1"]][disti,]
      msg("yo=", d$yoNumber, "\n")
      ##state$yoSelected <<- d$yoNumber
      global$yoSelected <<- d$yoNumber
    }
  }


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

  #' Suport function for plotting and brushing, used in several spots.
  #' This takes about 0.17s to compute (negigible compared with plotting), so
  #" no attempted is made to set up fancy caching.
  visibleIndices <- function(message="") {
    msg("  visibleIndices(", message, ") {\n", sep="")
    msg("    input$focus='", input$focus,"', input$focusYo=", input$focusYo, "\n", sep="")
    msg("    total number of original data points:            ndata         =", ndata, "\n")
    ## 1. start with data being displayed
    visible <- rep(TRUE, ndata)
    if (!is.null(input$focus) && input$focus == "yo")
      visible <- visible & (g[["yoNumber"]] == as.numeric(input$focusYo))
    ## 2. start with data in desired navStage
    visible <- visible & (g[["navState"]] %in% input$navState)
    msg("    after select navState:                           sum(!visible) =", sum(!visible), "\n")
    ## 3. remove any pressure spikes
    ##. if (input$despikePressure) {
    ##.   p <- g[["pressure"]]
    ##.   badp <- is.na(p)
    ##.   if (any(badp))
    ##.     p[badp] <- mean(p, na.rm=TRUE) # will trim later anyhow
    ##.   pressureShift <- abs(p - runmed(p, k=11))
    ##.   badPressure <- pressureShift > pressureThreshold
    ##.   if (any(badp))
    ##.     badPressure[badp] <- TRUE
    ##.   visible <- visible & !badPressure
    ##. }
    ##. msg("    after despikePressure:                           sum(!visible) =", sum(!visible), "\n")
    ## 4. remove pressures less than a specified limit
    if (input$hideTop > 0) {
      tooNearSurface <- p < input$hideTop
      visible <- visible & !tooNearSurface
    }
    msg("    after hideTop:                                   sum(!visible) =", sum(!visible), "\n")
    ## 5. hide initial yos
    if (input$hideInitialYos > 0)
      visible <- visible & (g[["yoNumber"]] > as.numeric(input$hideInitialYos))
    msg("    after hideInitialYos:                            sum(!visible) =", sum(!visible), "\n")
    ## 5. ignore for some time after powerup
    hapu <- debounce(hideAfterPowerOn, 2000)()
    poweringOn <- g[["tSincePowerOn"]] < hapu
    if (file.exists("stop")) browser()
    visible <- visible & !poweringOn
    msg("    after hideAfterPowerup:                          sum(!visible) =", sum(!visible), "\n")
    if (!is.null(input$trimOutliers) && input$trimOutliers) {
      SAok <- abs(SA - SAmean) < 3 * SAsd
      CTok <- abs(CT - CTmean) < 3 * CTsd
      Cok <- abs(C - Cmean) < 3 * Csd
      ok <- SAok & CTok & Cok
      ok[is.na(ok)] <- FALSE
      visible <- visible & ok
      msg("    after trimOutliers:                              sum(!visible) =", sum(!visible), "\n")
    }
    ## 6. for input$plotChoice=="N2 extremes", only show extreme data,
    ##    to avoid missing important data by over-plotting.
    if (input$colorBy == "N2 extremes") {
      N2 <- g[["N2"]]
      visible <- visible & ((N2 > 10^(N2plmean + 3*N2plsd)) | (N2 < -10^(N2nlmean + 3*N2nlsd)))
      visible[is.na(visible)] <- FALSE
      msg("    after isolating to high positive or negative N2: sum(!visible) =", sum(!visible), "\n")
    }
    ## 7. ignore already-flagged data
    visible <- visible & (state$flag != badFlagValue)
    msg("    after accounting for already-flagged data:       sum(!visible) =", sum(!visible), "\n")
    msg("  }  # visibleIndices(", message, ")\n", sep="")
    ## DEVELOPER: put new tests here, and be sure to use msg()
    visible
  }


  relevantRdaFiles <- function(glider=NULL, mission=NULL)
  {
    Sys.glob(paste(tolower(glider), tolower(mission), "*.rda", sep=""))
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

  rdaName <- function(time=TRUE) { # skip sec in timestamp, to save 3 char in pulldown menu
    tolower(paste0(paste(input$glider, input$mission, sep=""),
                   "_",
                   format(oce::presentTime(), "%Y%m%d_%H%M"), ".rda", sep=""))
  }

  output$gliderExists <- reactive({
    state$gliderExists
  })

  output$glider <- renderUI({
    state$gliderExists <- FALSE
    selectInput(inputId="glider",
                label=h6("Read raw data"),
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
      fileTimes <- gsub("([a-z0-9]*)_([0-9]+)_([0-9]+).rda", "\\2\\3", files)
      selectInput(inputId="rdaInputFile", label=h6("Continue"), choices=fileTimes, selected=fileTimes[1])
    }
  })

  output$loadRda <- renderUI({
    ##msg("output$loadRda\n")
    files <- relevantRdaFiles(input$glider, input$mission)
    if (length(files)) {
      actionButton(inputId="loadRdaAction", label=h6("Load previous"))
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
                label=h6("Focus"),
                choices=c("mission <m>"="mission", "yo <y>"="yo"),
                selected="mission")
  })

  output$plotChoice <- renderUI({
    selectInput(inputId="plotChoice",
                label=h6("Plot"),
                ## BOOKMARK_plot_type_1_of_4: note that 2, 3 and 4 must align with this
                choices=c("TS <t>"="TS",
                          "C(t)"="conductivity time-series",
                          "p(t) <p>"="pressure time-series",
                          "S(t) <s>"="salinity time-series",
                          "spiciness(t)"="spiciness time-series",
                          "T(t)"="temperature time-series",
                          "tSincePowerOn(t)"="tSincePowerOn time-series",
                          "C(p)"="conductivity profile",
                          "density(p)"="density profile",
                          "S(p)"="salinity profile",
                          "spiciness(p)"="spiciness profile",
                          "T(p)"="temperature profile",
                          "hist(C)"="conductivity histogram",
                          "hist(p)"="pressure histogram",
                          "hist(S)"="salinity histogram",
                          "hist(T)"="temperature histogram"),
                selected="pressure time-series")
  })

  output$plotType <- renderUI({
    selectInput(inputId="plotType", label=h6("Plot Style"), choices= c("l", "p", "o"), selected=c("p"))
  })

  output$colorBy <- renderUI({
    selectInput(inputId="colorBy",
                label=h6("Colour by"),
                choices=c("distance"="distance",
                          "latitude"="latitude",
                          "longitude"="longitude",
                          "p"="pressure",
                          "density"="sigma0",
                          "T"="temperature",
                          "S"="salinity",
                          "N2"="N2",
                          "N2 extremes"="N2 extremes",
                          "navState"="navState",
                          "tSincePowerOn"="tSincePowerOn",
                          "spiciness"="spiciness0",
                          "(none)"="(none)"),
                selected="distance")
  })

  output$focusYo <- renderUI({
    numericInput("focusYo",
                 ##if (is.null(maxYo)) "Yo number [enter value within 5s]" else paste("Yo number (in range 1 to ", maxYo, ") [enter value within 5s]", sep=""),
                 if (is.null(maxYo)) "Yo number" else paste("Yo number (in range 1 to ", maxYo, ")", sep=""),
                 value=if (is.null(global$yoSelected)) "1" else global$yoSelected)
                 ##value=if (is.null(state$yoSelected)) "1" else state$yoSelected)
  })

  observeEvent(input$keypressTrigger, {
               msg("keypress '", input$keypress, "'\n", sep="")
               if (input$keypress == 109) { # "m" only works if focus is 'yo'
                 ##msg("  m=mission-focus\n")
                 if (input$focus == "yo")
                     updateTextInput(session, "focus", value="mission")
               } else if (input$keypress == 121) { # "y" only works if focus is 'mission'
                 ##msg("  y=yo-focus\n")
                 if (input$focus == "mission")
                     updateTextInput(session, "focus", value="yo")
               } else if (input$keypress == 110) { # "n" only works if focus is 'yo'
                 ##msg("  n=next yo\n")
                 if (input$focus == "yo") {
                   if (input$focusYo < maxYo)
                     updateNumericInput(session, "focusYo", value=input$focusYo+1)
                 }
               } else if (input$keypress == 112) { # "p" only works if focus is 'yo'
                 ##msg("  p=previous yo\n")
                 if (input$focus == "yo") {
                   if (input$focusYo > (input$hideInitialYos+1))
                     updateNumericInput(session, "focusYo", value=input$focusYo-1)
                 }
               } else if (input$keypress %in% c(80, 112)) { # "p" or "P"
                 ## switch plotType to "pressure time-series"
                 updateTextInput(session, "plotChoice", value="pressure time-series")
               } else if (input$keypress %in% c(83, 115)) { # "s" or "S"
                 ## switch plotType to "S(t)"
                 updateTextInput(session, "plotChoice", value="salinity time-series")
               } else if (input$keypress %in% c(84, 116)) { # "t" or "T"
                 ## switch plotType to "TS"
                 updateTextInput(session, "plotChoice", value="TS")
               } else if (input$keypress %in% c(67, 99)) { # "c"  or "C"
                 ## should only work if focus is 'mission'
                 ##msg("before: yoSelected=", state$yoSelected, "\n")
                 msg(" 'c' pressed (input$hover$x=", input$hover$x, ", input$hover$y=", input$hover$y, ")\n")
                 saveYoAtMouse(input$hover$x, input$hover$y)
                 ##msg("after interpreting 's' keypress, state$yoSelected=", state$yoSelected, "\n")
                 msg("after interpreting 'c' keypress, global$yoSelected=", global$yoSelected, "\n")
               } else if (input$keypress == 63) { # "?" gives help
                 showModal(modalDialog(title="Key-stroke commands",
        HTML("
<ul>
<li> '<b>n</b>': in yo-focus, go to the next yo. (Ignored in mission-focus.)
<li> '<b>p</b>`: in yo-focus, go to the previous yo. (Ignored in mission-focus.)
<li> '<b>m</b>': in yo-focus, switch to mission-focus. (Ignored in mission-focus.)
<li> '<b>y</b>': in mission-focus, switch to yo-focus. (Ignored in yo-focus.)
<li> '<b>s</b>': in mission-focus, select the yo number under the mouse, so that
the next '<b>y</b>' operation will open a graph for that yo.  (Ignored in
  mission-focus.)
<li> '<b>c</b>': in mission-focus, copy yo number nearest the mouse to an internal
  buffer, so next '<b>y</b>' key-press will graph that yo.  (Ignored in
  mission-focus.)
<li> <b>'p'</b>: switch plot type to <i>p(t)</i>
<li> <b>'s'</b>: switch plot type to <i>S(t)</i>
<li> <b>'t'</b>: switch plot type to <i>TS</i>
<li> '<b>?</b>': show this summary
</ul>"),
        easyClose = TRUE
        ))
               }
  })

  observeEvent(focusYo, {
               msg("observeEvent(focusYo) ...\n")
               ## fy <- throttle(focusYoRaw, 5000)()
               fy <- debounce(focusYo, 1000)()
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
               ##msg("+++ observeEvent(hideAfterPowerOn) ...\n")
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

  ## output$flagYo <- renderUI({
  ##     actionButton(inputId="flagYo", label="Flag yo")
  ## })

  ## observeEvent(input$flagYo, {
  ##              msg("  input$focusYo=", input$focusYo, "\n")
  ##              if (!is.null(input$focusYo)) {
  ##                yo <- g[["yoNumber"]]
  ##                bad <- yo == input$focusYo
  ##                saveEditEvent(paste0("flag yo ", input$focusYo), bad)
  ##              }
  ## })


  ###old output$brushMode <- renderUI({
  ###old     selectInput(inputId="brushMode",
  ###old                 label="Brush mode",
  ###old                 choices=c("flag", "highlight [broken]", "zoom [broken]"),
  ###old                 selected="flag")
  ###old })

  output$hideInitialYos <- renderUI({
    sliderInput("hideInitialYos", h6("Hide initial yos"), min=0, max=10, value=0)
  })

  output$hideTop <- renderUI({
    sliderInput("hideTop", h6("Hide top data [m]"), min=0, max=15, value=0)
  })

  output$hideAfterPowerOn <- renderUI({
    sliderInput("hideAfterPowerOn", h6("Hide after power-on"), min=0, max=120, value=0)
  })

  ## output$despikePressure <- renderUI({
  ##   checkboxInput(inputId="despikePressure", label=h6("Hide p outliers"))
  ## })

  output$trimOutliers <- renderUI({
    checkboxInput(inputId="trimOutliers", label=h6("Hide T,S outliers"))
  })

  output$navState <- renderUI({
    ns <- navStateCodes("seaexplorer")
    checkboxGroupInput(inputId="navState", label=h6(""),
                       choices=ns, selected=ns, inline=TRUE)
  })

  output$status <- renderText({
    hoverx <- input$hover$x
    hovery <- input$hover$y
    res <- if (is.null(g)) "Status: no glider exists. Please read data or load previous analysis." else paste(input$glider, input$mission)
    res <- "(Move mouse into plot window to see properties)"
    if (!is.null(hoverx) && plotExists) {
      ## BOOKMARK_plot_type_2_of_4: note that 1, 3 and 4 must align with this
      if (input$plotChoice == "TS") {
        x <- g[["SA"]]
        y <- g[["CT"]]
      } else if (grepl("time-series$", input$plotChoice)) {
        x <- as.numeric(g[["time"]])
        if (input$plotChoice == "conductivity time-series") {
          y <- g[["conductivity"]]
        } else if (input$plotChoice == "pressure time-series") {
          y <- g[["pressure"]]
        } else if (input$plotChoice == "salinity time-series") {
          y <- g[["SA"]]
        } else if (input$plotChoice == "spiciness time-series") {
          y <- x - g[["spiciness0"]]
        } else if (input$plotChoice == "temperature time-series") {
          y <- g[["CT"]]
        } else if (input$plotChoice == "tSincePowerOn time-series)") {
          y <- x - g[["tSincePowerOn"]]
        } else {
          msg("coding error: cannot determine status for unknown time-series type '", input$plotChoice, "'", sep="")
          return()
        }
      } else if (grepl("profile$", input$plotChoice)) {
        y <- g[["pressure"]]
        if (input$plotChoice == "conductivity profile") {
          x <- g[["conductivity"]]
        } else if (input$plotChoice == "density profile") {
          x <- g[["sigma0"]]
        } else if (input$plotChoice == "salinity profile") {
          x <- g[["SA"]]
        } else if (input$plotChoice == "spiciness profile") {
          x <- g[["spiciness0"]]
        } else if (input$plotChoice == "temperature profile") {
          x <- g[["CT"]]
        } else {
          msg("coding error: cannot determine status for unknown profile type '", input$plotChoice, "'", sep="")
          return()
        }
      } else if (grepl("histogram$", input$plotChoice)) {
        return()
      }
      dist <- sqrt(((hoverx-x)/(state$usr[2]-state$usr[1]))^2 + ((hovery-y)/(state$usr[4]-state$usr[3]))^2)
      visible <- visibleIndices()
      dist[!visible] <- 2 * max(dist, na.rm=TRUE) # make flagged points be "far away"
      disti <- which.min(dist)
      d <- g[["payload1"]][disti,]
      res <- sprintf("yo=%d p=%.1f SA=%.4f CT=%.4f navState=%d %.3fE %.3fN %s\n",
                     d$yoNumber, d$pressure, d$SA, d$CT, d$navState,
                     d$longitude, d$latitude, format(d$time, "%Y-%m-%dT%H:%M:%S"))
      ## if (!is.null(state$yoSelected))
      ##   res <- paste0(res, "[selected yo=", state$yoSelected, "]")
      if (!is.null(global$yoSelected))
        res <- paste0(res, "[selected yo=", global$yoSelected, "]")
    }
    res
  })

  observeEvent(input$debug, {
               if (!is.null(input$debug))
                 debugFlag <<- input$debug
  })


  observeEvent(input$dblclick, {
               msg("input$dblclick$x=", input$dblclick$x," y=", input$dblclick$y,"\n")
               saveYoAtMouse(input$dblclick$x, input$dblclick$y)
  })

  observeEvent(input$brush, {
               msg("input$brush ...\n")
               xmin <- input$brush$xmin
               xmax <- input$brush$xmax
               ymin <- input$brush$ymin
               ymax <- input$brush$ymax
               bad <- state$flag == 3
               msg("  sum(bad)=", sum(bad), " of ", length(bad), " [based on present state$flag]\n", sep="")
               ## BOOKMARK_plot_type_4_of_4: note that 1, 2 and 3 must align with this
               if (input$plotChoice == "TS") {
                 x <- g[["SA"]]
                 y <- g[["CT"]]
               } else if (grepl("time-series$", input$plotChoice)) {
                 x <- as.numeric(g[["time"]])
                 if (input$plotChoice == "conductivity time-series") {
                   y <- g[["conductivity"]]
                 } else if (input$plotChoice == "pressure time-series") {
                   y <- g[["pressure"]]
                 } else if (input$plotChoice == "salinity time-series") {
                   y <- g[["SA"]]
                 } else if (input$plotChoice == "spiciness time-series") {
                   y <- g[["spiciness0"]]
                 } else if (input$plotChoice == "temperature time-series") {
                   y <- g[["CT"]]
                 } else if (input$plotChoice == "tSincePowerOn time-series") {
                   y <- x - g[["tSincePowerOn"]]
                 } else {
                   stop("coding error: cannot brush unknown time-series type '", input$plotChoice, "'", sep="")
                 }
               } else if (grepl("profile$", input$plotChoice)) {
                 y <- g[["pressure"]]
                 if (input$plotChoice == "conductivity profile") {
                   y <- g[["conductivity"]]
                 } else if (input$plotChoice == "density profile") {
                   y <- g[["sigma0"]]
                 } else if (input$plotChoice == "salinity profile") {
                   x <- g[["SA"]]
                 } else if (input$plotChoice == "spiciness profile") {
                   y <- g[["spiciness0"]]
                 } else if (input$plotChoice == "temperature profile") {
                   x <- g[["CT"]]
                 } else {
                   stop("coding error: cannot brush unknown profile type '", input$plotChoice, "'", sep="")
                 }
               } else if (grepl("histogram$", input$plotChoice)) {
                 if (input$plotChoice == "conductivity histogram") {
                   x <- g[["conductivity"]]
                   bad <- xmin <= x & x <= xmax
                   bad[is.na(bad)] <- TRUE
                   bad <- bad & visibleIndices() # we do not invalidate data not in the present view.
                   saveEditEvent("brush conductivity histogram", bad)
                   return()
                 } else if (input$plotChoice == "pressure histogram") {
                   x <- g[["pressure"]]
                   bad <- xmin <= x & x <= xmax
                   bad[is.na(bad)] <- TRUE
                   bad <- bad & visibleIndices() # we do not invalidate data not in the present view.
                   saveEditEvent("brush pressure histogram", bad)
                   return()
                 } else if (input$plotChoice == "salinity histogram") {
                   x <- g[["SA"]]
                   bad <- xmin <= x & x <= xmax
                   bad[is.na(bad)] <- TRUE
                   bad <- bad & visibleIndices() # we do not invalidate data not in the present view.
                   saveEditEvent("brush salinity histogram", bad)
                   return()
                 } else if (input$plotChoice == "temperature histogram") {
                   x <- g[["CT"]]
                   bad <- xmin <= x & x <= xmax
                   bad[is.na(bad)] <- TRUE
                   bad <- bad & visibleIndices() # we do not invalidate data not in the present view.
                   saveEditEvent("brush temperature histogram", bad)
                   return()
                  } else {
                   stop("coding error: cannot brush unknown histogram type")
                 }
               } else {
                 stop("coding error: brushing for plot type '", input$plotChoice, "' is not coded yet")
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
               p <<- g[["pressure"]]
               ndata <<- length(p)
               pmean <<- mean(p, na.rm=TRUE)
               psd <<- sd(p, na.rm=TRUE)

               ## FIXME save into object, for later use (even outside this app)
               ## yo-by-yo N^2 calculation
               g@data$payload1[["SA"]] <<- g[["SA"]]
               g@data$payload1[["CT"]] <<- g[["CT"]]
               sigma0 <-  g[["sigma0"]]
               g@data$payload1[["sigma0"]] <<- sigma0
               g@data$payload1[["spiciness0"]] <<- g[["spiciness"]]
               g@data$payload1[["distance"]] <<- oce::geodDist(g[["longitude"]], g[["latitude"]], alongPath=FALSE)
               g@data$payload1[["navStateColor"]] <<- navStateColors(g[["navState"]])
               ## It's a bit tricky to calculate N^2, because it will be defined
               ## per yo.
               A <- split(data.frame(p=p, sigma0=sigma0), g[["yoNumber"]])
               rho0 <- 1000 + mean(sigma0, na.rm=TRUE)
               N2 <- NULL
               for (Ayo in A) {
                 N2 <- c(N2, N2profile(Ayo$p, Ayo$sigma0, L=N2L, rho0=rho0))
               }
               N2plmean <<- mean(log10(subset(N2, N2 > 0)), na.rm=TRUE)
               N2plsd <<- sd(log10(subset(N2, N2 > 0)), na.rm=TRUE)
               N2nlmean <<- mean(log10(-subset(N2, N2 < 0)), na.rm=TRUE)
               N2nlsd <<- sd(log10(-subset(N2, N2 < 0)), na.rm=TRUE)
               g@data$payload1[["N2"]] <<- N2

               SA <<- g[["SA"]]
               SAmean <<- mean(SA, na.rm=TRUE)
               SAsd <<- sd(SA, na.rm=TRUE)
               CT <<- g[["CT"]]
               CTmean <<- mean(CT, na.rm=TRUE)
               CTsd <<- sd(CT, na.rm=TRUE)
               C <<- g[["conductivity"]]
               Cmean <<- mean(C, na.rm=TRUE)
               Csd <<- sd(C, na.rm=TRUE)

               state$flag <<- rep(initialFlagValue, ndata)
               ## Will use a single flag
               g@metadata$flags$payload1 <<- list(state$flag) # NOTE: no name given, which means apply to all
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
               filename <- paste(tolower(input$glider), tolower(input$mission),
                                 "_", substr(input$rdaInputFile, 1, 8),
                                 "_", substr(input$rdaInputFile, 9, 12), ".rda", sep="")
               ###msg("  load from '", filename, "' ..", sep="")
               withProgress(message=paste0("Loading '", filename, "'"), value=0,
                            {
                              load(filename)
                            })
               g <<- g
               p <<- g[["pressure"]]
               pmean <<- mean(p, na.rm=TRUE)
               psd <<- sd(p, na.rm=TRUE)
               SA <<- g[["SA"]]
               SAmean <<- mean(SA, na.rm=TRUE)
               SAsd <<- sd(SA, na.rm=TRUE)
               CT <<- g[["CT"]]
               CTmean <<- mean(CT, na.rm=TRUE)
               CTsd <<- sd(CT, na.rm=TRUE)
               C <<- g[["conductivity"]]
               Cmean <<- mean(C, na.rm=TRUE)
               Csd <<- sd(C, na.rm=TRUE)
               N2 <- g[["N2"]]
               N2plmean <<- mean(log10(subset(N2, N2 > 0)), na.rm=TRUE)
               N2plsd <<- sd(log10(subset(N2, N2 > 0)), na.rm=TRUE)
               N2nlmean <<- mean(log10(-subset(N2, N2 < 0)), na.rm=TRUE)
               N2nlsd <<- sd(log10(-subset(N2, N2 < 0)), na.rm=TRUE)
               ndata <<- length(p)
               maxYo <<- max(g[["yoNumber"]], na.rm=TRUE)
               t <<- as.numeric(g[["time"]]) # in seconds, for hover operations
               state$flag <<- g@metadata$flags$payload1$overall
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
        #", despikePressure=", input$despikePressure,
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
        if (grepl("time-series$", input$plotChoice)) {
          msg("* a time-series plot (input$plotChoice is '", input$plotChoice, "')\n", sep="")
          if (input$plotChoice == "conductivity time-series") {
            dataName <- "conductivity"
            axisName <- "conductivity S/m"
          } else if (input$plotChoice == "pressure time-series") {
            dataName <- "pressure"
            axisName <- "p"
          } else if (input$plotChoice == "salinity time-series") {
            dataName <- "SA"
            axisName <- "absolute salinity"
          } else if (input$plotChoice == "spiciness time-series") {
            dataName <- "spiciness0"
            axisName <- "spiciness"
          } else if (input$plotChoice == "temperature time-series") {
            dataName <- "CT"
            axisName <- "conservative temperature"
          } else if (input$plotChoice == "tSincePowerOn time-series") {
            dataName <- "tSincePowerOn"
            axisName <- "Time since powerup"
          } else {
            stop("programmer error: cannot plot time-series name '", input$plotChoice, "'")
          }
          x <- g@data$payload1[look, "time"]
          y <- g@data$payload1[look, dataName]
          msg("time-series plot. x (time) range:", paste(range(x, na.rm=TRUE), collapse=" to "),
              ", y range:", paste(range(y, na.rm=TRUE), collapse=" to "), "\n")
          ylim <- range(y, na.rm=TRUE)
          if (input$plotChoice == "pressure time-series")
            ylim <- rev(ylim)
          ylab <- resizableLabel(axisName)
          if (input$colorBy != "(none)") {
            if (input$colorBy == "navState") {
              timing <- system.time({
                oce.plot.ts(x, y, ylim=ylim,
                            type=input$plotType,
                            col=gg[["navStateColor"]],
                            mar=marTimeseries,
                            ylab=ylab, pch=pch, cex=cex, flipy=input$plotChoice=="pressure time-series")
              })
              msg(dataName, " time-series plot (coloured by navState) took elapsed time ", timing[3], "s\n", sep="")
              navStateLegend()
            } else if (input$colorBy == "N2 extremes") {
              oce.plot.ts(x, y, ylim=ylim,
                          type=input$plotType,
                          col=ifelse(g@data$payload1[look, "N2"] > 0, "forestgreen", "red"),
                          mar=marTimeseries,
                          ylab=ylab, pch=pch, cex=cex, flipy=input$plotChoice=="pressure time-series")
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
                            ylab=ylab, pch=pch, cex=cex, flipy=input$plotChoice=="pressure time-series")
              })
              msg(input$plotType, " time-series plot (coloured by ", input$colorBy, ") took elapsed time ", timing[3], "s\n", sep="")
            }
          } else {
            timing <- system.time({
              oce.plot.ts(x, y,
                          type=input$plotType,
                          mar=marTimeseries,
                          ylab=ylab, pch=pch, cex=cex, flipy=input$plotChoice=="pressure time-series")
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
            } else if (input$colorBy == "N2 extremes") {
              ## gray for whole dataset, then coloured for the narrowed dataset
              plotTS(g, pch=pch, cex=cex, col="lightgray", mar=marTS)
              points(gg[["SA"]], gg[["CT"]], pch=pch, cex=cex, col=ifelse(gg[["N2"]]>0, "forestgreen", "red"))
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
        } else if (length(grep("profile$", input$plotChoice))) {
          if ("conductivity profile" == input$plotChoice) {
            dataName <- "conductivity"
            axisName <- "conductivity S/m"
          } else if ("density profile" == input$plotChoice) {
            dataName <- "sigma0"
            axisName <- "sigma0"
          } else if ("salinity profile" == input$plotChoice) {
            dataName <- "SA"
            axisName <- "absolute salinity"
          } else if ("spiciness profile" == input$plotChoice) {
            dataName <- "spiciness0"
            axisName <- "spiciness0"
          } else if ("temperature profile" == input$plotChoice) {
            dataName <- "CT"
            axisName <- "conservative temperature"
          } else {
            stop("coding error: cannot plot unrecognized profile name '", input$plotChoice, "'")
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
        } else if (grepl("histogram$", input$plotChoice)) {
          if (input$plotChoice == "pressure histogram") {
            hist(gg[["pressure"]], breaks=100, main="Histogram of unflagged values", xlab="Pressure [dbar]")
            abline(v=pmean + psd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD), lwd=1.4)
            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                  at=pmean + psd * c(-3, 0, 3),
                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
          } else if (input$plotChoice == "conductivity histogram") {
            hist(gg[["conductivity"]], breaks=100, main="Histogram of unflagged values", xlab="Conductivity")
            abline(v=Cmean + Csd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                  at=Cmean + Csd * c(-3, 0, 3),
                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
          } else if (input$plotChoice == "salinity histogram") {
            hist(gg[["SA"]], breaks=100, main="Histogram of unflagged values", xlab="Absolute Salinity")
            abline(v=SAmean + SAsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                  at=SAmean + SAsd * c(-3, 0, 3),
                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
          } else if (input$plotChoice == "temperature histogram") {
            hist(gg[["CT"]], breaks=100, main="Histogram of unflagged values", xlab="Conservative Temperature")
            abline(v=CTmean + CTsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                  at=CTmean + CTsd * c(-3, 0, 3),
                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
          } else {
            stop("coding error: cannot plot unrecognized histogram name '", input$plotChoice, "'")
          }
        } else {
            stop("coding error: cannot plot unknown item '", input$plotChoice, "'")
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

