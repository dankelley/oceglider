## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2

debug <- 1
version <- "0.1.1"

library(shiny)
library(shinythemes)
library(oce)
library(oceanglider)
library(shinycssloaders)

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
                             strong("1. Select glider & mission"),
                             uiOutput(outputId="glider"),
                             uiOutput(outputId="mission"),
                             hr(),
                             tags$b("2a. Read raw data..."),
                             uiOutput(outputId="read"),
                             uiOutput(outputId="listRda"),
                             uiOutput(outputId="loadRda"),
                             hr(),
                             tags$b("3. Save analysis"),
                             uiOutput(outputId="saveRda"),
                             hr(),
                             uiOutput(outputId="plotChoice"),
                             uiOutput(outputId="colorBy"),
                             radioButtons(inputId="flagAction",
                                          label="Flagged action (broken)",
                                          choices=c("omit", "highlight"),
                                          selected=c("omit"),
                                          inline=TRUE),
                             uiOutput(outputId="deleteYo")),
                mainPanel(uiOutput(outputId="navState"),
                          uiOutput(outputId="status"),
                          withSpinner(plotOutput("plot",
                                                 hover="hover",
                                                 click="click",
                                                 width="100%",
                                                 height="650px",
                                                 brush=brushOpts(id="brush", resetOnNew=TRUE)))))


server <- function(input, output) {

  plotExists <- FALSE
  state <- reactiveValues(rda="", flag=NULL, yoSelected=NULL)

  relevantRdaFiles <- function(glider=NULL, mission=NULL)
  {
    Sys.glob(paste(tolower(glider), "_", tolower(mission), "*.rda", sep=""))
  }

  # flag=1 if ok; =3 if bad

  edits <- list()
  nedits <- length(edits)

  dataName <- function(basedir="/data/glider/delayedData") {
    if (nchar(input$glider) && nchar(input$mission)) {
      ## Start with what seems to be the most common directory structure.
      ## /data/glider/delayedData/SEA019/Data/M28/Payload/logs/
      glider <- input$glider
      mission <- input$mission
      if (tolower(glider) == "sea019") {
        if (tolower(mission) == "m28")
          return(paste(basedir, glider, "Data", mission, "Payload/logs", sep="/")) # no .pld1. files
        if (tolower(mission) == "m30")
          return(paste(basedir, glider, "Data", mission, mission, "Payload/logs/logs", sep="/"))
        if (tolower(mission) == "m49")
          return(paste(basedir, glider, "Data", mission, "PLD/logs/logs", sep="/"))
      }
      return(paste(basedir, glider, "Data", mission, "pld/logs", sep="/"))
    }
    NULL
  }

  varName <- function() {
    tolower(paste(input$glider, "_", input$mission, sep=""))
  }

  rdaName <- function(time=TRUE) { # timestamp does not give seconds, saving 3 chars in pulldown menu
    tolower(paste0(varName(), "_", format(Sys.time(),"%Y-%m-%dT%H:%M:%S"), ".rda", sep=""))
  }

  dataStatus <- function() {
    cat(file=stderr(), "dataStatus...\n")
    cat(file=stderr(), "  names(var): ", paste(names(var), collapse=","), "\n")
    "read" # "read" "load" ""
  }

  output$glider <- renderUI({
    selectInput(inputId="glider",
                label="",
                choices=gliders,
                selected=gliders[1])
  })

  output$mission <- renderUI({
    selectInput(inputId="mission",
                label="",
                choices=missions[input$glider],
                selected=missions[input$glider][1])
  })

  output$read <- renderUI({
    actionButton(inputId="readData", label="Read")
  })

  output$listRda <- renderUI({
    cat(file=stderr(), "output$listRda\n", sep="")
    ## files <- Sys.glob(paste0(varName(), "*.rda"))
    cat(file=stderr(), "  input$glider='", input$glider, "'\n", sep="")
    cat(file=stderr(), "  input$mission='", input$mission, "'\n", sep="")
    files <- relevantRdaFiles(input$glider, input$mission)
    cat(file=stderr(), "  files=", paste(files, collapse=","), "\n")
    if (length(files)) {
      filedates <- gsub("([a-z0-9]*)_([a-z0-9]*)_(.*).rda", "\\3", files)
      cat(file=stderr(), "  filedates='", paste(filedates, collapse="','"), "'\n", sep="")
      selectInput(inputId="rdaInputFile", label="2b. ... or resume previous analysis", choices=filedates, selected=filedates[1])
    }
  })

  output$loadRda <- renderUI({
    cat(file=stderr(), "output$listRda\n", sep="")
    files <- relevantRdaFiles(input$glider, input$mission)
    cat(file=stderr(), "  files=", paste(files, collapse=","), "\n")
    if (length(files)) {
      actionButton(inputId="loadRdaAction", label="Load .rda")#h5(paste0("Load ", rda)))
    }
  })

  output$saveRda <- renderUI({
    cat(file=stderr(), "output$saveRda\n", sep="")
    actionButton(inputId="saveRda", label="Save")
  })

  output$plotChoice <- renderUI({
    cat(file=stderr(), "output$plotChoice\n", sep="")
    cat(file=stderr(), "  plotExists=", plotExists, "\n", sep="")
    selectInput(inputId="plotChoice",
                label="Plot type",
                choices=c("pt", "TS", "hist(S)", "hist(p)"),
                selected=c("pt"))
  })

  output$colorBy <- renderUI({
    selectInput(inputId="colorBy",
                label="Colour by",
                choices=c("latitude", "longitude", "pressure", "temperature", "salinity", "navState", "(none)"),
                selected="latitude")
  })

  output$deleteYo <- renderUI({
    if (!is.null(state$yoSelected)) {
      actionButton(inputId="deleteYo", label=paste("Delete yo #", state$yoSelected, sep=""))
    }
  })

  output$redraw <- renderUI({
    actionButton(inputId="resetPlot", h5("Reset plot scales"))
  })

  output$navState <- renderUI({
    ns <- navStateCodes("seaexplorer")
    checkboxGroupInput(inputId="navState", label="Show navState",
                       choices=ns, selected=ns, inline=TRUE)
  })

  observeEvent(input$navState, {
               cat(file=stderr(), "input$navState observed...\n")
               cat(file=stderr(), "  navState=c(", paste(input$navState, collapse=","), ")\n", sep="")
  })

  output$status <- renderText({
    x <- input$hover$x
    y <- input$hover$y
    res <- if (is.null(g)) "Status: no glider exists. Please read data or load previous analysis." else paste(input$glider, input$mission)
    res <- "-"
    distThreshold <- 0.05
    usr <- par("usr")
    if (!is.null(x) && plotExists) {
      ## note scaling of the x and y, dependent on plot type. The scales are a rough guess.
      if (input$plotChoice == "pt") {
        dist <- sqrt(((x-t)/(usr[2]-usr[1]))^2 + ((y-p)/(usr[4]-usr[3]))^2)
        disti <- which.min(dist)
        d <- g[["payload1"]][disti,]
        ## FIXME: maybe hide if dist[disti] exceeds some number
        res <- sprintf("dist=%.4f %s yo=%d %s p=%.1f\n",
                       dist[disti],
                       if (dist[disti] < distThreshold) " (click to select) " else {
                         if (!is.null(state$yoSelected)) " (click to unselect)" else ""},
                       d$yoNumber, format(d$time, "%Y-%m-%dT%H:%M:%S"), d$pressure)
      } else if (input$plotChoice == "TS") {
        dist <- sqrt(((x-SA)/(usr[2]-usr[1]))^2 + ((y-CT)/(usr[4]-usr[3]))^2)
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
               state$flag[yo == state$yoSelected] <- 3
               state$yoSelected <<- NULL
  })

  observeEvent(input$click, {
               cat(file=stderr(), "click\n", sep="")
               distThreshold <- 0.05
               usr <- par("usr")
               x <- input$click$x
               y <- input$click$y
               if (input$plotChoice == "pt") {
                 dist <- sqrt(((x-t)/(usr[2]-usr[1]))^2 + ((y-p)/(usr[4]-usr[3]))^2)
                 disti <- which.min(dist)
                 d <- g[["payload1"]][disti,]
                 res <- sprintf("dist=%.4f x=%.4f y=%.4f (yo=%d, t=%s, p=%.1f)\n",
                                dist[disti], x, y, d$yoNumber, d$time, d$pressure)
                 state$yoSelected <- if (dist[disti] < distThreshold) d$yoNumber else NULL
                 cat(file=stderr(), res)
               } else if (input$plotChoice == "TS") {
                 dist <- sqrt(((x-SA)/(usr[2]-usr[1]))^2 + ((y-CT)/(usr[4]-usr[3]))^2)
                 disti <- which.min(dist)
                 d <- g[["payload1"]][disti,]
                 res <- sprintf("dist=%.4f x=%.4f y=%.4f (yo=%d, t=%s, p=%.1f, S=%.4f, T=%.4f)\n",
                                dist[disti], x, y, d$yoNumber, d$time, d$pressure, d$salinity, d$temperature)
                 state$yoSelected <- if (dist[disti] < distThreshold) d$yoNumber else NULL
                 cat(file=stderr(), res)
               }
  })

  ##?? observeEvent(input$hover, {
  ##??              x <- input$hover$x
  ##??              y <- input$hover$y
  ##??              cat(file=stderr(), "x=", x, ", y=", y, "\n")
  ##??              if (input$plotChoice == "pt") {
  ##??                i <- which.min(abs(x - t)/3600 + abs(y - p))
  ##??                d <- g[["payload1"]][i,]
  ##??                ##print(d, file=stderr())
  ##??                cat(file=stderr(), "i=", i, ", yo=", d$yo, " (pt)\n")
  ##??              } else if (input$plotChoice == "TS") {
  ##??                i <- which.min(abs(x - SA)/30 + abs(y - CT) / 10)
  ##??                d <- g[["payload1"]][i,]
  ##??                ## print(d, file=stderr())
  ##??                cat(file=stderr(), "i=", i, ", yo=", d$yo, " (TS)\n")
  ##??              } else if (input$plotChoice == "hist(S)") {
  ##??              } else {
  ##??              }
  ##?? })

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
               } else if (input$plotChoice == "TS") {
                 cat(file=stderr(), "  TS\n", sep="")
                 SA <- g[["SA"]] # FIXME: allow oceEOS=="unesco"
                 CT <- g[["CT"]]
                 bad <- xmin <= SA & SA <= xmax & ymin <= CT & CT <= ymax
                 bad[is.na(bad)] <- TRUE
                 cat(file=stderr(), " sum(bad)=", sum(bad), "\n")
                 ## bad[is.na(bad)] <- TRUE
               } else if (input$plotChoice == "hist(p)") {
                 cat(file=stderr(), "  hist(p)\n", sep="")
                 p <- g[["pressure"]]
                 bad <- xmin <= p & p <= xmax
                 bad[is.na(bad)] <- TRUE
                 cat(file=stderr(), " sum(bad)=", sum(bad), "\n")
                 #if (any(is.na(bad))) browser()
               } else if (input$plotChoice == "hist(S)") {
                 cat(file=stderr(), "  hist(S)\n", sep="")
                 SA <- g[["SA"]] # FIXME: allow oceEOS=="unesco"
                 bad <- xmin <= SA & SA <= xmax
                 bad[is.na(bad)] <- TRUE
                 cat(file=stderr(), " sum(bad)=", sum(bad), "\n")
                 ## bad[is.na(bad)] <- TRUE
               }
               if (length(bad)) {
                 bad <- which(bad)
                 cat(file=stderr(), "  total number of bad points: ", length(bad), "\n", sep="")
                 state$flag[bad] <- 3 # HERE HERE HERE
                 cat(file=stderr(), "  set state ok\n", sep="")
                 nedits <<- nedits + 1
                 edits[[nedits]] <<- bad
                 cat(file=stderr(), "  set edits ok\n", sep="")
                 cat(file=stderr(), "  set nedits ok\n", sep="")
               }
               if (debug > 0) {
                 cat(file=stderr(), "  count of flag==3: ", sum(state$flag==3), " (after)\n", sep="")
                 cat(file=stderr(), "  nedits=", nedits, "\n", sep="")
               }
  })

  observeEvent(input$readData, {
               cat(file=stderr(), "readData...\n")
               dir <- dataName()
               cat(file=stderr(), "  about to read '", dir, "'\n", sep="")
               t <- try(read.glider.seaexplorer.delayed(dir))
               if (inherits(t, "try-error")) {
                 cat(file=stderr(), " no data FIXME: put up a dialog box\n")
                 showModal(modalDialog("", paste0("no .pld1. files in directory '", dir, "'")))
               } else {
                 g <<- t
                 state$flag <- rep(1, length(g[["pressure"]]))
               }
               SA <<- g[["SA"]]
               CT <<- g[["CT"]]
               p <<- g[["pressure"]]
               t <<- as.numeric(g[["time"]]) # in seconds, for hover operations
               ##> state$Slim <- range(SA, na.rm=TRUE) # FIXME: seems to make infinite loop since reactive
               ##> state$Tlim <- range(CT, na.rm=TRUE)
               ##> state$plim <- range(p, na.rm=TRUE)
               cat(file=stderr(), "   ... done\n", sep="")
  })

  observeEvent(input$loadRdaAction, {
               cat(file=stderr(), "loadRda...\n")
               filename <- paste(tolower(input$glider), "_", tolower(input$mission), "_", input$rdaInputFile, ".rda", sep="")
               cat(file=stderr(), "  input$glider '", input$glider, "' ...\n", sep="")
               cat(file=stderr(), "  input$mission '", input$mission, "' ...\n", sep="")
               cat(file=stderr(), "  load from '", filename, "' ..", sep="")
               load(filename)
               g <<- g
               SA <<- g[["SA"]]
               CT <<- g[["CT"]]
               p <<- g[["pressure"]]
               t <<- as.numeric(g[["time"]]) # in seconds, for hover operations
               ##> state$Slim <- range(SA, na.rm=TRUE) # FIXME: seems to make infinite loop since reactive
               ##> state$Tlim <- range(CT, na.rm=TRUE)
               ##> state$plim <- range(p, na.rm=TRUE)
               state$flag <- g[["pressureFlag"]]
               cat(file=stderr(), ". done\n", sep="")
               state$rda <- filename
  })

  observeEvent(input$saveRda, {
               cat(file=stderr(), "saveRda...\n")
               rda <- rdaName()
               cat(file=stderr(), "  save to '", rda, "' ...\n", sep="")
               g@metadata$flags$payload1$pressure <<- ifelse(g[["navState"]] %in% input$navState, state$flag, 3)
               save(g, file=rda)
               cat(file=stderr(), "  ... done\n", sep="")
  })

  output$plot <- renderPlot({
    cat(file=stderr(), "plot:\n", sep="")
    cat(file=stderr(), "  input$plotChoice='", paste(input$plotChoice, collapse=","), "'\n", sep="")
    cat(file=stderr(), "  nedits=", nedits, "\n", sep="")
    cat(file=stderr(), "  is.null(g)=", is.null(g), "\n", sep="")
    cat(file=stderr(), "  plotExists=", plotExists, "\n", sep="")
    cat(file=stderr(), "  colorBy=", input$colorBy, "\n", sep="")

    if (!is.null(g) && !is.null(state$flag))  {
      n <- length(g[["pressure"]])
      cat(file=stderr(), "  n=", n, "\n", sep="")
      ## if (is.null(state$flag)) {
      ##   state$flag <- rep(1, n)
      ##   cat(file=stderr(), "  setting flag\n", sep="")
      ## }
      cat(file=stderr(), "  state$flag[1:5]=", paste(state$flag[1:5],collapse=","), "\n", sep="")
      visible <- g[["navState"]] %in% input$navState
      flagged <- state$flag == 3
      cat(file=stderr(), "  flagged[1:5]: ", paste(flagged[1:5],collapse=","), "\n", sep="")
      cat(file=stderr(), "  colorBy: \"", input$colorBy, "\"\n", sep="")
      if (input$plotChoice == "pt") {
        cat(file=stderr(), "  pt\n")
        t <- g[["time"]]
        p <- g[["pressure"]]
        if (input$flagAction == "highlight") {
          cat(file=stderr(), "  highlight\n")
          oce.plot.ts(t[visible], p[visible], type="p", ylab="Pressure [dbar]", pch=".",
                      cex=3, col=ifelse(flagged, 1, 2))
          points(t[visible & flagged], p[visible & flagged], pch=1, cex=1.4)
        } else if (input$flagAction == "omit") {
          cat(file=stderr(), "  omit\n")
          oce.plot.ts(t[!flagged & visible], p[!flagged & visible], type="p", ylab="Pressure [dbar]", pch=".", cex=3)
        } else {
          stop("unhandled flagAction='", input$flagAction, "' detected by plot")
        }
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
        if (input$flagAction == "omit") {
          gg@data$payload1 <- g@data$payload1[!flagged & visible,] # FIXME: use subset() instead?
          if (input$colorBy != "(none)") {
            cm <- colormap(gg[[input$colorBy]])
            drawPalette(colormap=cm, zlab=input$colorBy)
            plotTS(gg, pch=1, cex=0.3, col=cm$zcol, mar=c(3, 3, 2, 5.5))
          } else {
            plotTS(gg, pch=1, cex=0.3)
          }
        } else {
          gg@data$payload1 <- g@data$payload1[visible,] # FIXME: use subset() instead?
          if (input$colorBy != "(none)") {
            cm <- colormap(gg[[input$colorBy]])
            drawPalette(colormap=cm, zlab=input$colorBy)
            plotTS(gg, pch=1, cex=ifelse(flagged[visible], 1, 0.3), col=cm$zcol, mar=c(3, 3, 2, 5.5))
          } else {
            plotTS(gg, pch=1, cex=ifelse(flagged[visible], 1, 0.3))
          }
        }
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
        if (input$flagAction == "colourize") {
          hist(p[visible], breaks=100, main="p")
        } else if (input$flagAction == "omit") {
          hist(p[!flagged & visible], breaks=100, main="p trimmed to unflagged values")
        } else {
          stop("unhandled flagAction='", input$flagAction, "' detected by plot")
        }
      } else if (input$plotChoice == "hist(S)") {
        cat(file=stderr(), "  hist(S)\n", sep="")
        SA <- g[["SA"]]
        if (input$flagAction == "colourize") {
          hist(SA[visible], breaks=100, main="NOTE: colorization not sensible here")
        } else if (input$flagAction == "omit") {
          hist(SA[!flagged & visible], breaks=100, main="SA trimmed to unflagged values")
        } else {
          stop("unhandled flagAction='", input$flagAction, "' detected by plot")
        }
        cat(file=stderr(), "summary(SA):", summary(SA), "\n")
        cat(file=stderr(), "summary(SA[!flagged]):", summary(SA[!flagged]), "\n")
      }
      plotExists <<- TRUE
    }
  }) # output$plot

} # server

shinyApp(ui=ui, server=server)

