#' Visualization of Binning
#'
#' This function visualizes the binning results generated via \code{\line{woebin}}
#' @name woebin_plot
#' @param bins binning generated via \code{\line{woebin}}
#' @export
#' @examples
#' data(germancredit)
#' bins <- woebin(germancredit, y="creditability")$bins
#'
#' plotlist <- woebin_plot(bins)
#' plotlist
#'
woebin_plot <- function(bins) {

  pf <- function(bin) {
    # data
    dat <- bin[,.(
      variable, bin, good, bad, counts=good+bad, badprob, woe
    )][, `:=`(
      bin = ifelse(is.na(bin), "NA", bin),
      badprob2 = badprob*max(counts),
      badprob = round(badprob,4),
      rowid = as.integer(row.names(.SD))
    )][, bin := factor(bin, levels = bin)]

    dat_melt <- melt(dat, id.vars = c("variable", "bin","rowid"), measure.vars =c("good", "bad"), variable.name = "goodbad")[
      ,goodbad:=factor(goodbad, levels=c( "bad", "good"))
      ]

    # plot
    ggplot() +
      geom_bar(data=dat_melt, aes(x=bin, y=value, fill=goodbad), stat="identity") +
      geom_text(data=dat, aes(x = bin, y = counts, label = counts), vjust = -0.5) +
      geom_line(data=dat, aes(x = rowid, y = badprob2), colour = "blue") +
      geom_point(data=dat, aes(x = rowid, y=badprob2), colour = "blue", shape=21, fill="white") +
      geom_text(data=dat, aes(x = rowid, y = badprob2, label = badprob), colour="blue", vjust = -0.5) +
      scale_y_continuous(sec.axis = sec_axis(~./max(dat$counts), name = "Bad probability")) +
      labs(title = dat[1, variable], x=NULL, y="Bin count", fill=NULL) +
      theme_bw() +
      theme(legend.position="bottom", legend.direction="horizontal")

  }

  # plot export
  plotlist <- list()
  if (!is.data.frame(bins) & is.list(bins)) {
    bins_length <- length(bins)
    for (i in 1:bins_length) plotlist[[bins[[i]][1,variable]]] <- pf(bins[[i]])
  } else if (is.data.frame(bins)) {
    bins_length <- 1
    plotlist[[bins[1,variable]]] <- pf(bins)
  }

  return(plotlist)
}


#' binning adjustment
#'
#' This function provides a shiny interface for binning adjustment.
#' @name woebin_adj
#' @param dt name of input data frame.
#' @param y name of y variable.
#' @param x names of x variables, default: all column names except y.
#' @export
#' @examples
#' data(germancredit)
#' woebin_adj(germancredit, y="creditability")
#'
woebin_adj <- function(dt, y, x="") {
  if (x=="") x <- setdiff(names(dt), y)


  # server ------
  server <- function(input, output) {

    # binning <- reactive({
    #   woebin(dt[, c(y, input$variable)], y, stop_limit = input$stop_limit)[[1]]
    # })


    # Show the first "n" observations
    output$distPlot <- renderPlot({
      binning <- woebin(dt[, c(y, input$variable)], y, stop_limit = input$stop_limit)$bins[[1]]
      woebin_plot(binning)
    })

    output$bins <- renderPrint({
      binning <- woebin(dt[, c(y, input$variable)], y, stop_limit = input$stop_limit)$bins[[1]]
      binning[, .(bin)]
    })

    output$table <- renderTable({
      binning <- woebin(dt[, c(y, input$variable)], y, stop_limit = input$stop_limit)$bins[[1]]
      binning[, `:=`(bin=NULL, bstbin = NULL, bstbrkp = NULL)]
    })

  }

  # ui ------
  ui <- fluidPage(

    # Application title
    headerPanel("Binning Adjustment"),


    # Sidebar with a slider input for number of observations
    sidebarPanel(
      selectInput("variable", "1.Variable:", x),
      numericInput("stop_limit", "2.stop_limit", value = 0.1, step = 0.01)
      # min_perc_total=0.02, stop_limit=0.1

    ),

    # Show a plot of the generated distribution
    mainPanel(
      h4("bad probability by bins"),
      plotOutput("distPlot"),

      h4("bin"),
      verbatimTextOutput("bins"),

      h4("binning table"),
      tableOutput("table")
    )
  )

  # shinyApp ------
  shinyApp(ui = ui, server = server)

}

