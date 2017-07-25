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

  pfun <- function(bins) {

    dat <- bins[,.(
      variable, bin, counts=good+bad, badprob
    )][, `:=`(
      bin = ifelse(is.na(bin), "NA", bin),
      badprob2 = badprob*max(counts),
      badprob = round(badprob,4),
      rowid = as.integer(row.names(.SD))
    )][, bin := factor(bin, levels = bin)]


    ggplot(dat) +
      geom_bar(aes(x=bin, y=counts), stat = "identity", colour = "black", fill="white") +
      geom_text(aes(x = bin, y = counts, label = counts), vjust = -0.5) +
      geom_line(aes(x = rowid, y = badprob2), colour = "red") +
      geom_point(aes(x = rowid, y=badprob2), colour = "red", shape=21, fill="white") +
      geom_text(aes(x = rowid, y = badprob2, label = badprob), colour = "red", vjust = -0.5) +
      labs(title = dat[1, variable], x=NULL, y=NULL) + theme_bw()

  }

  # server ------
  server <- function(input, output) {

    # binning <- reactive({
    #   woebin(dt[, c(y, input$variable)], y, stop_limit = input$stop_limit)[[1]]
    # })


    # Show the first "n" observations
    output$distPlot <- renderPlot({
      binning <- woebin(dt[, c(y, input$variable)], y, stop_limit = input$stop_limit)[[1]]
      pfun(binning)
    })

    output$bins <- renderPrint({
      binning <- woebin(dt[, c(y, input$variable)], y, stop_limit = input$stop_limit)[[1]]
      binning[, .(bin)]
    })

    output$table <- renderTable({
      binning <- woebin(dt[, c(y, input$variable)], y, stop_limit = input$stop_limit)[[1]]
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

