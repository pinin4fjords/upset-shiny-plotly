library(shiny)
library(shinydashboard)
library(plotly)
library(UpSetR)

setlimit <- 20

ui <-
  dashboardPage(
    dashboardHeader(title = "UpSet with Plotly"),
    dashboardSidebar(
      upset_fields <-
        list(
          fileInput(
            "file1",
            "Choose CSV File",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain", ".csv")
          ),
          downloadLink("example_data", "Example data"),
          tags$hr(),
          uiOutput("sets"),
          selectInput(
            "intersection_assignment_type",
            "Intersection assignment type",
            choices = c(
              `Highest-order (UpSet)` = "upset",
              `All associated intersections` = "all"
            ),
            selected = "upset"
          ),
          uiOutput("nsets"),
          sliderInput(
            "nintersects",
            label = "Number of intersections",
            min = 2,
            max = 40,
            step = 1,
            value = 20
          ),
          checkboxInput("set_sort", "Sort sets by size?", value = TRUE),
          checkboxInput("bar_numbers",
                        "Show bar numbers?", value = FALSE),
          checkboxInput(
            "show_empty_intersections",
            label = "Show empty intersections?",
            value = TRUE
          )
        )
    ),
    dashboardBody(
      h3("Intersection plots"),
      wellPanel(uiOutput('upset_credits')),
      plotlyOutput("plotly_upset", height = "600px")
    )
  )

server <- function(input, output) {
  # Render a control for users to pick from their supplied sets
  
  output$sets <- renderUI({
    valid_sets <- getValidSets()
    req(!is.null(valid_sets))
    
    selectInput(
      "sets",
      "Sets",
      choices = names(valid_sets),
      selectize = TRUE,
      multiple = TRUE,
      selected = names(valid_sets)
    )
  })
  
  # Render a control to decide how many sets to consider in the plot
  
  output$nsets <- renderUI({
    selected_sets <- getSelectedSets()
    req(!is.null(selected_sets))
    
    max_sets <-
      ifelse(length(selected_sets) > setlimit,
             setlimit,
             length(selected_sets))
    sliderInput(
      "nsets",
      label = "Number of sets to include in plot",
      min = 2,
      max = max_sets,
      step = 1,
      value = min(10, max_sets)
    )
  })
  
  ############################################################################# Form accessors
  
  # Accessor for user-selected sets
  
  getSelectedSetNames <- reactive({
    req(input$sets)
    input$sets
  })
  
  # Accessor for the nsets parameter
  
  getNsets <- reactive({
    req(!is.null(input$nsets))
    input$nsets
  })
  
  # Accessor for the nintersections parameter
  
  getNintersections <- reactive({
    validate(need(!is.null(input$nintersects), "Waiting for nintersects"))
    input$nintersects
  })
  
  getShowEmptyIntersections <- reactive({
    validate(need(
      !is.null(input$show_empty_intersections),
      "Waiting for empty intersections option"
    ))
    input$show_empty_intersections
  })
  
  # Accessor for the intersection assignment type
  
  getIntersectionAssignmentType <- reactive({
    validate(need(
      !is.null(input$intersection_assignment_type),
      "Waiting for group_by"
    ))
    input$intersection_assignment_type
  })
  
  # Set sorting
  
  getSetSort <- reactive({
    validate(need(!is.null(input$set_sort), "Waiting for set_sort"))
    input$set_sort
  })
  
  # Bar numbers
  
  getBarNumbers <- reactive({
    validate(need(!is.null(input$bar_numbers), "Waiting for bar numbers"))
    input$bar_numbers
  })
  
  ############################################################################# The business end- derive sets and pass for intersection
  
  # Get the input file
  
  getInfile <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      # Look for example data to use by default
      
      if (file.exists(system.file("extdata", "movies.csv", package = "UpSetR"))) {
        filename <- system.file("extdata", "movies.csv", package = "UpSetR")
      } else if (file.exists('movies.csv')) {
        filename <- 'movies.csv'
      } else {
        filename <- NULL
      }
    } else {
      filename <- inFile$datapath
    }
  })
  
  # Read input data
  
  readInputData <- reactive({
    filename <- getInfile()
    req(!is.null(filename))
    read.csv(filename, sep = ";")
  })
  
  # Read input sets
  
  getValidSets <- reactive({
    withProgress(message = "Deriving input sets", value = 0, {
      filename <- getInfile()
      
      if (is.null(filename)) {
        NULL
      } else {
        setdata <- readInputData()
        logical_cols <-
          colnames(setdata)[apply(setdata, 2, function(x)
            all(x %in% c(0, 1)))]
        names(logical_cols) <- logical_cols
        
        lapply(logical_cols, function(x)
          which(setdata[[x]] == 1))
      }
    })
  })
  
  # Subset sets to those selected
  
  getSelectedSets <- reactive({
    valid_sets <- getValidSets()
    validate(need(!is.null(valid_sets), "Please upload data"))
    chosen_sets <- getSelectedSetNames()
    sets <- valid_sets[chosen_sets]
    if (getSetSort()) {
      sets <- sets[order(unlist(lapply(sets, length)))]
    }
    sets
  })
  
  # Get the sets we're going to use based on nsets
  
  getSets <- reactive({
    selected_sets <- getSelectedSets()
    req(length(selected_sets) > 0)
    
    nsets <- getNsets()
    selected_sets[1:min(nsets, length(selected_sets))]
  })
  
  # Calculate intersections between sets
  
  calculateIntersections <- reactive({
    selected_sets <- getSets()
    
    withProgress(message = "Calculating set intersections", value = 0, {
      sets <- getSets()
      nsets <- length(sets)
      
      # Get all possible combinations of sets
      
      combinations <- function(items, pick) {
        x <- combn(items, pick)
        lapply(seq_len(ncol(x)), function(i)
          x[, i])
      }
      
      assignment_type <- getIntersectionAssignmentType()
      
      # No point starting at size 1 in a non-upset plot
      
      startsize <- ifelse(assignment_type == "upset", 1, 2)
      
      combos <- lapply(startsize:nsets, function(x) {
        combinations(1:length(selected_sets), x)
      })
      
      # Calculate the intersections of all these combinations
      
      withProgress(message = "Running intersect()", value = 0, {
        intersects <- lapply(combos, function(combonos) {
          lapply(combonos, function(combo) {
            Reduce(intersect, selected_sets[combo])
          })
        })
      })
      
      # For UpSet-ness, membership of higher-order intersections takes priority Otherwise just return the number of entries in each intersection
      
      intersects <- lapply(1:length(intersects), function(i) {
        intersectno <- intersects[[i]]
        members_in_higher_levels <-
          unlist(intersects[(i + 1):length(intersects)])
        lapply(intersectno, function(intersect) {
          if (assignment_type == "upset") {
            length(setdiff(intersect, members_in_higher_levels))
          } else {
            length(intersect)
          }
        })
      })
      
      combos <- unlist(combos, recursive = FALSE)
      intersects <- unlist(intersects)
      
      if (!getShowEmptyIntersections()) {
        combos <- combos[which(intersects > 0)]
        intersects <- intersects[which(intersects > 0)]
      }
      
      # Sort by intersect size
      
      combos <- combos[order(intersects, decreasing = TRUE)]
      intersects <-
        intersects[order(intersects, decreasing = TRUE)]
      list(combinations = combos, intersections = intersects)
      
    })
    
  })
  
  ########################################################################### Render the plot with it separate components
  
  output$plotly_upset <- renderPlotly({
    grid <- upsetGrid()
    set_size_chart <- upsetSetSizeBarChart()
    intersect_size_chart <- upsetIntersectSizeBarChart()
    
    # Hide tick labels on the grid
    
    # Unfortunately axis titles get hidden on the subplot. Not sure why.
    
    intersect_size_chart <-
      intersect_size_chart %>% layout(yaxis = list(title = "Intersections size"))
    
    # The y axis labels of the
    
    s1 <-
      subplot(
        plotly_empty(type = "scatter", mode = "markers"),
        plotly_empty(type = "scatter", mode = "markers"),
        plotly_empty(type = "scatter", mode = "markers"),
        set_size_chart,
        nrows = 2,
        widths = c(0.6, 0.4)
      )
    s2 <-
      subplot(intersect_size_chart,
              grid,
              nrows = 2,
              shareX = TRUE) %>% layout(showlegend = FALSE)
    
    subplot(s1, s2, widths = c(0.3, 0.7))
    
    
  })
  
  # Add some line returns to contrast names
  
  getSetNames <- reactive({
    selected_sets <- getSets()
    gsub("_", " ", names(selected_sets))
  })
  
  # Make the grid of points indicating set membership in intersections
  
  upsetGrid <- reactive({
    selected_sets <- getSets()
    ints <- calculateIntersections()
    
    intersects <- ints$intersections
    combos <- ints$combinations
    
    # Reduce the maximum number of intersections if we don't have that many
    
    nintersections <- getNintersections()
    nintersections <- min(nintersections, length(combos))
    
    # Fetch the number of sets
    
    nsets <- getNsets()
    setnames <- getSetNames()
    
    lines <-
      data.table::rbindlist(lapply(1:nintersections, function(combono) {
        data.frame(
          combo = combono,
          x = rep(combono, max(2, length(combos[[combono]]))),
          y = (nsets - combos[[combono]]) + 1,
          name = setnames[combos[[combono]]]
        )
      }))
    
    plot_ly(
      type = "scatter",
      mode = "markers",
      marker = list(color = "lightgrey", size = 8)
    ) %>% add_trace(
      type = "scatter",
      x = rep(1:nintersections,
              length(selected_sets)),
      y = unlist(lapply(1:length(selected_sets), function(x)
        rep(x - 0.5, nintersections))),
      hoverinfo = "none"
    ) %>% add_trace(
      type = "scatter",
      data = group_by(lines, combo),
      mode = "lines+markers",
      x = lines$x,
      y = lines$y - 0.5,
      line = list(color = "black", width = 3),
      marker = list(color = "black",
                    size = 10),
      hoverinfo = "text",
      text = ~ name
    ) %>% layout(
      xaxis = list(
        showticklabels = FALSE,
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        showticklabels = FALSE,
        showgrid = TRUE,
        range = c(0, nsets),
        zeroline = FALSE,
        range = 1:nsets
      ),
      margin = list(t = 0, b = 40)
    )
    
  })
  
  # Make the bar chart illustrating set sizes
  
  upsetSetSizeBarChart <- reactive({
    setnames <- getSetNames()
    selected_sets <- getSets()
    
    plot_ly(
      x = unlist(lapply(selected_sets, length)),
      y = setnames,
      type = "bar",
      orientation = "h",
      marker = list(color = "black")
    ) %>% layout(
      bargap = 0.4,
      yaxis = list(
        categoryarray = rev(setnames),
        categoryorder = "array"
      )
    )
  })
  
  # Make the bar chart illustrating intersect size
  
  upsetIntersectSizeBarChart <- reactive({
    ints <- calculateIntersections()
    intersects <- ints$intersections
    combos <- ints$combinations
    nintersections <- getNintersections()
    
    p <-
      plot_ly(showlegend = FALSE) %>% add_trace(
        x = 1:nintersections,
        y = unlist(intersects[1:nintersections]),
        type = "bar",
        marker = list(color = "black",
                      hoverinfo = "none")
      )
    
    bar_numbers <- getBarNumbers()
    
    if (bar_numbers) {
      p <-
        p %>% add_trace(
          type = "scatter",
          mode = "text",
          x = 1:nintersections,
          y = unlist(intersects[1:nintersections]) + (max(intersects) * 0.05),
          text = unlist(intersects[1:nintersections]),
          textfont = list(color = "black")
        )
    }
    
    p
  })
  
  # Provide the example data for download
  
  output$example_data <- downloadHandler(
    filename = function() {
      paste("movies-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- readInputData()
      
      write.csv(data, file)
    }
  )
  
  # Don't make out like UpSet is my work....
  
  output$upset_credits <- renderUI({
    list(p(
      HTML(
        "This is a Shiny and Plotly-driven plot based on the work of Lex, Gehlenborg et al on UpSet plots. See the <a href='http://www.caleydo.org/tools/upset/'>UpSet documentation</a> and <a href='http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html'>paper</a> for more information."
      )
    ),
    p(
      HTML(
        "UpSet already comes in an advanced <a href = 'http://vcg.github.io/upset/'>interactive form</a>, and a <a href='https://gehlenborglab.shinyapps.io/upsetr/'>basic UpSet Shiny</a> has been built by UpSet's authors. The motivation for making this Shiny app was just to create a more interactive version of an UpSet plot from within R, using <a href='https://plot.ly/r/'>Plotly</a> to produce the interactive elements from within Shiny."
      )
    ))
  })
}

shinyApp(ui, server)
