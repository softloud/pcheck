# packages ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(DT)
library(purrr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(glue)

# ui tabs -----------------------------------------------------------------

source("ui/plausibility.R")
source("ui/stouffer.R")


# Define UI ----
ui <-
  dashboardPage(
    skin = "black",


    # ui header ---------------------------------------------------------------

    dashboardHeader(title = "P-value checking tools",
                    # puts sidebar toggle on right
                    titleWidth = "calc(100% - 44px)"),



    # ui tabs -----------------------------------------------------------------
    dashboardSidebar(sidebarMenu(
      id = "tabs",

      menuItem(
        "Plausibility",
        tabName = "plausibility",
        icon = icon("question")
      ),

      menuItem("Stouffer",
               tabName = "stouffer",
               icon = icon("plus"))

    )),


    # ui body -----------------------------------------------------------------


    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(
        # links to files in www/
        tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(src = "custom.js")
      ),

      tabItems(plausibility_tab,
               stouffer_tab)
    )
  )


# Define server logic ----
server <- function(input, output, session) {
  groups <- reactiveVal(list())


# demo --------------------------------------------------------------------

observeEvent(input$demo, {
  list(
    A = list(mu = 1, sigma = 0.4, n = 10),
    B  = list(mu = 1.2, sigma = 0.2, n = 20),
    C = list(mu = 1.5, sigma = 1, n = 15)
  ) %>%
    groups()
})
  # set current summary -----------------------------------------------------

  # tabulated_groups --------------------------------------------------------

  output$tabulated_groups <- renderDT({
    # tabulated summary of current groups
    groups() %>%
      bind_rows() %>%
      mutate(group = names(groups())) %>%
      select(group, everything()) %>%
      rename(
        Group = group,
        Mean = mu,
        SD = sigma,
        N = n

      )

  },
  options = list(lengthChange = FALSE,
                 info = FALSE,
                 ordering = FALSE,
                 pageLength = 20,
                 searching = FALSE,
                 paging = FALSE
                 ),
  rownames = TRUE,
  selection = "single")

  # add group ---------------------------------------------------------------

  observeEvent(input$add_group, {
    current_groups <- groups()

    # set new group object
    new_group <- list(mu = input$mu,
                      sigma = input$sigma,
                      n = input$n)

    # update groups
    current_groups[[input$group_name]] <- new_group

    groups(current_groups)

    # reset group interface
    reset("n")
    reset("mu")
    reset("sigma")

    # update group name
    updateTextInput(session = session,
                    inputId = "group_name",
                    value = LETTERS[length(groups()) + 1])

  })


  # edit_group --------------------------------------------------------------

  observeEvent(input$tabulated_groups_rows_selected, {
    idx <- input$tabulated_groups_rows_selected[[1]]

    message("idx = ", idx)

    group_displayed <- groups()[[idx]]

    group_name <- names(groups())[[idx]]

    # update group name
    updateTextInput(session,
                    "group_name",
                    value = group_name)
    updateNumericInput(session, "mu",
                       value = group_displayed$mu)
    updateNumericInput(session, "sigma",
                       value = group_displayed$sigma)
    updateNumericInput(session, "n", value = group_displayed$n)


  })

  observeEvent(input$remove_group, {
    current_groups <- groups()

    current_groups[[input$group_name]] <- NULL

    groups(current_groups)

  })


# simulation --------------------------------------------------------------

pvals <- reactiveVal()

observeEvent(input$run_sim, {
  p_sim(groups(), trials = input$trials) %>%
    pvals()
})

output$group_densities <- renderPlot(p_groups_plot(groups()))

output$sim_results <-
  renderPlot({
    req(pvals())
    p_sim_plot(pvals(), input$reported_p, input$trials)
  })


# stouffer ----------------------------------------------------------------
stouff_pvals <- reactiveVal()

observeEvent(input$stouff_input, {
  clean_pvals(input$stouff_input) %>% stouff_pvals()

})


# stouff_pvals_check ------------------------------------------------------

output$stouff_pvals_check <- renderTable({
  tibble(
    p = stouff_pvals()
  )
})

output$stouffer_result <- renderUI({
  h2(calc_stouffer(stouff_pvals()))
})



}

# Run the application ----
shinyApp(ui = ui, server = server)
