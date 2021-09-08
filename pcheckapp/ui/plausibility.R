plausibility_tab <-

  tabItem(tabName = "plausibility",

          # plausibility selectors --------------------------------------------------

          column(
            width = 4,

            # load demo ---------------------------------------------------------------

            box(
              width = 12,
              numericInput(
                inputId = "reported_p",
                label = "Reported p-value",
                value = 0.4,
                min = 0,
                max = 1
              ),
              actionButton(inputId = "demo",
                           label = "Load demo"),
              actionButton(inputId = "run_sim",
                           label = "Run simulations"),
              numericInput(
                inputId = "trials",
                label = "Number of simulations run",
                value = 100,
                min = 0,
                max = 5000
              )
            ),

            # group selection summary info
            box(
              title = "Current groups",
              solidHeader = TRUE,
              width = 12,
              DTOutput("tabulated_groups")
            ),
            # set number of groups
            box(
              title = "Group information",
              solidHeader = TRUE,
              width = 12,
              textInput("group_name", "Group name", "A"),
              numericInput("mu", "Mean", NULL),
              numericInput("sigma", "Standard deviation", NULL, min = 0),
              numericInput("n", "Sample size", NULL, min = 0),
              actionButton(
                inputId = "add_group",
                label = "Add group",
                icon = icon("plus")
              ),
              actionButton(
                inputId = "remove_group",
                label = "Remove group",
                icon = icon("minus")
              )

            )
          ),


          # plausibility results ----------------------------------------------------
          column(
            width = 8,

            box(
              title = "Densities",
              solidHeader = TRUE,
              width = 12,
              plotOutput("group_densities", height = "300px")
            ),
            box(
              title = "Simulation results",
              solidHeader = TRUE,
              width = 12,
              plotOutput("sim_results")
            )


          ))
