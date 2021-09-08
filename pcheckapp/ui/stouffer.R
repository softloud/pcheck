stouffer_tab <-

tabItem(
  tabName = "stouffer",

# input -------------------------------------------------------------------
  column(
    width = 4,
    box(
      title = "Enter p-values to test",
      width = 12,
      solidHeader = TRUE,
      p("Copy and paste p-values from table or type manually.
        Decimals indicated with a period, i.e. 3.65 or 4.2.
        p-values can be separated by space,
        commas, newline, or anything that is not a number."),
      textInput(
        inputId = "stouff_input",
        label = "p-values"
      )
    )
  ),


# results -----------------------------------------------------------------
  column(
    width = 8,
    box(
      title = "p-values entered",
      width = 3,
      align = "center",
      solidHeader = TRUE,
      tableOutput("stouff_pvals_check")
    ),
    box(
      title = "Stouffer's combined p-value measure",
      width = 9,
      solidHeader = TRUE,
      uiOutput("stouffer_result")
    )
  )

)
