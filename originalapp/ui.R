fluidPage(
  h1("Plausibility of reported significance of difference of two groups"),
  sidebarLayout(sidebarPanel(
  inputPanel(
    h1("Reported p-value"),
    numericInput("reportedp", NULL, 0.04, min = 0, max = 1)
  ),
  h2("Expected group parameters"),
  inputPanel(
    h3("Group A"),
    numericInput("an", "Sample size", 43),
    numericInput("amu", "Mean", 1),
    numericInput("asigma", "Standard deviation", 0.2, min = 0)
  ),
  inputPanel(
    h3("Group B"),
    numericInput("bn", "Sample size", 32),
    numericInput("bmu", "Mean", 1),
    numericInput("bsigma", "Standard deviation", 0.4, min = 0)
  ),
  h2("Simulation parameters"),
  inputPanel(
    numericInput("trials", "Trials", 1000),
    p("A single trial draws a random sample,
    of reported sample size, from the distribution of each group,
      performs a t-test, and produces a p-value.")
  )
),
mainPanel(
plotOutput("densities"),
plotOutput("pvals")
))
)
