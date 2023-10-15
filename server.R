server <- function(input, output, session) {
  Forecast('forecast')
  Comparison('comparison')
}