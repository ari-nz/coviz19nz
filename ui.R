

navbarPage("Coviz - Covid19 in NZ", id="nav",

           tabPanel("Interactive map",
                    div(class="outer",

                        tags$head(
                            # Include our custom CSS
                            includeCSS("www/style.css")
                        ),

                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),

                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 90, left = "auto", right = 50, bottom = "auto",
                                      width = 450, height = "auto",

                                      h2("Covid 19 cases in NZ"),

                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "adultpop"),
                                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      )

                        ),

                        tags$div(id="cite",
                                 'Data compiled from The Ministry of Health New Zealand & Stats NZ:',
                                 tags$em(tags$a("Covid Numbers, ", href = 'https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases')),
                                 tags$em(tags$a("Boundary Files, ", href = 'http://archive.stats.govt.nz/browse_for_stats/Maps_and_geography/Geographic-areas/digital-boundary-files.aspx')),
                                 tags$em(tags$a("Population Estimates", href = 'http://archive.stats.govt.nz/browse_for_stats/population/estimates_and_projections/subnational-pop-estimates-tables.aspx'))
                        )
                    )

           )

)

