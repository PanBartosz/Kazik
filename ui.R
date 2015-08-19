library(shiny)
library(rCharts)
options(RCHART_LIB = 'polycharts')

shinyUI(pageWithSidebar(
  headerPanel("Efekt Knobe'a i efekt epistemiczny w języku polskim"),
  sidebarPanel(
    tabsetPanel(
      tabPanel("Efekt Knobe'a'", 
    h4("Wybierz przysłówki:"),
    checkboxInput("brak", "Brak przysłówka", value=TRUE),
    checkboxInput("celowo", "Celowo", value=TRUE),
    checkboxInput("chcial", "Chciał", value=TRUE),
    checkboxInput("specjalnie", "Specjalnie", value=TRUE),
    checkboxInput("swiadomie", "Świadomie", value=TRUE),
    checkboxInput("umyslnie", "Umyślnie", value=TRUE),
    checkboxInput("zrozmyslem", "Z rozmysłem", value=TRUE),
    checkboxInput("zamierzal", "Zamierzał", value=TRUE),
    h4("Wybierz sposób prezentacji:"),
    selectInput("present", "Prezentacja:",
                c("Grupy" = "groups",
                  "I-modyfikatory" = "imodifiers")), value = "intentionality"
      ),
    tabPanel("Efekt epistemiczny", 
             h4("Wybierz czasownik:"),
             checkboxInput("sadzic", "Sądzić", value=TRUE),
             checkboxInput("wiedziec", "Wiedzieć", value=TRUE),
             h4("Wybierz formę:"),
             checkboxInput("agentywna", "..że zaszkodzi/pomoże...", value=TRUE),
             checkboxInput("kauzalna", "..że program zaszkodzi/pomoże...", value=TRUE),
             checkboxInput("stan", "..że stan środowiska się pogorszy/polepszy...", value=TRUE),
             h4("Wybierz sposób prezentacji:"),
             selectInput("present2", "Prezentacja:",
                         c("Grupy" = "groups",
                           "E-modyfikatory" = "emodifiers")), value = "epistemic"
    ), id = "tab")
  ,width = c(3)),
  mainPanel(
    showOutput("barchart", lib = "polycharts")
  )
))

