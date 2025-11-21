# Lade notwendige Bibliotheken
# (Stellen Sie sicher, dass diese Pakete in Ihrer R-Umgebung installiert sind: install.packages(c("shiny", "dplyr", "ggplot2", "tidyr")))
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

# UI Definition (Benutzeroberfläche)
ui <- fluidPage(
  # Titel der App
  titlePanel("Vergleich von Barwert und Hyperbolischem Diskontieren"),
  
  # Seitenleiste für die Eingaben
  sidebarLayout(
    sidebarPanel(
      # Eingabe für maximale Zeit (t) - als Slider
      sliderInput("t_max",
                  "Maximale Laufzeit (t):",
                  min = 10,
                  max = 100,
                  value = 20,
                  step = 5),
      
      # Eingabe für Diskontierungsrate (i) - als numerisches Feld
      numericInput("i_rate",
                   "Diskontierungsrate (i):",
                   value = 0.10,
                   min = 0.01,
                   max = 0.5,
                   step = 0.01),
      
      # Eingabe für Rendite (r) - als numerisches Feld
      numericInput("r_rate",
                   "Rendite (r):",
                   value = 0.07,
                   min = 0.01,
                   max = 0.5,
                   step = 0.01),
      
      # Zusatzinformationen
      p(strong("Annahmen:")),
      p("- Kosten der Aktion: 100 (fix)"),
      p("- Ertrag der Aktion: Kosten * (1 + r)")
    ),
    
    # Hauptpanel für die Ausgabe (Plot)
    mainPanel(
      plotOutput("diskontierungsPlot")
    )
  )
)

# Server-Logik
server <- function(input, output) {
  
  # Reaktiv: Datenrahmen basierend auf Eingaben berechnen
  data_df <- reactive({
    # Benutzerdefinierte Variablen aus den UI-Inputs
    t_max_val <- input$t_max
    i_val <- input$i_rate
    r_val <- input$r_rate
    kosten <- 100 # Kosten sind im Originalcode fix
    
    # 1. Datenrahmen erstellen und Barwert-Werte berechnen
    df <- data.frame(t = 0:t_max_val) %>%
      mutate(
        Kosten = kosten,
        Ertrag = Kosten * (1 + r_val),
        
        # Standard-Barwert (exponentielles Diskontieren)
        Barwert = -Kosten / (1 + i_val)^t + Ertrag / (1 + i_val)^(t + 1),
        
        # Hyperbolischer Barwert (näherungsweise)
        Barwert_hyper = -Kosten / (1 + i_val * t) + Ertrag / (1 + i_val * (t + 1)),
        
        # Die Verhältnisse aus dem Originalcode werden ebenfalls berechnet
        Verhaeltnis_1 = (Kosten / (1 + i_val)^t) / (Ertrag / (1 + i_val)^(t + 1)),
        Verhaeltnis_2 = (Kosten / (1 + i_val * t)) / (Ertrag / (1 + i_val * (t + 1)))
      )
    
    # 2. Pivot Longer für ggplot, um die Barwert-Funktionen zu plotten
    df_long <- df %>%
      pivot_longer(
        cols = starts_with("Barwert"), # Wir brauchen nur die Barwert-Werte für den Plot
        names_to = "Funktion",
        values_to = "Werte"
      )
    
    return(df_long)
  })
  
  # Output: Plot generieren
  output$diskontierungsPlot <- renderPlot({
    df_long <- data_df()
    
    # Plot erstellen
    df_long %>%
      ggplot(aes(t, Werte, color = Funktion)) +
      geom_line(linewidth = 1.2) + # Linie etwas dicker machen
      geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      labs(
        title = "Vergleich: Exponentielles vs. Hyperbolisches Diskontieren",
        subtitle = paste0("i = ", input$i_rate, ", r = ", input$r_rate),
        x = "Startzeitpunkt der Aktion (t)",
        y = "Bewertung (Barwert/Gegenwartswert)",
        color = "Diskontierungsmodell"
      ) +
      scale_color_manual(
        values = c("Barwert" = "#1e90ff", "Barwert_hyper" = "#3cb371"),
        labels = c("Barwert" = "Exponentiell", "Barwert_hyper" = "Hyperbolisch")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom"
      )
  })
}

# Shiny App starten
shinyApp(ui = ui, server = server)