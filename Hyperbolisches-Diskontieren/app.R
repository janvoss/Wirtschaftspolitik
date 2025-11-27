# Lade notwendige Bibliotheken
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
    
    # Hauptpanel für die Ausgabe (Plot und Text)
    mainPanel(
      plotOutput("diskontierungsPlot"),
      hr(), # Horizontale Trennlinie
      htmlOutput("tc_output") # Ausgabe für den t_c Wert
    )
  )
)

# Server-Logik
server <- function(input, output) {
  
  # 1. Reaktiv: Datenrahmen basierend auf Eingaben berechnen (Unverändert)
  data_df <- reactive({
    t_max_val <- input$t_max
    i_val <- input$i_rate
    r_val <- input$r_rate
    kosten <- 100 
    
    df <- data.frame(t = 0:t_max_val) %>%
      mutate(
        Kosten = kosten,
        Ertrag = Kosten * (1 + r_val),
        Barwert = -Kosten / (1 + i_val)^t + Ertrag / (1 + i_val)^(t + 1),
        Barwert_hyper = -Kosten / (1 + i_val * t) + Ertrag / (1 + i_val * (t + 1)),
        Verhaeltnis_1 = (Kosten / (1 + i_val)^t) / (Ertrag / (1 + i_val)^(t + 1)),
        Verhaeltnis_2 = (Kosten / (1 + i_val * t)) / (Ertrag / (1 + i_val * (t + 1)))
      )
    
    df_long <- df %>%
      pivot_longer(
        cols = starts_with("Barwert"), 
        names_to = "Funktion",
        values_to = "Werte"
      )
    
    return(df_long)
  })
  
  # 2. Reaktiv: Cross-over time (t_c) berechnen
  tc_calc <- reactive({
    i <- input$i_rate
    r <- input$r_rate
    
    if (i > r) {
      t_c <- (i - r) / (i * r)
      # Nur positive t_c Werte sind sinnvoll
      return(ifelse(t_c >= 0, t_c, NA))
    } else {
      return(NA)
    }
  })
  
  # 3. Output: Plot generieren
  output$diskontierungsPlot <- renderPlot({
    df_long <- data_df()
    t_c <- tc_calc() # t_c aus der reaktiven Berechnung abrufen
    
    i <- input$i_rate
    r <- input$r_rate
    
    # Basis-Plot erstellen
    p <- df_long %>%
      ggplot(aes(t, Werte, color = Funktion)) +
      geom_line(linewidth = 1.2) + 
      geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      labs(
        title = "Vergleich: Exponentielles vs. Hyperbolisches Diskontieren",
        subtitle = paste0("i = ", i, ", r = ", r),
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
    
    # Bedingtes Hinzufügen von geom_vline
    if (!is.na(t_c)) {
      p <- p + geom_vline(xintercept = t_c, linetype = "dotted", color = "darkgreen", linewidth = 1)
    }
    
    print(p)
  })
  
  # 4. Output: Textausgabe für t_c
  output$tc_output <- renderUI({
    t_c <- tc_calc() # t_c aus der reaktiven Berechnung abrufen
    
    if (!is.na(t_c)) {
      # Wenn t_c gültig ist, geben wir den formatierten Text aus
      text <- paste0("Bei hyperbolischem Diskontieren erscheint die Aktion als vorteilhaft, wenn sie mindestens im Zeitpunkt ", 
                     round(t_c, 2), 
                     " liegt.")
      # Verwenden von HTML, um Fettdruck und Symbole zu ermöglichen
      HTML(text) 
    } else {
      # Wenn t_c nicht gültig ist (i <= r), geben wir eine Erklärung aus
      HTML("Hinweis: Die Diskontierungsrate (i) ist nicht größer als die Rendite (r).")
    }
  })
}

# Shiny App starten
shinyApp(ui = ui, server = server)