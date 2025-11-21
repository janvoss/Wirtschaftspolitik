# Wirtschaftspsychologische Grundkonzepte

## Mental Accounting

s. Tafelaufschrieb

## Denken in Wahrscheinlichkeiten

- Es gibt hinweise darauf, dass in Entscheidungsprozessen Entscheidungen nicht auf Wahrscheinlichkeiten ($p$), sondern auf auf Wahrscheinlichkeiten beruhenden Gewichtungen ($w(p)$) basieren. 

- Hierbei gilt wohl, dass niedrige Wahrscheinlichkeiten ein systematisch zu hohes Gewicht erhalten ($w(p_{niedrig})>p_{niedrig}$) und hohe Wahrscheinlichkeiten ein zu niedriges ($w(p_{hoch})<p_{hoch}$)



::: {.cell}

```{.r .cell-code}
library(ggplot2)

# Definition der Gewichtungsfunktion nach Kahneman/Tversky
weighting_function <- function(p, gamma = 0.61) {
  p^gamma / (p^gamma + (1-p)^gamma)^(1/gamma)
}

# Daten erstellen
probabilities <- seq(0.01, 0.99, by = 0.01)
weights <- weighting_function(probabilities)
df <- data.frame(p = probabilities, w = weights)

# Plot erstellen
ggplot(df, aes(x = p)) +
  geom_line(aes(y = w), color = "blue", linewidth = 1) +  # Gewichtungsfunktion
  geom_line(aes(y = p), color = "black", linetype = "dashed") +  # Referenzdiagonale
  geom_ribbon(aes(ymin = p, ymax = ifelse(w > p, w, p)),
              fill = "green", alpha = 0.3) +  # Übergewichtungsbereich
  geom_ribbon(aes(ymin = ifelse(w < p, w, p), ymax = p),
              fill = "red", alpha = 0.3) +  # Untergewichtungsbereich
  labs(title = "Wahrscheinlichkeitsgewichtungsfunktion nach Kahneman & Tversky",
     #  subtitle = expression(paste("Parameter ", gamma, " = 0.61")),
       x = "Objektive Wahrscheinlichkeit (p)",
       y = "Subjektive Gewichtung w(p)",
       caption = "Abbildung: Jan S. Voßwinkel") +
  #scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  #scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_minimal()
```

::: {.cell-output-display}
![Wahrscheinlichkeitsgewichtung](Wirtschaftspsychologie_files/figure-html/unnamed-chunk-1-1.png){width=672}
:::
:::




- Dieser Effekt ist tendenziell für positive Ereignisse stärker ausgeprägt als für negative Ereignisse

- Eine mögliche Operationalisierung:  
  - $w(p) = \frac{p^{\gamma}}{\left[ p^{\gamma} + (1-p)^{\gamma} \right]^{1/\gamma}}$
  
  - Mit $\gamma_{positiv}<\gamma_{negativ}$


::: {.cell}

```{.r .cell-code}
library(ggplot2)

# Definition der Gewichtungsfunktion nach Kahneman/Tversky
weighting_function <- function(p, gamma) {
  p^gamma / (p^gamma + (1-p)^gamma)^(1/gamma)
}

# Daten erstellen
probabilities <- seq(0.01, 0.99, by = 0.01)
df <- data.frame(
  p = rep(probabilities, 2),
  w = c(weighting_function(probabilities, gamma = 0.61),
        weighting_function(probabilities, gamma = 0.69)),
  gamma = factor(rep(c("positive Ereignisse", "negative Ereignisse"), each = length(probabilities)))
)

# Plot erstellen
ggplot(df, aes(x = p, y = w, color = gamma)) +
  geom_line(linewidth = 1) +
  geom_line(aes(x = p, y = p), color = "black", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  labs(
    title = "Wahrscheinlichkeitsgewichtungsfunktion nach Kahneman & Tversky",
    subtitle="Unterschiedliche Gewichtungen bei positiven und negativen Ereignissen",
    x = "Objektive Wahrscheinlichkeit p",
    y = "Subjektive Gewichtung w(p)",
    color = "Ereignisart",
    caption = "Abbildung: Jan S. Voßwinkel"
  ) +
  theme_minimal()
```

::: {.cell-output-display}
![Wahrscheinlichkeitsgewichtung bei positiven und negativen Ereignissen](Wirtschaftspsychologie_files/figure-html/unnamed-chunk-2-1.png){width=672}
:::
:::




## Asymmetrie von Gewinnen und Verlusten



::: {.cell}

```{.r .cell-code}
library(ggplot2)

#  Sigmoid-Funktion
sigmoid_sym <- function(x, k, lambda) {
  ifelse(x>=0, x / (1 + abs(x)^k)^(1/k),
         (lambda)*x / (1 + abs(x)^k)^(1/k))
}

# Daten erzeugen
df <- data.frame(x = seq(-5, 5, length.out = 200))
df$y <- sigmoid_sym(df$x, k = .88, lambda=2.25)

# Hilfslinien und Achsen-Breaks
x_vals <- c(-3, -2, -1, 1, 2, 3)
y_vals <- sigmoid_sym(x_vals, k = .88, lambda=2.25)
lines_df <- data.frame(x = x_vals, y = y_vals)

p <- ggplot(df, aes(x, y)) +
  geom_line(size = 1.2, color = "blue") +
  # Vertikale Hilfslinien
  geom_segment(
    data = lines_df,
    aes(x = x, xend = x, y = 0, yend = y),
    linetype = "dashed", color = "red"
  ) +
  # Horizontale Hilfslinien
  geom_segment(
    data = lines_df,
    aes(x = x, xend = 0, y = y, yend = y),
    linetype = "dashed", color = "red"
  ) +
  # Achsen durch den Ursprung
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  # Achsen-Breaks
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2)) +
  scale_y_continuous(breaks = c(round(y_vals, 2), 0)) +
  labs(title = "Eine Nutzenfunktion nach der Prospect Theory",
       x = "Zugang von Gütern", y = "Nutzen") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.line = element_blank())

# p

p+ coord_cartesian(xlim = c(0, 5), ylim = c(0, 1))+
  ggtitle("Eine Nutzenfunktion")
```

::: {.cell-output-display}
![Eine Nutzenfunktion](Wirtschaftspsychologie_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::




::: {.cell}

```{.r .cell-code}
p
```

::: {.cell-output-display}
![Eine Wertefunktion nach der Prospect Theory](Wirtschaftspsychologie_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::
:::


## Status quo Bias

## Expressives Wählerverhalten


::: {.cell}

```{.python .cell-code}
# Python


A= 0
a= 0
B= 1
b= 0
C= 0
c= 1
D= -1
d= -1


from lets_plot import *

LetsPlot.setup_html()

ggplot() + \
geom_text(x=- .5, y= 2   , label=a) +\
geom_text(x=-1.5, y=  .75, label=A) +\
geom_text(x=- .5, y=-1   , label=b) +\
geom_text(x=-1.5, y=-2.25, label=B) +\
geom_text(x= 1.5, y= 2   , label=c) +\
geom_text(x=  .5, y=  .75, label=C) +\
geom_text(x= 1.5, y=-1   , label=d) +\
geom_text(x=  .5, y=-2.25, label=D) +\
geom_text(x=-1  , y= 3.3 , label='dagegen')+\
geom_text(x= 1  , y= 3.3 , label='dafür')+\
geom_text(x=-2.2, y= 1.375, angle=90, label='dagegen')+\
geom_text(x=-2.2, y=-1.625, angle=90, label='dafür')+\
geom_text(x= 0 , y= 4 ,
          fontface = "bold", label='Wähler 2')+\
geom_text(x=-2.7 , y= 0 , angle=90,
          fontface = "bold", label='Wähler 1')+\
geom_segment(x=-2,  xend= 2, y= 0, yend= 0) +\
geom_segment(x=-2,  xend= 2, y= 3, yend= 3) +\
geom_segment(x=-2,  xend= 2, y=-3, yend=-3) +\
geom_segment(x=-2,  xend=-2, y=-3, yend= 3) +\
geom_segment(x= 0,  xend= 0, y=-3, yend= 3) +\
geom_segment(x= 2,  xend= 2, y=-3, yend= 3) +\
theme_void()+\
labs(title='Auszahlungen bei expressivem Wählerverhalten')
```

::: {.cell-output-display}

```{=html}
<html lang="en">
   <head>
       <meta charset="UTF-8">
       <style> html, body { margin: 0; padding: 0; } </style>
       <script type="text/javascript" data-lets-plot-script="library" src="https://cdn.jsdelivr.net/gh/JetBrains/lets-plot@v4.6.2/js-package/distr/lets-plot.min.js"></script>
   </head>
   <body>
          <div id="GEz7wv"></div>
   <script type="text/javascript" data-lets-plot-script="plot">
   
   (function() {
   // ----------
   
   const forceImmediateRender = false;
   const responsive = false;
   
   let sizing = {
       width_mode: "MIN",
       height_mode: "SCALED",
       width: null, 
       height: null 
   };
   
   const preferredWidth = document.body.dataset.letsPlotPreferredWidth;
   if (preferredWidth !== undefined) {
       sizing = {
           width_mode: 'FIXED',
           height_mode: 'SCALED',
           width: parseFloat(preferredWidth)
       };
   }
   
   const containerDiv = document.getElementById("GEz7wv");
   let fig = null;
   
   function renderPlot() {
       if (fig === null) {
           const plotSpec = {
"mapping":{
},
"data_meta":{
},
"theme":{
"name":"classic",
"line":"blank",
"axis":"blank"
},
"ggtitle":{
"text":"Auszahlungen bei expressivem Wählerverhalten"
},
"kind":"plot",
"scales":[],
"layers":[{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":-0.5,
"y":2.0,
"label":0.0,
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":-1.5,
"y":0.75,
"label":0.0,
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":-0.5,
"y":-1.0,
"label":0.0,
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":-1.5,
"y":-2.25,
"label":1.0,
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":1.5,
"y":2.0,
"label":1.0,
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":0.5,
"y":0.75,
"label":0.0,
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":1.5,
"y":-1.0,
"label":-1.0,
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":0.5,
"y":-2.25,
"label":-1.0,
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":-1.0,
"y":3.3,
"label":"dagegen",
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":1.0,
"y":3.3,
"label":"dafür",
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":-2.2,
"y":1.375,
"angle":90.0,
"label":"dagegen",
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":-2.2,
"y":-1.625,
"angle":90.0,
"label":"dafür",
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":0.0,
"y":4.0,
"fontface":"bold",
"label":"Wähler 2",
"data":{
}
},{
"geom":"text",
"mapping":{
},
"data_meta":{
},
"x":-2.7,
"y":0.0,
"angle":90.0,
"fontface":"bold",
"label":"Wähler 1",
"data":{
}
},{
"geom":"segment",
"mapping":{
},
"data_meta":{
},
"x":-2.0,
"xend":2.0,
"y":0.0,
"yend":0.0,
"data":{
}
},{
"geom":"segment",
"mapping":{
},
"data_meta":{
},
"x":-2.0,
"xend":2.0,
"y":3.0,
"yend":3.0,
"data":{
}
},{
"geom":"segment",
"mapping":{
},
"data_meta":{
},
"x":-2.0,
"xend":2.0,
"y":-3.0,
"yend":-3.0,
"data":{
}
},{
"geom":"segment",
"mapping":{
},
"data_meta":{
},
"x":-2.0,
"xend":-2.0,
"y":-3.0,
"yend":3.0,
"data":{
}
},{
"geom":"segment",
"mapping":{
},
"data_meta":{
},
"x":0.0,
"xend":0.0,
"y":-3.0,
"yend":3.0,
"data":{
}
},{
"geom":"segment",
"mapping":{
},
"data_meta":{
},
"x":2.0,
"xend":2.0,
"y":-3.0,
"yend":3.0,
"data":{
}
}],
"metainfo_list":[],
"spec_id":"1"
};
           fig = LetsPlot.buildPlotFromProcessedSpecs(plotSpec, containerDiv, sizing);
       } else {
           fig.updateView({});
       }
   }
   
   const renderImmediately = 
       forceImmediateRender || (
           sizing.width_mode === 'FIXED' && 
           (sizing.height_mode === 'FIXED' || sizing.height_mode === 'SCALED')
       );
   
   if (renderImmediately) {
       renderPlot();
   }
   
   if (!renderImmediately || responsive) {
       // Set up observer for initial sizing or continuous monitoring
       var observer = new ResizeObserver(function(entries) {
           for (let entry of entries) {
               if (entry.contentBoxSize && 
                   entry.contentBoxSize[0].inlineSize > 0) {
                   if (!responsive && observer) {
                       observer.disconnect();
                       observer = null;
                   }
                   renderPlot();
                   if (!responsive) {
                       break;
                   }
               }
           }
       });
       
       observer.observe(containerDiv);
   }
   
   // ----------
   })();
   
   </script>
   </body>
</html>
```

:::
:::



## Hyperbolisches Diskontieren

<iframe 
  src="https://janvoss.shinyapps.io/Hyperbolisches-Diskontieren/" 
  width="100%" 
  height="900" 
  title="Spielegenerator">
</iframe>
