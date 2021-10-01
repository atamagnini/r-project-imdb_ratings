library(tidyverse)
library(highcharter)
library(htmlwidgets)

df <- read.csv('~/df_movies.csv')

#clean dataset
df2 <- df[-which(df$genres == ""), ]
df2 <- df2[, -c(1:3,5,7,10:16)]
df2 <- df2[!duplicated(df2), ]
any(is.na(df2))
df2 <- df2[!is.na(df2$genres),]
df2 <- df2[!is.na(df2$averageRating),]
glimpse(df2)
#eliminar segunda palabra en columna (después de la ,)
df2$genres <- gsub(",.*","",df2$genres)
#promedios de una columna
df3 <- df2 %>% group_by(genres) %>% summarize(averageRating = round(mean(averageRating),2))

#plot
p <- hchart(df3, type = 'line',
            hcaes(x = 'genres', y = 'averageRating')) %>%
  hc_plotOptions(line = list(color = "black")) %>%
  hc_chart(polar = TRUE) %>%
  hc_yAxis(max = 7.5, title = FALSE) %>%
  hc_xAxis(title = FALSE) %>%
  hc_title(text = 'Movie rating on IMDB', style = list(fontWeight = 'bold', fontSize = '20px'),
           align = 'left') %>%
  hc_subtitle(text = 'IMDB movies ranked by genre', style = list(fontWeight = 'bold', fontSize = '15px'),
              align = 'left') %>%
  hc_tooltip(useHTML = TRUE, headerFormat = '', 
             pointFormat = '<b> <span style = "color: blue"> {point.genres} </span> <br> <em> Average Rating: {point.y} <em>') %>%
  hc_credits(enabled = TRUE, text = 'By Antonela Tamagnini <br> Source: IMDB Website') %>%
  hc_add_theme(hc_theme_gridlight())
p
saveWidget(widget = p, file = 'plot.html')
