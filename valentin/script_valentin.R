library(tidyverse)
library(gapminder)
library(rnaturalearth)
library(sf)
library(countrycode)
library(plotly)
library(ggthemes)
library(lubridate)

world <- ne_countries(scale = "small", 
                      returnclass = "sf") %>%
  select(iso_a3, economy, geometry) %>%
  rename(iso3c = iso_a3)

gapminder <- gapminder %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) 

df <-
left_join(
  x = world,
  y = gapminder,
  by = "iso3c"
) %>%
  mutate(country = countrycode(iso3c, "iso3c", "country.name"),
         year = ymd(year, truncated = 2L),
         point = st_point_on_surface(geometry),
         x_coord = st_coordinates(point)[1],
         y_coord = st_coordinates(point)[2]) %>%
  filter(continent == "Africa") %>%
  st_as_sf() %>%
  highlight_key(~iso3c)
  
map_plot <-  
  df %>%
  filter(year == 2007) %>%
  ggplot() + 
  geom_sf(aes(fill = gdpPercap, 
              text = paste("<b>", country, "</b>", "\n", "Year:", year(year), "\n", "GDP per capita:", round(gdpPercap, 2)))) +
  scale_fill_viridis_c(option = "viridis", 
                       trans = "log", 
                       name = "GDP per capita"
  ) +
  theme_map()

ggplotly(map_plot) %>%
  animation_opts(transition = 0, redraw = T) %>%
  animation_slider(
    currentvalue = list(prefix = "Year: ", font = list(color = "black"))
  ) 

plt1 <-
plot_ly(
  stroke = I("black"),
  span = I(1)
  ) %>%
  add_sf(data = df,
         split = ~iso3c,
         frame = ~year(year),
         color = ~log(gdpPercap),
         text = ~paste("<b>", country, "</b>", "\n", "Year:", year(year), "\n", "GDP per capita:", round(gdpPercap, 2)),
         hoveron = "fills",
         hoverinfo = "text",
         visible = T
  ) %>%
  add_sf(data = df,
         split = ~iso3c,
         frame = ~year(year),
         color = ~log(pop),
         text = ~paste("<b>", country, "</b>", "\n", "Year:", year(year), "\n", "Population:", pop),
         hoveron = "fills",
         hoverinfo = "text",
         visible = F
  ) %>%
  layout(
    showlegend = F,
    updatemenus = list(
      list(
        buttons = list(
          list(method = "restyle",
               args = list('visible', c(rep(T, 48), rep(F, 49))),
               label = "gdpPercap"),
          list(method = "restyle",
               args = list('visible', c(rep(F, 48), rep(T, 49))),
               label = "pop")
        )))) %>%
  animation_opts(
    transition = 0, redraw = T) %>%
  animation_slider(
    currentvalue = list(prefix = paste("<b>", "Year: "), font = list(color = "black"))
  ) %>%
  style(
    hoveron = "fills",
    hoverlabel = list(
      bgcolor = "white",
      bordercolor = "transparent",
      font = list(
        color = "black")
    )) %>%
  highlight(on = "plotly_hover",
            opacityDim = .75)
plt1

plt2 <-
  plot_ly(
    data = df,
    split = ~iso3c,
    color = I("grey90"),
    showlegend = F
  ) %>%
  add_markers(data = df,
              frame = ~year(year),
              x = ~x_coord,
              y = ~y_coord,
              size = ~log(pop),
              color = ~log(pop),
              text = ~paste("<b>", country, "</b>", "\n", "Year:", year(year), "\n", "Population:", pop),
              hoveron = "fills",
              hoverinfo = "text",
              ) %>%
  animation_opts(
    transition = 0, redraw = T) %>%
  animation_slider(
    currentvalue = list(prefix = paste("<b>", "Year: "), font = list(color = "black"))
  ) %>%
  style(
    hoveron = "fills",
    hoverlabel = list(
      bgcolor = "white",
      bordercolor = "transparent",
      font = list(
        color = "black")
    )) %>%
  highlight(on = "plotly_hover",
            opacityDim = .75)
plt2

plotly_json(plt1)

plt <- plotly_build(plt1)
length(plt$x$data)
x <- invisible(lapply(1:length(plt$x$data),
                      function(i){
                        plt$x$data[[i]]$name
                      })) %>% unlist()
table(x)
  

