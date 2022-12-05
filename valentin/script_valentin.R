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
         year = ymd(year, truncated = 2L))

map_plot <-
df %>%
  filter(continent == "Africa") %>%
  st_as_sf() %>%
  ggplot() + 
  geom_sf(aes(fill = gdpPercap, text = country, frame = year(year))) +
  scale_fill_viridis_c(option = "viridis", 
                       trans = "log", 
                       breaks = c(min(df$gdpPercap, na.rm = T), 
                                median(df$gdpPercap, na.rm = T),
                                mean(df$gdpPercap, na.rm = T),
                                max(df$gdpPercap, na.rm = T)),
                       labels = c(sprintf("Min: %s",min(df$gdpPercap, na.rm = T) %>% 
                                            round(digits = 2)),
                                sprintf("Median: %s", median(df$gdpPercap, na.rm = T) %>% 
                                          round(digits = 2)),
                                sprintf("Mean: %s", mean(df$gdpPercap, na.rm = T) %>% 
                                          round(digits = 2)),
                                sprintf("Max: %s", max(df$gdpPercap, na.rm = T) %>% 
                                          round(digits = 2))),
                       name = "GDP per capita"
  ) +
  theme_map()

ggplotly(map_plot,
         tooltip = c("text", "fill")) %>%
  layout(hoverlabel = list(
    bgcolor = "white",
    bordercolor = "transparent",
    font = list(
      color = "black")
    )) %>%
  animation_opts(transition = 0, redraw = F) %>%
  animation_slider(
    currentvalue = list(prefix = "Year: ", font = list(color = "black"))
  )

plot_geo(df, locationmode = "world") %>% 
  add_trace(locations = ~iso3c, z = ~log(gdpPercap), color = ~log(gdpPercap), frame = ~year(year))

plot_geo(df %>%
           filter(year(year) == 2007)
         ,locationmode = "world") %>% 
  add_trace(locations = ~iso3c, z = ~log(gdpPercap), color = ~log(gdpPercap)) %>%
  add_trace(locations = ~iso3c, z = ~log(pop), color = ~log(pop)) %>%
  layout(
    updatemenus = list(
      list(
        type = "list",
        label = 'Category',
        buttons = list(
          list(method = "restyle",
               args = list('visible', c(TRUE, FALSE)),
               label = "gdpPercap"),
          list(method = "restyle",
               args = list('visible', c(FALSE, TRUE)),
               label = "pop")
        ))))
