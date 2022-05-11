
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lattes

-   After seeing [this viral repo](https://github.com/cj-holmes/toast)
    about the best way ‘to sort a toast’, I thought I’d have a go
    replicate it to sort lattes.

``` r
library(tidyverse)
library(magick)
```

-   Start by reading the raw toast image
-   Then convert it to a dataframe with values of red, green and blue
    for each pixel

``` r
raw <- image_read('lattes.png')
i_raster <- raw %>% image_raster() %>% mutate(col2rgb(col) %>% t() %>% as_tibble()) %>% as_tibble()
```

-   Manually pick x and y values that will get me somewhere near the
    center of each latte
-   I’m using the fact that the lattes are roughly on a uniform grid to
    allow me to choose only 12 x-values and 10 y-values
    -   I’m doing it this way because I’m lazy
    -   It would be better to manually (or otherwise) get the 120
        coordinate pairs that land in the center of each piece of toast
-   Then visualise the x and y values to see if they are sensible

``` r
xs <- c(47, 110, 170, 230, 285, 340)
ys <- c(45, 105, 165, 220, 280, 340)

i_raster %>% 
  ggplot()+
  geom_raster(aes(x,y,fill=I(col)))+
  geom_hline(yintercept = ys, col=4)+
  geom_vline(xintercept = xs, col=4)+
  coord_equal()+
  scale_y_reverse()
```

![](README_lattes_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

-   Generate the labels for each latte and define the width `w` and
    height `h` of the selection box to apply to each latte
    -   Width and height needs to be an odd number here

``` r
labels <- crossing(column = LETTERS[1:6], row = 1:6) %>% mutate(label = paste0(column, row))
w <- 23
h <- 23
```

-   Now generate a dataframe of the 36 center coordinate pairs
-   Bind the center coordinates to their labels
-   Generate the coordinates of all pixels inside a box of width `w` and
    height `h` centered on each of the center coordinates
-   Inner join to the raster image dataframe to get the pixel colours at
    each coordinate contained within each selection box
-   Create a greyscale intensity column from the red, green and blue
    values of each pixel
    -   I did this using the weights sugegsted in [this stack overflow
        post](https://stackoverflow.com/questions/687261/converting-rgb-to-grayscale-intensity)
    -   I’m not sure if this is the best or even correct way of doing
        this
-   Add some normalised coordinates so each box can be plotted on the
    same coordinates
-   **And just wow…at how good the tidyverse is**

``` r
d <-
  crossing(x_c = xs, y_c = ys) %>%
  bind_cols(labels) %>% 
  mutate(tmp = map2(x_c, y_c, ~crossing(x = (.x - (w-1)/2):(.x + (w-1)/2),
                                        y = (.y - (h-1)/2):(.y + (h-1)/2)))) %>% 
  unnest(tmp) %>% 
  inner_join(i_raster, by = c("x", "y")) %>% 
  mutate(g = (0.2989 * red) + (0.5870 * green) + (0.1140 * blue)) %>% 
  group_by(label) %>% 
  mutate(x_norm = scales::rescale(x, c(1, w)), 
         y_norm = scales::rescale(y, c(1, h))) %>% 
  ungroup()
```

-   Visualise the selection boxes to check that they are sensible and
    visualise the selection of pixels within each box

``` r
ggplot()+
  geom_raster(data = i_raster, aes(x, y, fill = I(col)))+
  geom_rect(data = 
              d %>% 
              group_by(label) %>% 
              summarise(xs = min(x), xe = max(x), 
                        ys = min(y), ye = max(y),
                        .groups = "drop"),
            aes(xmin = xs, xmax = xe, ymin = ys, ymax = ye),
            col = 4, fill = NA)+
  coord_equal()+
  scale_y_reverse()+
  theme_void()

d %>% 
  ggplot() + 
  geom_raster(aes(x_norm, y_norm, fill = I(col)))+
  facet_grid(row~column)+
  coord_equal()+
  scale_y_reverse()+
  theme_void()
```

<img src="README_lattes_files/figure-gfm/unnamed-chunk-7-1.png" width="50%" /><img src="README_lattes_files/figure-gfm/unnamed-chunk-7-2.png" width="50%" />

-   Aggregate the pixels in each selection box
-   Compute the mean and standard deviation of the red, green, blue and
    greyscale values

``` r
agg <- 
  d %>% 
  group_by(column, row, x_c, y_c, label) %>% 
  summarise(across(red:g, list(m = mean, sd = sd)), .groups = "drop")
```

-   Visualise the mean and standard deviation as a grid

``` r
agg %>% 
  ggplot()+
  geom_raster(aes(column, row, fill = g_m))+
  scale_fill_viridis_c(option = "plasma")+
  coord_equal()+
  scale_y_reverse(breaks = 1:12)+
  labs(title = "Intensity", x="", y="")+
  theme(legend.position = "bottom")

agg %>% 
  ggplot()+
  geom_raster(aes(column, row, fill = g_sd))+
  scale_fill_viridis_c(option = "plasma")+
  coord_equal()+
  scale_y_reverse(breaks = 1:12)+
  labs(title = "Standard deviation", x="", y="")+
  theme(legend.position = "bottom")
```

<img src="README_lattes_files/figure-gfm/unnamed-chunk-9-1.png" width="50%" /><img src="README_lattes_files/figure-gfm/unnamed-chunk-9-2.png" width="50%" />

-   Visualise the mean and standard deviation as an ordered bar chart
-   Compute the colour of each aggregated selection box from the mean
    red, mean green and mean blue values
    -   Fill the bars with this colour

``` r
agg %>% 
  mutate(col_m = rgb(red_m, green_m, blue_m, maxColorValue = 255)) %>% 
  ggplot() + 
  geom_col(aes(reorder(label, g_m), g_m, fill=I(col_m))) + 
  geom_errorbar(aes(x = label, ymin = g_m - g_sd, ymax = g_m + g_sd))+
  coord_flip()
```

![](README_lattes_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

-   Compute the order of the labels and visualise the selection boxes in
    order

``` r
latte_order <- agg %>% arrange(desc(g_m)) %>% pull(label)

d %>% 
  mutate(label = factor(label, levels = latte_order)) %>% 
  ggplot() + 
  geom_raster(aes(x_norm, y_norm, fill = I(col)))+
  facet_wrap(~label)+
  scale_y_reverse()+
  coord_equal()+
  theme_void()
```

![](README_lattes_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
