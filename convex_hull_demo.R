# demonstration for adding convex hull to ggplot2 scatter
# based on https://stackoverflow.com/questions/48690755/adding-convex-hull-to-ggplot-map

# aim is to add to PCA plots

library(tidyverse)

# convex hull of points on plot (mtcars mpg vs wt)
# these are the slice of data points that define the outer boundary of point area on mpg vs wt plot
hull <- mtcars %>% 
  slice(chull(mpg, wt))

# original scatterplot
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point(shape = 16)

# overlay convex hull as polygon
p + geom_polygon(data = hull, alpha = 0.5)

# separate groupings split by cyl
hull_cyl <- mtcars %>% 
  group_by(cyl) %>% 
  slice(chull(mpg, wt))

# group by both am and cyl, separate hull for each
hull_cyl_a <- mtcars %>%
  group_by(am, cyl) %>% filter(am == 0) %>%
  slice(chull(mpg, wt))

hull_cyl_m <- mtcars %>%
  group_by(am, cyl) %>% filter(am == 1) %>%
  slice(chull(mpg, wt))


# plot with cyl group hulls
p + 
  aes(fill = factor(cyl), color = factor(cyl)) + 
  geom_point(color = 'black', size = 1) +
  geom_polygon(data = hull_cyl, alpha = 0.5, size = .5, linetype = 2)

# plot with am x cyl group hulls
p + 
  aes(fill = factor(cyl), color = factor(cyl), linetype = factor(am)) + 
  geom_point(color = 'black', size = 1) +
  geom_polygon(data = hull_cyl_a, alpha = 0.5) +
  geom_polygon(data = hull_cyl_m, alpha = 0.3)


