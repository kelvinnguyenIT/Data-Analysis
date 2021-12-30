install.packages("ggplot2")
library(ggplot2)

dat <- ggplot2::mpg

dat <- transform(dat,
                 cyl = factor(cyl),
                 drv = factor(drv),
                 fl = factor(fl),
                 year = factor(year),
                 class = factor(class)
)
str(dat)

ggplot(dat) +
  aes(x = cyl, y = fl ) +
  geom_point()

ggplot(dat) +
  aes(x = displ, y = hwy) +
  geom_point()+
  geom_line()

ggplot(dat) +
  aes(x = hwy) +
  geom_histogram(bins = sqrt(nrow(dat)))


