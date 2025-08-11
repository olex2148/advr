library("tidyverse")

#3.3

#1

# Needs to be outside mapping, since mapping specify relation between variable and value.
# This is what is likely meant

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")

#2

?mpg
glimpse(mpg)

# chr ones are categorical
# int or dbl are continuous

#3

# Map cty (city miles per gallon) which is continuous to colour. 
# It becomes a scale of colour and not descrete colours. 

ggplot(mpg, aes(x = displ, y = hwy, colour = cty)) +
  geom_point()

# Scale of sizes

ggplot(mpg, aes(x = displ, y = hwy, size = cty)) +
  geom_point()

# Shape is not possible

ggplot(mpg, aes(x = displ, y = hwy, shape = cty)) +
  geom_point()

#4

ggplot(mpg, aes(x = displ, y = hwy, colour = hwy, size = displ)) +
  geom_point()

# We get a plot but redundant information since size and x-axis position give the same information. Same for colour

#5

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 2, stroke = 5)

# Creates an "aura" or border with thickness of the specified value

#6

ggplot(mpg, aes(x = displ, y = hwy, colour = displ < 5)) +
  geom_point()

# It creates a colour boolean, so that above and below have a colour each

#3.6

#1

# line chart: geom_line()
# boxplot: geom_boxplot()
# histogram: geom_histogram()
# area chart: geom_area()

#2

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)


# both scatterplot and smooth (without se). Gas consumption vs miles per gallon,
# coloured according to drive, i.e. front, back, 4 wheel


#3


ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, colour = drv),
    show.legend = FALSE
  )

# in the name, legend will not be shown. If we delete, it is there, so it is on by default

#4

#adds standard error to the geom_smooth:

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = TRUE)

#on by default:

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth()


#5


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

# No, the 2 geoms will inherit the data specified in the mapping. This is easier to look at, code more clear. 

#6

#.1

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)

#.2

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(group = drv), se = FALSE) +
  geom_point()


#.3

ggplot(mpg, aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

#.4

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = drv)) +
  geom_smooth(se = FALSE)

#.5

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = drv)) +
  geom_smooth(aes(linetype = drv), se = FALSE)


#.6

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 4, color = "white") +
  geom_point(aes(colour = drv))
