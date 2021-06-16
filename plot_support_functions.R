library(ellipse)
library(ggforce)
library(gridExtra)

source("support_functions.R")

# radii and center(s) of circles of dart board
circle_df = tibble(x = 0, y = 0, r = rings)
circle_df

# all end points of border lines within dart board
border_phis <- 2 * pi / 40 + 0:19 / 20 * 2*pi
inner_border_points <- cart_coords(tibble(r = rings[2], phi = border_phis))
outer_border_points <- cart_coords(tibble(r = 1, phi = border_phis)) %>%
  rename(xend = x, yend = y)
border_df <- bind_cols(inner_border_points, outer_border_points) 

# board numbers df
# dataframe that contains numbers of dart board in correct order
# in order to be displayed on a plot showing a dart board
number_phis <- 0:19 / 20 * 2 * pi
board_numbers_df <- cart_coords(tibble(r = 1.05, phi = number_phis)) %>%
  mutate(numbers = numbers)

# dart board creation
dart_board_plt <- ggplot()+ geom_circle(data = circle_df, mapping = aes(x0 = x, y0 = y, r = r)) +
  geom_segment(data = border_df, mapping = aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_void() +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1,1)) +
  theme(aspect.ratio=1) + 
  geom_text(data = board_numbers_df, mapping = aes(x = x, y = y, label = numbers))

# function for producing a plot of throws, all shifted as if the target had been Bulls Eye
# Options for coloring points by target and adding prediction ellipses
centered_throws_plot <- function(centered_analysis_data, color_by_target = FALSE, sds_for_ellipses = NULL) {
  if (!color_by_target) {
    throw_points <- geom_point(
      data = centered_analysis_data,
      mapping = aes(x = x_centered,
                    y = y_centered),
      color = "red", 
      shape = 4,
      size = 3,
      stroke = 2
    )
  } else {
    throw_points <- geom_point(
      data = centered_analysis_data,
      mapping = aes(x = x_centered,
                    y = y_centered,
                    color = cat_target),
      shape = 4,
      size = 3,
      stroke = 2
    )
  }
  mean_point <- geom_point(
    mapping = aes(x = mean(centered_analysis_data$x_centered), y = mean(centered_analysis_data$y_centered)),
    color = "red",
    size = 3,
    stroke = 2
  )
  
  if(!is.null(sds_for_ellipses)) {
    # compute prediction ellipses 
    corr_M <- matrix(c(sds_for_ellipses["horiz"]^2, 0, 0, sds_for_ellipses["vert"]^2), ncol = 2)
    inner_ellipse_M <- ellipse(corr_M, level = 2/3)
    inner_ellipse_df <- tibble(x = inner_ellipse_M[,1], y = inner_ellipse_M[,2])
    outer_ellipse_M <- ellipse(corr_M, level = 0.9)
    outer_ellipse_df <- tibble(x = outer_ellipse_M[,1], y = outer_ellipse_M[,2])
    
    return(dart_board_plt +
             throw_points +
             mean_point +
             geom_path(data = inner_ellipse_df, 
                       mapping = aes(x,y), 
                       color = 'darkred', 
                       linetype = 'dashed',
                       size = 1) +
             geom_path(data = outer_ellipse_df, 
                       mapping = aes(x,y), 
                       color = 'darkred',
                       size = 1)
           )
  } else {
    return(dart_board_plt +
             throw_points +
             mean_point)
  }
}

raw_throws_plot <- function(analysis_data) {
  analysis_data <- analysis_data %>%
    mutate(
      x_target = round(x_target, 1),
      y_target = round(y_target, 1)
    )
  
  # compute avg hitting point per target
  mean_point_data <- analysis_data %>%
    group_by(cat_target) %>%
    summarize(x = mean(x_hit),
              y = mean(y_hit))

  # get unique target points
  target_point_data <- analysis_data %>%
    group_by(cat_target) %>%
    summarize(x = x_target[1],
              y = y_target[1])
  
  
  # bind all points to be plotted together in one dataframe
  plot_data <- analysis_data %>%
    mutate(x = x_hit, y = y_hit) %>%
    select(cat_target, x, y) %>%
    bind_rows("hit" = ., "mean" = mean_point_data, "target" = target_point_data, .id = "type")
  
  # generate geom object of all points to be plotted
  plot_points <- geom_point(
    data = plot_data,
    mapping = aes(x = x,
                  y = y,
                  color = cat_target,
                  shape = type),
    size = 3,
    stroke = 2
  ) 
  
  return(
    dart_board_plt +
      plot_points +
      scale_shape_manual(values = c(4, 1, 3))
  )
}