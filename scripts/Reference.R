install.packages("geomtextpath")
library(geomtextpath)
library(tidyverse)

df <- data.frame(x1 = c(seq(0, 10/6 * pi, pi/3),
												 seq(0, 10/6 * pi, 2*pi/3)), 
								 y1 = c(rep(2, 6), rep(-1, 3)), 
								 x2 = c(seq(0, 10/6 * pi, pi/3)  + pi/3, 
				    							seq(0, 10/6 * pi, 2*pi/3) + 2*pi/3),
								 y2 = c(rep(4, 6), rep(2, 3)),
								 group = letters[c(1:6, (1:3) * 2)],
								 alpha = c(rep(1, 6), rep(0.4, 3)))

df %>% ggplot(aes(x1, y1)) +
	geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = group,
								alpha = alpha),
						color = "white", linewidth = 2)

plot <- df %>% ggplot(aes(x1, y1)) +
	geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = group,
								alpha = alpha),
						color = "white", linewidth = 2) +
	geom_textpath(data = data.frame(x1 = seq(0, 2 * pi, length = 300),
																	y1 = rep(0.5, 300),
																	label = rep(c("stats", "effects", "polar"), each = 100)),
								aes(label = label), linetype = 0, size = 8,
								upright = TRUE) +
	geom_textpath(data = data.frame(x1 = seq(0, 2 * pi, length = 300),
																	y1 = rep(3, 300),
																	label = rep(c("density", "smooth", "unique", "organic",
																								"easy to use", "automatic"),
																							each = 50)),
								aes(label = label), linetype = 0, size = 4.6, color = "white",
								upright = TRUE) +
	scale_y_continuous(limits = c(-5, 4)) +
	scale_x_continuous(limits = c(0, 2*pi)) +
	scale_fill_manual(values = c("deepskyblue3", "deepskyblue4",
															 "green3", "green4","tomato", "tomato2")) +
	scale_alpha_identity() +
	theme_void() +
	theme(legend.position = "none") 

plot + coord_polar() # 极坐标化

