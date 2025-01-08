library(plotly)
library(ggplot2)

shot_types <- c("Off-Ball Screen","On-Ball Screen", "Off-Ball Cut",
                "Closeout Drive", "Isolation Drive", "Transition",
                "PnR Drive", "PnR Roll", "PnR Pop",
                "Drive + Dump", "OREB Putback", "Post-Up",
                "Stepback", "Face-Up", "Pull-Up", "Spot-Up","DHO",
                "Other")

turnover_types <- c("Perimeter/Strip", "Bad Pass", "Drive", 
                    "Drive + Pass", "Post Entry", "Other")

regions3 <- c("Left Corner", "Left Wing", "Above the Arc", 
              "Right Wing", "Right Corner")

half_court <-
  ggplot(data=data.frame(x=1,y=1), aes(x,y)) +
  geom_path(data=data.frame(x=25 + c(-25, -25, 25, 25,-25),
                            y=c(0, 47, 47, 0, 0))) + # out of bounds
  geom_path(data=data.frame(x=25 + c(-8000:(-1)/1000, 1:8000/1000),
                            y=c(19 + sqrt(8^2 - c(-8000:(-1)/1000, 1:8000/1000)^2))),
            aes(x=x, y=y)) + # top of key (top semicircle)
  geom_path(data=data.frame(x=25 + c(-8, -8, 8, 8, -8),
                            y=c(0, 19, 19, 0, 0))) + # paint
  geom_path(data=data.frame(x=25 + c(-4000:(-1)/1000, 1:4000/1000),
                            y=c(5.25 + sqrt(4^2 - c(-4000:(-1)/1000, 1:4000/1000)^2))),
            aes(x=x, y=y)) + # restricted area
  geom_path(data=data.frame(x=25 + c(-6000:(-1)/1000,1:6000/1000),
                            y=c(47 - sqrt(6^2 - c(-6000:(-1)/1000, 1:6000/1000)^2))),
            aes(x=x, y=y)) + # half court semicircle
  geom_path(data=data.frame(x=25 + c(-750:(-1)/1000, 1:750/1000, 750:1/1000, -1:-750/1000),
                            y=c(c(5.25 + sqrt(0.75^2 - c(-750:(-1)/1000, 1:750/1000)^2)),
                                c(5.25 - sqrt(0.75^2 - c(750:1/1000, -1:-750/1000)^2)))),
            aes(x=x, y=y)) + # basket
  geom_path(data=data.frame(x=25 + c(-3, 3),
                            y=c(4, 4))) + # backboard
  geom_path(data=data.frame(x=25 + c(-22, -22, -22000:(-1)/1000, 1:22000/1000, 22, 22),
                            y=c(0, 169 / 12, 5.25 + sqrt(23.75^2 - c(-22000:(-1)/1000, 1:22000/1000)^2), 169 / 12, 0)),
            aes(x=x, y=y)) + # three-point line
  lims(x=c(0, 50),
       y=c(0, 47)) +
  coord_fixed() +
  theme_classic() +
  theme(line = element_blank(),
        text = element_blank())

get_court <- function(i) {
  if ((i %% 4) == 0) {
    half_court + 
      coord_flip() + 
      scale_y_reverse() +
      theme(aspect.ratio = 1)
  } else if ((i %% 4) == 1) {
    half_court
  } else if ((i %% 4) == 2) {
    half_court + 
      coord_flip() + 
      theme(aspect.ratio = 1)
  } else {
    half_court + 
      scale_y_reverse()
  }
}

get_x <- function(x, y, i) {
  if ((i %% 4) == 0) {
    return(y)
  } else if ((i %% 4) == 1) {
    return(x)
  } else if ((i %% 4) == 2) {
    return(y)
  } else {
    return(x)
  }
}
get_y <- function(x, y, i) {
  if ((i %% 4) == 0) {
    return(x)
  } else if ((i %% 4) == 1) {
    return(y)
  } else if ((i %% 4) == 2) {
    return(x)
  } else {
    return(y)
  }
}

geom_half_court <- function(ggplot) {
  ggplot +
    geom_path(data=data.frame(x=25 + c(-25, -25, 25, 25,-25),
                              y=c(0, 47, 47, 0, 0))) + # out of bounds
    geom_path(data=data.frame(x=25 + c(-8000:(-1)/1000, 1:8000/1000),
                              y=c(19 + sqrt(8^2 - c(-8000:(-1)/1000, 1:8000/1000)^2))),
              aes(x=x, y=y)) + # top of key (top semicircle)
    geom_path(data=data.frame(x=25 + c(-8, -8, 8, 8, -8),
                              y=c(0, 19, 19, 0, 0))) + # paint
    geom_path(data=data.frame(x=25 + c(-4000:(-1)/1000, 1:4000/1000),
                              y=c(5.25 + sqrt(4^2 - c(-4000:(-1)/1000, 1:4000/1000)^2))),
              aes(x=x, y=y)) + # restricted area
    geom_path(data=data.frame(x=25 + c(-6000:(-1)/1000,1:6000/1000),
                              y=c(47 - sqrt(6^2 - c(-6000:(-1)/1000, 1:6000/1000)^2))),
              aes(x=x, y=y)) + # half court semicircle
    geom_path(data=data.frame(x=25 + c(-750:(-1)/1000, 1:750/1000, 750:1/1000, -1:-750/1000),
                              y=c(c(5.25 + sqrt(0.75^2 - c(-750:(-1)/1000, 1:750/1000)^2)),
                                  c(5.25 - sqrt(0.75^2 - c(750:1/1000, -1:-750/1000)^2)))),
              aes(x=x, y=y)) + # basket
    geom_path(data=data.frame(x=25 + c(-3, 3),
                              y=c(4, 4))) + # backboard
    geom_path(data=data.frame(x=25 + c(-22, -22, -22000:(-1)/1000, 1:22000/1000, 22, 22),
                              y=c(0, 169 / 12, 5.25 + sqrt(23.75^2 - c(-22000:(-1)/1000, 1:22000/1000)^2), 169 / 12, 0)),
              aes(x=x, y=y)) + # three-point line
    lims(x=c(0, 50),
         y=c(0, 47)) +
    coord_fixed() +
    theme_classic() +
    theme(line = element_blank(),
          text = element_blank())
}

shot_chartly <- function(df) {
  (half_court +
     geom_point(data=filter(df, Outcome == "Make"),
                aes(x=x, 
                    y=y,
                    text=paste(Player, `Shot Type`, Outcome, sep="\n")),
                size=3,
                stroke=0.5,
                color="darkgreen",
                shape=1,
                show.legend=FALSE) +
     geom_point(data=filter(df, Outcome == "Miss"),
                aes(x=x, 
                    y=y,
                    text=paste(Player, `Shot Type`, Outcome, sep="\n")),
                size=3,
                stroke=0.5,
                color="red",
                shape=4,
                show.legend=FALSE) +
     geom_point(data=filter(df, str_detect(Outcome, "Foul")),
                aes(x=x, 
                    y=y,
                    text=paste(Player, `Shot Type`, Outcome, sep="\n")),
                size=3,
                stroke=0.5,
                color="darkorange",
                shape=2,
                show.legend=FALSE)) %>%
    ggplotly(tooltip=c("text"))
}

distance <- function(x1, y1, x2, y2) {
  sqrt((y2 - y1)^2 + (x2 - x1)^2)
}

get_region <- function(x, y) {
  if ((x >= 17) & (x <= 33) & (y <= 19)) {
    return ("Paint")
  } else if ((x < 3) & (y <= 14)) {
    return ("Right Corner")
  } else if ((y > 14) & (x < 17) & (distance(25, 5.25, x, y) > 23.75)) {
    return ("Right Wing")
  } else if ((x >= 17) & (x <= 33) & (distance(25, 5.25, x, y) > 23.75)) {
    return ("Above the Arc")
  } else if ((y > 14) & (x > 33) & (distance(25, 5.25, x, y) > 23.75)) {
    return ("Left Wing")
  } else if ((x > 47) & (y <= 14)) {
    return ("Left Corner")
  } else if ((x >= 3) & (x < 17) & (y <= 14)) {
    return ("Right Baseline")
  } else if ((y > 14) & (x < 17) & (distance(25, 5.25, x, y) <= 23.75)) {
    return ("Right Elbow")
  } else if ((x >= 17) & (x <= 33) & (y > 19) & (distance(25, 5.25, x, y) <= 23.75)) {
    return ("Top of Key")
  } else if ((y > 14) & (x > 33) & (distance(25, 5.25, x, y) <= 23.75)) {
    return ("Left Elbow")
  } else {
    return ("Left Baseline")
  }
}

