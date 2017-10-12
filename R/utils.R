calcHeading <- function(df, id_var = NULL, unit = c("rad", "deg")) {
  if(!is.null(id_var)) {
    df <- df %>%
      group_by_(id_var) %>%
      mutate(v = c(NA, diff(complex(real = x, imaginary = y)))) %>%
      mutate(heading = c(diff(Arg(v)) %% (2*pi), NA) - pi)
  } else {
    df <- df %>%
      mutate(v = c(NA, diff(complex(real = x, imaginary = y)))) %>%
      mutate(heading = c(diff(Arg(v)) %% (2*pi), NA) - pi)
  }
  
  if(match.arg(unit) == "deg") {
    df$heading <- df$heading * 180 / pi
  }
  
  return(df)
}

calcSpeed <- function(df, id_var = NULL, fps, smooth = 1, align = c("center", "right", "left")) {
  align <-  match.arg(align)
  if(!is.null(id_var)) {
    df %>%
      group_by_(id_var) %>%
      mutate(v = c(NA, diff(complex(real = x, imaginary = y)))) %>%
      mutate(speed = Mod(v) * fps) %>%
      mutate(speed = rollapply(speed, smooth, mean, align = align, fill = 'NA')) %>%
      select(-v)
  } else {
    df %>%
      mutate(v = c(NA, diff(complex(real = x, imaginary = y)))) %>%
      mutate(speed = Mod(v) * fps) %>%
      mutate(speed = rollapply(speed, smooth, mean, align = align, fill = 'NA')) %>%
      select(-v)
  }
}

calcAccel <- function(df, id_var = NULL, smooth = 1, align = c("center", "right", "left")) {
  align <-  match.arg(align)
  if(!is.null(id_var)) {
    df %>%
      group_by_(id_var) %>%
      mutate(accel = c(NA, diff(speed))) %>%
      mutate(accel = rollapply(speed, smooth, mean, align = align, fill = 'NA'))
  } else {
    df %>%
      mutate(accel = c(NA, diff(speed))) %>%
      mutate(accel = rollapply(speed, smooth, mean, align = align, fill = 'NA'))
  }
}
