DAILY_SALARY <- 500 # nolint
MAXIMUM_REGULAR_HOURS <- 8 # nolint



# 0 - Normal
# 1 - Rest

RESTDAYS <- c(1, 0, 0, 0, 0, 0, 1) # nolint

# 0 - Normal Days
# 1 - Special
# 2 - Holiday
SPECIAL_DAYS <- c(0, 0, 0, 0, 0, 0, 0) # nolint

INTIME <- 0900 # nolint
OUTTIME <- 1800 # nolint

RATES <- c(1, 1.30, 1.50, 2, 2.60) # nolint
OT_RATES <- c(1.25, 1.69, 1.95, 2.60, 3.38) # nolint
OT_NS_RATES <- c(1.375, 1.859, 2.145, 2.86, 3.718) # nolint
weekly_Pay <- c(0.0,0.0,0.0,0.0,0.0,0.0,0.0)
DAYS <- c( # nolint
  "Sunday", "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday"
)

is_valid_military_time <- function(value) {
  if (!is.integer(value)) {
    return(FALSE)
  }

  if (value >= 0 && value <= 2359) { # Adjusted the upper limit to 2359
    hours <- value %/% 100
    minutes <- value %% 100

    if (hours >= 0 && hours <= 23 && minutes >= 0 && minutes <= 59) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

print_menu <- function(day) {
  cat(paste(
    "Hello User ! Today is", DAYS[day],
    "! Please choose the following:\n"
  ))
  cat("[1] Worked\n")
  cat("[2] Edit Configuration\n")
  cat("[3] Exit\n")
  flush.console()
}

print_arr <- function(arr) {
  i <- 1
  for (element in arr) {
    if (element != 0) {
      cat(DAYS[i])
      cat(" ")
    }
    i <- i + 1
  }
}

invalid_prompt <- function() {
  cat("INVALID INPUT !\n")
  flush.console()
}

edit_daily_salary <- function() {
  temp <- readline(prompt = "Set new Salary: ")
  temp <- as.integer(temp)
  DAILY_SALARY <<- temp # nolint
}

edit_rest_days <- function() {
  flush.console()
  temp <- ""
  RESTDAYS <<- c(0, 0, 0, 0, 0, 0, 0) # nolint
  while (temp != "done") {
    cat("Rest Days: ")
    print_arr(RESTDAYS)
    cat("\n")
    temp <- readline(prompt = "List down which days should be rest days, otherwise type 'done': ") # nolint
    if ((temp %in% DAYS)) {
      index <- which(DAYS == temp)
      cat(paste(temp, " is now a restday\n"))
      RESTDAYS[index] <<- 1 # nolint
    } else if (temp != "done") {
      invalid_prompt()
    }
  }
}

edit_special_days <- function() {
  flush.console()
  temp <- ""
  SPECIAL_DAYS <<- c(0, 0, 0, 0, 0, 0, 0) # nolint
  while (temp != "done") {
    cat("Special Days: ")
    print_arr(SPECIAL_DAYS)
    cat("\n")
    temp <- readline(prompt = "List down which days should be special days, otherwise type 'done': ") # nolint
    if ((temp %in% DAYS)) {
      index <- which(DAYS == temp)

      valid_day <- FALSE

      while (valid_day == FALSE) {
        tryCatch(
          expr = {
            type <- readline("[0] Normal Day\n[1] Special Non-Working Day\n[2] Regular Holiday\n")
            type <- as.integer(type)
            if (is_in_range(type, 0, 2)) {
              valid_day <- TRUE
            } else {
              cat("Invalid Value !")
            }
          },
          error = function(e) {
            invalid_prompt()
          },
          warning = function(w) {
            invalid_prompt()
          }
        )
      }
      SPECIAL_DAYS[index] <<- type # nolint
    } else if (temp != "done") {
      invalid_prompt()
    }
  }
}

is_in_range <- function(val, min, max) {
  return(val >= min && val <= max)
}

edit_max_reg_hours <- function() {
  temp <- readline(prompt = "Set new maximum regular hours: ")
  temp <- as.integer(temp)
  if (is_in_range(temp, 0, 24) == TRUE) {
    MAXIMUM_REGULAR_HOURS <<- temp # nolint\
  } else {
    cat("Invalid Value!\n")
  }
}

edit_in_time <- function() {
  temp <- readline(prompt = "Set new in time: ")
  temp <- as.integer(temp)
  if (is_in_range(temp, 0, 2400) && is_valid_military_time(temp)) {
    INTIME <<- temp
  } else {
    invalid_prompt()
  }
}

edit_config <- function(edit_choice) {
  flush.console()
  tryCatch(
    expr = {
      if (edit_choice == 1) {
        edit_daily_salary()
      } else if (edit_choice == 2) {
        edit_max_reg_hours()
      } else if (edit_choice == 3) {
        edit_rest_days()
      } else if (edit_choice == 4) {
        edit_special_days()
      } else if (edit_choice == 5) {
        edit_in_time()
      } else {
        cat("BACK!\n")
      }
    },
    error = function(e) {
      cat("BACK!\n")
    },
    warning = function(w) {
      cat("BACK!\n")
    }
  )
}

print_configuration <- function() {
  cat("What would you like to Edit?\n")
  cat(paste("[1] Daily Salary: ", DAILY_SALARY, "\n"))
  cat(paste("[2] Maximum Regular Hours: ", MAXIMUM_REGULAR_HOURS, "\n"))
  cat("[3] Rest Days: ")
  print_arr(RESTDAYS)
  cat("\n")
  cat("[4] Special Days: ")
  print_arr(SPECIAL_DAYS)
  cat("\n")
  cat(paste("[5] In time: ", INTIME, "\n"))
  cat("Enter Anything Else To Go Back!\n")
  flush.console()
}


get_hours_worked <- function() {
  tryCatch(
    expr = {
      cat(paste("In time: ", INTIME, "\n"))
      out_time <- readline(prompt = "Enter Time of Exit: ")
      out_time <- as.integer(out_time)
      hours <- compute_hours(out_time, INTIME)
      if (hours == 0) {
        return(c(TRUE, 0, out_time))
      }
      if (hours >= 8 && is_valid_military_time(out_time)) {
        return(c(TRUE, hours, out_time))
      } else {
        cat("Users must work between 8 - 24 hours\n")
        return(c(FALSE, 0, out_time))
      }
    },
    error = function(e) {
      invalid_prompt()
    },
    warning = function(w) {
      invalid_prompt()
    }
  )
}

# compute how many hours between 2 periods, time 1 should be the later period
compute_hours <- function(time1, time2) {
  time1 <- time1 / 100
  time2 <- time2 / 100
  time1_d <- (time1 - floor(time1)) / .60
  time2_d <- (time2 - floor(time2)) / .60
  time1 <- floor(time1) + time1_d
  time2 <- floor(time2) + time2_d
  if (time1 - time2 < 0) {
    return(24 + (time1 - time2))
  } else {
    return(time1 - time2)
  }
}

# checks if a time is within some time frames
is_within <- function(val, start, end) {
  val <- as.integer(val)
  start <- as.integer(start)
  end <- as.integer(end)

  if (start <= end) {
    return(val >= start && val <= end)
  } else {
    return(val >= start || val <= end || (val == start && val == end))
  }
}

# time2 is the out time, time1 is the in time
compute_night_int <- function(start, end) {
  day_mins <- 0
  night_mins <- 0
  ot_night_mins <- 0
  ot_day_mins <- 0
  normal_rate_mins <- MAXIMUM_REGULAR_HOURS * 60
  mins_worked <- 0
  x <- 0
  for (i in 0:1439) {
    x <- x %% 2400
    if (is_within(x, start, end) && x != end) {
      if (is_within(x, 600, 2159)) {
        if (mins_worked - 60 < normal_rate_mins) {
          day_mins <- day_mins + 1
        } else {
          ot_day_mins <- ot_day_mins + 1
        }
      } else {
        if (mins_worked - 60 < normal_rate_mins) {
          night_mins <- night_mins + 1
        } else {
          ot_night_mins <- ot_night_mins + 1
        }
      }

      mins_worked <- mins_worked + 1
    }
    x <- x + 1
    x_minutes <- x %% 100
    if (x_minutes == 60) {
      x <- x + 40
    }
  }
  return(c(day_mins, ot_day_mins, night_mins, ot_night_mins))
}

take_day_type <- function(day) {
  if (RESTDAYS[day] == 1) { # if its a rest day
    if (SPECIAL_DAYS[day] == 0) { # if its a normal day
      return(2)
    } else if (SPECIAL_DAYS[day] == 1) { # if itsd a snwd
      return(3)
    } else if (SPECIAL_DAYS[day] == 2) { # if its a regular holiday
      return(5)
    }
  } else { # if not a rest day
    if (SPECIAL_DAYS[day] == 0) { # if its a normal day
      return(1)
    } else if (SPECIAL_DAYS[day] == 1) { # if itsd a snwd
      return(2)
    } else if (SPECIAL_DAYS[day] == 2) { # if its a regular holiday
      return(4)
    }
  }
}



# p1 is day hours
# p2 is day_hours overtime
# p3 is Night hours
# p4 is Night hours overtime
payments_computations <- function(p1, p2, p3, p4, days) {
  hourly_rate <- DAILY_SALARY / MAXIMUM_REGULAR_HOURS
  day_type <- take_day_type(days)

  base <- 0

  # compute daily salary based on day
  hour_pay <- DAILY_SALARY * RATES[day_type]

  # add by day overtime
  ot_hour_pay <- base + (hourly_rate * p2 * OT_RATES[day_type])

  # add by night shift
  night_pay <- base + (hourly_rate * p3 * 1.1 * RATES[day_type])

  # add by night overtime
  ot_night_pay <- base + (hourly_rate * p4 * OT_NS_RATES[day_type])

  full_pay <- round(hour_pay + ot_hour_pay + night_pay + ot_night_pay, 2)
  day <- sprintf("Day Computation: %d * %.2f = %.2f\n", p1, RATES[day_type], hour_pay) # nolint
  ot_day <- sprintf("OT Day Computation: %d * %.2f= %.2f\n", p2, OT_RATES[day_type], ot_hour_pay) # nolint
  night <- sprintf("Night Computation: %d * %.2f * 1.1 = %.2f\n", p3, RATES[day_type], night_pay) # nolint
  ot_night <- sprintf("OT Night Computation: %d * %.2f = %.2f\n", p4, OT_NS_RATES[day_type], ot_night_pay) # nolint
  pay <- sprintf("%.2f + %.2f + %.2f + %.2f = %.2f\n", hour_pay, ot_hour_pay, night_pay, ot_night_pay, full_pay) # nolint
  cat(day)
  cat(ot_day)
  cat(night)
  cat(ot_night)
  cat(pay)
  
  weekly_Pay[days] <<- full_pay
  
  return(full_pay)
}



compute_pay <- function(day) {
  is_valid <- c(FALSE, 0)
  while (is_valid[1] == FALSE) {
    is_valid <- get_hours_worked()
  }

  hours <- is_valid[2]
  out_time <- is_valid[3]

  if (hours == 0) {
    if (RESTDAYS[day] == 1) {
      absent_prompt <- sprintf("You are paid %.2f\n", DAILY_SALARY)
      weekly_Pay[day] <<- DAILY_SALARY
    } else {
      absent_prompt <- sprintf("ABSENT! NO PAY!\n")
    }
    cat(absent_prompt)
    return(TRUE)
  }

  mins_worked <- compute_night_int(as.integer(INTIME), as.integer(out_time))

  day_hours <- mins_worked[1] / 60
  day_hours_ot <- mins_worked[2] / 60
  night_hours <- mins_worked[3] / 60
  night_hours_ot <- round(mins_worked[4] / 60)

  cat(paste("Day Hours", day_hours, "\n"))
  cat(paste("Day Hours Overtime", day_hours_ot, "\n"))
  cat(paste("Night Hours", night_hours, "\n"))
  cat(paste("Night Hours Overtime", night_hours_ot, "\n"))
  payments_computations(day_hours,day_hours_ot,night_hours,night_hours_ot,day)
  cat("\n")
  cat("\n")
  return(TRUE)
}


main <- function() {
  flush.console()
  exit <- 0
  day <- 1
  while (exit == 0 && day != 8) {
    tryCatch(
      expr = {
        choice <- readline(prompt = print_menu(day))
        choice <- as.integer(choice)

        if (choice == 1) {
          if (compute_pay(day) == TRUE) {
            day <- day + 1
          }
        } else if (choice == 2) {
          cat("Edit Configuration\n")
          edit_choice <- readline(print_configuration())
          edit_choice <- as.integer(edit_choice)
          edit_config(edit_choice)
        } else if (choice == 3) {
          cat("EXITING PROGRAM!\n")
          exit <- 1
        } else {
          invalid_prompt()
        }
      },
      error = function(e) {
        invalid_prompt()
      },
      warning = function(w) {
        invalid_prompt()
      }
    )

  }
    day_pay <- 0
      for (i in 1:7) {
        printer <- sprintf("%s: %.2f\n", DAYS[i], weekly_Pay[i])
        cat(printer)
        day_pay <- weekly_Pay[i] + day_pay
      }
      cat(paste("You have earned :", day_pay, " this week!\n"))
  
}

main()
