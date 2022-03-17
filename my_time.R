library("magrittr")
library("dplyr")
library("lubridate")

CLOCK_PATH <- paste0(getwd(), "/mytime_clock.csv")
cur_year <- year(Sys.Date())
SUMMARY_PATH <- paste0(getwd(), "/Undis_worktime_", cur_year, ".csv")

Start <- function(set.time = NULL) {
  "Start the clock.

  ARGUMENTS
  set.time - str, optional. Start time in format 'HH:MM'.
  If not provided, start clock at the moment function
  is called.

  VALUE
  NULL
  "
  
  # Load clock data
  info <- read.csv(CLOCK_PATH,
                   stringsAsFactors = FALSE)
  
  # Throw error if already running
  if(is.na(data.table::last(info$stop))) {
    stop(paste("Already started at", last(info$start)))
  }
  
  if(is.null(set.time)) { #Start now
    time <- as.character(Sys.time())
  } else { #Start at given time 
    # Parse together current date and given time
    start_time <- Sys.time() %>% as.POSIXct()
    hour(start_time)   <- substr(set.time, 1, 2) %>% as.double
    minute(start_time) <- substr(set.time, 4, 5) %>% as.double
    second(start_time) <- 0
    tz(start_time)     <- Sys.timezone()
    time <- as.character(start_time)
  }
  
  # Add start time to clock and save
  new <- data.frame("start" = time, "stop" = NA, "time" = NA)
  info <- rbind(info, new)
  write.csv(info,
            file = CLOCK_PATH,
            row.names = FALSE)
  
  print(paste("Clock started at", time))

}


Stop  <- function(set.time = NULL) {
  "Stop the clock.

  ARGUMENTS
  set.time - str, optional. Start time in format 'HH:MM'.
  If not provided, stop clock at the moment function
  is called.

  VALUE
  NULL
  "
  
  # Read clock data
  info <- read.csv(CLOCK_PATH,
                   stringsAsFactors = FALSE)
  
  # Thorw error if clock not started
  if(!is.na(last(info$stop))) {
    stop("Work has not been Start():ed.")
  }
  
  n <- nrow(info)
  if(is.null(set.time)) { # Stop now
    info$stop[n] <- as.character(Sys.time())
  } else { # Stop at given time
    # Parse given time together with given time
    stop_time <- last(info$start) %>% as.POSIXct()
    hour(stop_time)   <- substr(set.time, 1, 2) %>% as.double
    minute(stop_time) <- substr(set.time, 4, 5) %>% as.double
    second(stop_time) <- 0
    tz(stop_time)     <- Sys.timezone()
    info$stop[n] <- as.character(stop_time)
  }
  
  # Add stop time and worked hours to data, then save
  info$time[n] <- difftime(info$stop[n], info$start[n], units = "hour")
  write.csv(info,
            file = CLOCK_PATH,
            row.names = FALSE)
  
  print(paste("Stopped at", info$stop[n], "Worked", last(info$time), "hours."))

}


work_summary <- function(year = NULL) {
  "
  Print a summary of worked time:
  Hours worked today, current month average per day,
  hours worked each month, cumulative sum, expected hours according
  to contract, and accumulated difference in hours and holidays.
  "

  cur_year <- year(Sys.Date())
  if(is.null(year)) {
    year <- year(Sys.Date())
  }
  
  if(year != cur_year) {
    # Inspect a past year
    file <- paste0(
      "Undis_worktime_",
      year,
      ".csv")
    df <- read.csv(file, stringsAsFactors = FALSE)
  } else {
    # Inspect current year
    if(file.exists(SUMMARY_PATH)) {
      # Ongoing year, data exists
      df <- read.csv(SUMMARY_PATH, stringsAsFactors = FALSE) #load data
    } else {
      # WHEN STARTING A NEW YEAR:
      date_seq <- seq.Date(as.Date(paste0(cur_year, "-01-01")), 
                           as.Date(paste0(cur_year, "-12-31")),
                           "day")
      new_df <- data.frame("date" = date_seq,
                           "hours_worked" = numeric(length(date_seq)))
      new_df$work_day <- wday(new_df$date) %in% 2:6
      new_df$month <- month(new_df$date)
      workdays.month <- new_df %>% group_by(month) %>%
        summarise(workdays.month = sum(work_day)) %>% as.data.frame()
      new_df <- left_join(new_df, workdays.month, by = "month")

      write.csv(new_df, file = SUMMARY_PATH, row.names = FALSE)
      
      df <- new_df
      
    }
    
    # Update summarizing dataframe:
    info_all <- read.csv(CLOCK_PATH,
                         stringsAsFactors = FALSE)
    # Select dates of current year where work has been done
    info <- info_all %>% subset(year(as.Date(start)) == cur_year & !is.na(time))
    
    if(nrow(info) > 0) {
      sums <- info %>% group_by(as.character(as.Date(start))) %>%
        summarise(hours = sum(time)) %>% as.data.frame() # Count work sums per day
      colnames(sums)[1] <- "date"
      dates <- which(df$date %in% sums$date)
      df$hours_worked[dates] <- sums$hours # input counted sums to data frame
      
      # Save the updated data frame
      write.csv(df,
                file = SUMMARY_PATH,
                row.names = FALSE)
    }
  }
  
  # form and print summary
  my.time <- data.frame("h.done"     = double(12),
                        "h.todo"     = double(12),
                        "h.done.sum" = double(12),
                        "h.todo.sum" = double(12),
                        "diff"       = double(12),
                        "holidays"   = double(12))
  rownames(my.time) <- c(months(seq(as.Date(df$date[1]),
                                    as.Date(last(df$date)),
                                    "month")))
  
  my.time$h.done <- df %>%
    group_by(month) %>%
    summarize(h.done = sum(hours_worked, na.rm = TRUE)) %>%
    pull
  
  days.per.month  <- df %>% group_by(month) %>% summarize(n()) %>% pull
  todo.per.year.40   <- 1624 * 0.4 # 
  todo.per.year.80   <- 1612 * 0.8 # new TES -> lessened number of hours per year
  
  if(year == "2020") {
    my.time$h.todo[1:7]     <- ((days.per.month[1:7] / 366) * todo.per.year.40)
    my.time$h.todo[8]       <- (9/366) * todo.per.year.40 + (22/366) * todo.per.year.80
    my.time$h.todo[9:12]    <- ((days.per.month[9:12] / 366) * todo.per.year.80)
  } else {
    my.time$h.todo <- (days.per.month / nrow(df)) * todo.per.year.80
  }

  my.time$h.todo.sum <- my.time$h.todo %>% cumsum
  
  my.time$h.done.sum <- my.time$h.done %>% cumsum
  if(year == 2021) {
    my.time$h.done.sum <- my.time$h.done.sum + 21.08 # WORKS FOR 2021: add + hours from 2020 
  }
  
  my.time$diff      <- (my.time$h.done.sum - my.time$h.todo.sum)
  my.time$holidays  <- (my.time$diff) / 5.8
  
  if(year == cur_year) {
    # Show months only up to this month
    current.month <- month(Sys.Date())
    
    if(current.month != 12) {
      my.time[(current.month+1):12, ] <- NA
    }
  }

  print(round(my.time,2))
  
  if(year == cur_year) {
    current_month_so_far <- df %>% subset(month == month(Sys.Date()) & as.Date(date) < Sys.Date())
    
    print(data.frame("date" = current_month_so_far$date,
                     "hours_worked" = round(current_month_so_far$hours_worked, 2)))
    
    avg_month <- sum(current_month_so_far$hours_worked) / sum(current_month_so_far$work_day)
    full_hours_avg <- floor(avg_month)
    minutes_avg <- ((avg_month - full_hours_avg) * 60) %>% round()
    print(paste("Average hours per day this month:", full_hours_avg, "h", minutes_avg, "min"))
    
    today_hours <- df %>% subset(as.Date(date) == Sys.Date()) %>% .$hours_worked
    
    
    if (is.na(last(info_all$stop))) {
      running_hours <- 
        difftime(Sys.time(),
                 as.POSIXct.default(last(info_all$start)),
                 units = "hour") %>%
        as.double
      today_hours <- (today_hours + running_hours) %>% round(2)
    }
    
    print(paste(today_hours, "hours worked today."))
  }


}


set_day <- function(date_string, hours = 6) {
  "Set working time of a day in past.

  ARGUMENTS
  date_string - str. Date in format 'YYYY-MM-DD' 
  hours - numerical. Number of work hours to set for given day.
  "

  df <- read.csv(CLOCK_PATH,
                 stringsAsFactors = FALSE)

  absence_day <- df[grepl(date_string, df$start), ] 
  if(nrow(absence_day) > 0) {
    old_hours <- round(sum(absence_day$time), 2)
    q_str <- paste0("Already ", old_hours, " hours marked on ", date_string,
                  ". Replace with ", hours, " hours? ")
    do_change <- askYesNo(q_str)
    if (is.na(do_change) | !do_change) {
      print("Setting hours cancelled.")
      return()
    }
  }
  df <- df[!grepl(date_string, df$start), ]
  start <- paste(date_string, "08:00:00")  
  stop <- (as.POSIXct(start) + (hours*60*60)) %>% strftime()

  df <- rbind(df, c(start, stop, hours))
  df <- df[order(df$start), ]
  
  write.csv(df,
            file = CLOCK_PATH,
            row.names = FALSE)
  
}
