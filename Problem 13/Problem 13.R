normal.months = c(31, #Januraru
               28, #February
               31, #March
               30, #April
               31, #May
               30, #June
               31, #July
               31, #August
               30, #September
               31, #October
               30, #November
               31) #December

leap.months = c(31, #Januraru
               29, #February
               31, #March
               30, #April
               31, #May
               30, #June
               31, #July
               31, #August
               30, #September
               31, #October
               30, #November
               31) #December

total.months = 0

for (y in 1900:2000) {
  if (y %% 4 == 0) {
    if (y %% 100 == 0) {
      if (y %% 400 == 0) {
        total.months = c(total.months, leap.months)
      } else {
        total.months = c(total.months, normal.months)
      }
    } else {
      total.months = c(total.months, leap.months)
    }
  } else {
    total.months = c(total.months, normal.months)
  }
}

total.months = subset(total.months, total.months != 0)

day = 1
sundays = 0

for (m in 1:length(total.months)) {
  day = ((day - 1 + total.months[m]) %% 7) + 1
  if (day == 7) {
    sundays = sundays + 1
  }
}

sundays2 = 0

for (m in 1:length(normal.months)) {
  day = ((day - 1 + normal.months[m]) %% 7) + 1
  if (day == 7) {
    sundays2 = sundays2 + 1
  }
}

sundays = sundays - sundays2
cat(sundays, "\n")
