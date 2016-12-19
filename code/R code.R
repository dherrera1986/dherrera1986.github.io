require("dplyr")
require("tidyr")
require("ggplot2")
require("lubridate")
require("scales")
executions2 <- read.csv("Executions U.S. 1608-2002.csv")
deathrow <- read.csv("Death Row Inmates By State.csv")
deathrow2 <- read.csv("Number of Death Row Inmates.csv")

yearly_race <- group_by(executions2, YEAR, RACE) %>%
summarize(total=n())

chart1 <-ggplot(yearly_race,
                aes(x= YEAR, y = total, color = RACE)) +
                geom_line(linetype = "solid", size = 1) +
                facet_wrap(~RACE, ncol = 2) + 
                ggtitle("U.S. Executions by Race, 1608-2002")
              
chart2 <- ggplot(deathrow2, 
                 aes(x = Year, y = Total)) + 
                 geom_area(fill= "light blue") +
                 geom_line() +
                 ggtitle("Death Row Inmates By Year, 1968-2016") +
                 scale_x_continuous(limits=c(1968,2016), breaks=seq(1968, 2016, 4))
                 

deathstat <- mutate(deathrow, percent = (Total/1500000 * 100000))
chart3 <- ggplot(deathstat, 
                 aes(x = reorder(State, Total), y = percent)) + 
                 geom_bar(stat = "identity", fill = "coral") + 
                 coord_flip() +
                 ggtitle("Death Row Inmates By State as of July 2016")


