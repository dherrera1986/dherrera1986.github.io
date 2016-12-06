require("dplyr")
require("tidyr")
require("ggplot2")
require("lubridate")
executions <- read.csv("Executions in US.csv")
executions2 <- read.csv("Executions U.S. 1608-2002.csv")
deathrow <- read.csv("Death Row Inmates By State.csv")
deathrow2 <- read.csv("Number of Death Row Inmates.csv")

latino <- filter(executions, Race == "Latino")
chart1 <- ggplot(latino,
                aes(x = Age, fill= "orange")) + 
                geom_bar() + 
                ggtitle("Latinos Executed In U.S. by Age") +
                ggsave("Latinos Executed In U.S.pdf")
               
chart2 <-ggplot(executions,
                aes(x= State, color = factor(Race))) +
                geom_line() +
                facet_wrap(~RACE, ncol = 5)  
              

yearly_totals <- group_by(executions2, YEAR, RACE) %>%
summarize(total=n())

chart3 <- ggplot(yearly_totals,
                 aes(x = YEAR, y= total, color = RACE)) +
                 geom_point(alpha = .9) + 
                 scale_size_area() +
                 ggtitle("Executions in U.S. by Race")

chart4 <- ggplot(executions2,
                 aes(x = STATE, fill = CRIME)) +
                 geom_bar(position = "stack") +
                 coord_flip() +
                 theme_dark() +
                 ggtitle("Crimes That Recieved Death Penalty In U.S. 1608-2002")

chart5 <- ggplot(deathrow2, 
                 aes(x = Year, y = Total)) + 
                 geom_area(fill= "light blue", color = "dark blue") +
                 ggtitle("Death Row Inmates By Year")
                 

#parse dates
executions_cleandates <- parse_date_time(executions$Date, "%d/%m/%Y")

testplot <- ggplot(yearly_totals2, aes(x = YEAR, color = CRIME)) + 
                   geom_bar()
                   



#allisons grouping
yearly_crimes <- group_by(executions2, YEAR, CRIME == "slave_revolt") %>%
summarize(total=n())


yearly_totals <- group_by(executions2, YEAR, RACE) %>%
summarize(total=n())


yearly_chart <- ggplot(yearly_crimes, aes(x=YEAR, y=total, color=CRIME)) +
geom_line()
print(yearly_chart)

slave_revolt_totals <- group_by(slave_revolt, YEAR) %>%
summarize(total=n())

yearly_totals2 <- group_by(executions2, YEAR, CRIME) %>%
summarize(total=n())
