#Internet usage by age group
barplot(Internet_Usage_by_age$`Percent (Some location)`, names.arg = c("3-17","18-34","35-44","45-64","65 or older"), xlab = "Age Groups", ylab = "percent who use internet")

#Internet usage by education
#barplot(Internet_Usage_by_education$`Percent(has)`, col = rainbow(20), 
#names.arg = (Internet_Usage_by_education$` .Educational attainment`), cex.names = .70, xlab = "Education level", ylab = "percent of who have internet at home")
subset <- t(data.frame(Internet_Usage_by_education$`Percent(has)`, Internet_Usage_by_education$`percent(doesn't)`))
barplot(subset, legend = c("Has internet", "No Internet"), names.arg=Internet_Usage_by_education$` .Educational attainment`,
        cex.names = .7, col = rainbow(2), beside=TRUE,xlab = "Education level", ylab = "percentage of people", las =1)

#internet by state
barplot(Internet_Usage_by_state$`Number (From home)`, col = rainbow(50), 
        names.arg = Internet_Usage_by_state$Location, cex.names = .70, cex.axis = .70, xlab = "", 
        ylab = "Amount who have internet at home (in Thousands)", las = 2)
