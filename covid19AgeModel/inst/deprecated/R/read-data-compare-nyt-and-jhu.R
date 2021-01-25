nyt <- readRDS("usa/data/nyt_death_data_padded.rds")
jhu <- readRDS("usa/data/jhu_death_data_padded.rds")

plot_cases <- function(state, nyt, jhu){
    plot.new()

    plot(jhu[jhu$code == state,]$date, 
        jhu[jhu$code == state,]$cumulative_cases, 
        xlab = "Date", 
        ylab = "Cumulative Cases", 
        type = "l", 
        col = 'red',
        main = sprintf("%s Cumulative Cases", state))
    lines(nyt[nyt$code == state,]$date, 
        nyt[nyt$code == state,]$cumulative_cases, 
        col = 'green')

    p <- recordPlot()
    return(p)
}


plot_deaths <- function(state, nyt, jhu){
    plot.new()

    plot(jhu[jhu$code == state,]$date, 
        jhu[jhu$code == state,]$cumulative_deaths, 
        xlab = "Date", 
        ylab = "Cumulative Deaths", 
        type = "l", 
        col = 'red',
        main = sprintf("%s Cumulative Deaths", state))
    lines(nyt[nyt$code == state,]$date, 
        nyt[nyt$code == state,]$cumulative_deaths, 
        col = 'green')

    p <- recordPlot()
    return(p)
}


for (state in unique(nyt$code)){
    png(sprintf("usa/figures/%s_nyt_and_jhu_cases.png", state))
    plot_cases(state, nyt, jhu)
    dev.off()
}


for (state in unique(nyt$code)){
    png(sprintf("usa/figures/%s_nyt_and_jhu_deaths.png", state))
    plot_deaths(state, nyt, jhu)
    dev.off()
}