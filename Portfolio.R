library(mosaic)
library(quantmod)
library(foreach)


#### Now use a bootstrap approach
#### With more stocks

mystocks = c("WMT", "TGT", "XOM", "MRK", "JNJ")
myprices = getSymbols(mystocks, from = "2007-01-01")


# A chunk of code for adjusting all stocks
# creates a new object adding 'a' to the end
# For example, WMT becomes WMTa, etc
for(ticker in mystocks) {
	expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
	eval(parse(text=expr))
}

tail(WMTa)

# Combine all the returns in a matrix
all_returns = cbind(	ClCl(WMTa),
								ClCl(TGTa),
								ClCl(XOMa),
								ClCl(MRKa),
								ClCl(JNJa))
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)



# Now loop over two trading weeks
# let's run the following block of code 5 or 6 times
# to eyeball the variability in performance trajectories

## begin block
total_wealth = 10000
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = weights * total_wealth
n_days = 10  # capital T in the notes
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
	return.today = resample(all_returns, 1, orig.ids=FALSE)  # sampling from R matrix in notes
	holdings = holdings + holdings*return.today
	total_wealth = sum(holdings)
	wealthtracker[today] = total_wealth
}
total_wealth
plot(wealthtracker, type='l')
## end block

# Now simulate many different possible futures
# just repeating the above block thousands of times
initial_wealth = 10000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
	total_wealth = initial_wealth
	weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
	holdings = weights * total_wealth
	n_days = 10
	wealthtracker = rep(0, n_days)
	for(today in 1:n_days) {
		return.today = resample(all_returns, 1, orig.ids=FALSE)
		holdings = holdings + holdings*return.today
		total_wealth = sum(holdings)
		wealthtracker[today] = total_wealth
	}
	wealthtracker
}


# each row is a simulated trajectory
# each column is a data
head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)

# note: this is  a negative number (a loss, e.g. -500), but we conventionally
# express VaR as a positive number (e.g. 500)

