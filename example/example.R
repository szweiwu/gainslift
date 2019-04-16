# Load our functions
Sys.setenv("TAR" = "internal")
devtools::install_github('szweiwu/gainslift')
library(gainslift)

# Load modeling output
source('./example/modeling.R')

# Put predictions into dataframes
bank_1predict <- data.frame("bank_rf" = bank.rf.pred[,"1"])
bank_3predict <- data.frame("bank_rf" = bank.rf.pred[,"1"], "bank_rp" = bank.rp.pred[,"1"], "bank_nb" = bank.nb.pred[,"1"])
bank_actual <- as.numeric(bank.test$y)-1

credit_1predict <- data.frame("credit_rf" = credit.rf.pred[,"1"])
credit_3predict <- data.frame("credit_rf" = credit.rf.pred[,"1"], "credit_rp" = credit.rp.pred[,"1"], "creditk_nb" = credit.nb.pred[,"1"])
credit_actual <- as.numeric(credit.test$y)-1

bank_vs_credit <- list(bank=list(bank_1predict, bank_actual), credit=list(credit_1predict, credit_actual))

########## Plot Gains Charts ##########
# Cumulative Gains Chart
## 1 curve
plot.gains(bank_1predict, bank_actual)
## multiple curves
plot.gains(bank_3predict, bank_actual)
## multiple curves with different datasize
plot.gains.diffsize(bank_vs_credit)


# Cumulative Decile Gains Chart
## 1 curve
plot.gains(bank_1predict, bank_actual, type = 'decile')
## multiple curves
plot.gains(bank_3predict, bank_actual, type = 'decile')
## multiple curves with different datasize
plot.gains.diffsize(bank_vs_credit, type = 'decile')


# Cumulative Profit Gains Chart
## 1 curve
profitgainsobj <- gainsliftTable(bank_1predict, bank_actual, benefit = 10, cost = 1.5)
profitgainschart <- gainschart(profitgainsobj)
profitgainschart
write.csv(profitgainschart$data$bank_rf, 'C:/Users/User/Desktop/table.csv')
## multiple curves
plot.gains(bank_3predict, bank_actual, benefit = 20, cost = 5)


# Cumulative Decile Profit Gains Chart
## 1 curve
plot.gains(bank_1predict, bank_actual, type = 'decile', benefit = 20, cost = 5)
## multiple curves
plot.gains(bank_3predict, bank_actual, type = 'decile', benefit = 20, cost = 5)


# Non-cumulative Decile Gains Chart
## 1 curve
plot.gains(bank_1predict, bank_actual, type = 'noncum')
## multiple curves
plot.gains(bank_3predict, bank_actual, type = 'noncum')
## multiple curves with different datasize
plot.gains.diffsize(bank_vs_credit, type = 'noncum')


# Non-cumulative Decile Profit Gains Chart
## 1 curve
plot.gains(bank_1predict, bank_actual, type = 'noncum', benefit = 20, cost = 5)
## multiple curves
plot.gains(bank_3predict, bank_actual, type = 'noncum', benefit = 20, cost = 5)



########## Plot Lift Charts ##########
# Cumulative Lift Chart
## 1 curve
plot.lift(bank_1predict, bank_actual)
## multiple curves
plot.lift(bank_3predict, bank_actual)
## multiple curves with different datasize
plot.lift.diffsize(bank_vs_credit)


# Cumulative Decile Lift Chart
## 1 curve
plot.lift(bank_1predict, bank_actual, type = 'decile')
## multiple curves
plot.lift(bank_3predict, bank_actual, type = 'decile')
## multiple curves with different datasize
plot.lift.diffsize(bank_vs_credit, type = 'decile')


# Cumulative Profit Lift Chart
## 1 curve
plot.lift(bank_1predict, bank_actual, benefit = 20, cost = 5)
## multiple curves
plot.lift(bank_3predict, bank_actual, benefit = 20, cost = 5)


# Cumulative Decile Profit Lift Chart
## 1 curve
plot.lift(bank_1predict, bank_actual, type = 'decile', benefit = 20, cost = 5)
## multiple curves
plot.lift(bank_3predict, bank_actual, type = 'decile', benefit = 20, cost = 5)


# Non-cumulative Decile Lift Chart
## 1 curve
plot.lift(bank_1predict, bank_actual, type = 'noncum')
## multiple curves
plot.lift(bank_3predict, bank_actual, type = 'noncum')
## multiple curves with different datasize
plot.lift.diffsize(bank_vs_credit, type = 'noncum')

# Non-cumulative Decile Profit Lift Chart
## 1 curve
plot.lift(bank_1predict, bank_actual, type = 'noncum', benefit = 20, cost = 5)
## multiple curves
plot.lift(bank_3predict, bank_actual, type = 'noncum', benefit = 20, cost = 5)
