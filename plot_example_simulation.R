require(tikzDevice)
source('runs.R')

set.seed(14)

# Simulation parameters
n = 1000
m = 50
C = 2
gammas = get_gammas(m, 'geo')

# Simulate x-data
X = var1_sim(n, m, 0.999)

# Get true model
model = get_perfect_model(C, gammas, alpha=0)

# Get alarm parameters (default values for alarm_mw, alarm_w, alarm_lag)
alarm_params = list(alarm_delta=1.5, alarm_p=.55)

png('example_simulation.png', width=6*200, height=2.2*200, pointsize=20)
# pdf('example_simulation.pdf', width=6, height=2.2, pointsize=8)
# tikz('example_simulation.tex', width=6, height=2.2, pointsize=8, standAlone=F)

# Run single simulation and plot the results
result = single_run(X, y=NULL, model, plot=T, alarm_params=alarm_params)
print(result$cost)

lims = par('usr')
legend(lims[2], lims[3], xjust=1, yjust=0, bg='white', cex=.8,
       c(
         'Alarm threshold',
         'Defect probability',
         'Defect probability (no adjustments)',
         'Relative cost',
         'Relative cost (no adjustments)'
       ),
       col=c('black', 'black', 'black', 'red', 'red'),
       lty=c(3, 1, 5, 1, 5)
)

dev.off()




