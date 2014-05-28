require(rjags)
setwd("/Users/Swupnil/Documents/School/Berkeley/12. Fall/Stat 157/Midterm Project")
source('midterm.funcs.R')
##########################
### Begin user defined ###
##########################
results04 = read.table('./Data/election_outcome_2004.dat', header = T, sep = ',') #########################
results08 = read.table('./Data/election_outcome_2008.dat', header = T, sep = ',') #########################
year = 2008
poll.folder = paste('./Data/polls_', year, sep = '')
uninformative.prior = F
# if T, prior ~ Dirich(.5, .5, .5); if F, method implemented (see below)
plot.states = F
# if T, 3 plots for each state generated in the state loop - trace plots and posterior data
dateCutoff = F
# if F then the num.polls cutoff is used instead
cutoffDates = c(25, 90)
# likelihood includes only polls conducted between the first element and the 
# second element, that is, polls conducted between the fewest number of days 
# before the election and the largest number of days before the election; 
# 19 DAYS IS THE FEWEST NUMBER OF DAYS BEFORE ELECTION AVAIL FOR 2012, THUS 
# IT MAY BE IMPORTANT TO ONLY USE POLLS AT LEAST 19 DAYS BEFORE THE 2008 
# ELECTION FOR MODEL SELECTION/VALIDATION PURPOSES!
num.polls = 10
# likelihood only includes the num.polls polls nearest to the election
### NOTE THAT A STATE THAT HAS NO POLLS IN THE cutoffDates RANGE WILL INSTEAD USE
### ONLY THE MOST RECENT POLL. LIKEWISE, IF LESS THAN num.polls ARE AVAIL FOR
### A GIVEN STATE, ONLY THE AVAIL NUMBER OF POLLS ARE USED IN THE LIKELIHOOD
d.und = .01
# OK has a 0% prior probability of undecided/3rd party voters
# we change this to d.und probability and reduce the other two
# by .5*d.und in order to use valid hyperparameters
c = 50
# See Rigdon paper; c is the constraint for the sum of hyperparameters
adaptSteps = 500            # Number of steps to "tune" the samplers.
burnInSteps = 1000          # Number of steps to "burn-in" the samplers.
nChains = 3                 # Number of chains to run.
numSavedSteps = 10000       # Total number of steps in chains to save.
thinSteps = 1               # Number of steps to "thin" (1=keep every step).
##########################
###  End user defined  ###
##########################

if (year == 2012){prior.dat = make.prior.table(results04, results08)}
if (year == 2008){prior.dat = make.prior.table(results04, results04)} # use only 04 to set prior
to.win = get.e.votes(year)
states = get.avail.states(poll.folder)
numPollsLimit = ifelse(dateCutoff, F, T) 
# two methods currently supported for trimming the
# entire pool of available polls for 08/12; either dateCutoff or numPollsLimit
parameters = c('theta')
# The parameter(s) to be monitored.
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
# Steps per chain.
d.iter.table = matrix(rep(NA), nrow = nIter*nChains, ncol = length(states))


##########################
###  Begin state loop  ###
##########################
for (state in states){
    
  rawdat = read.table(paste('./Data/polls_', year,
                            '/', state, '.dat', sep = ''), sep = ',', header = T)
  names(rawdat) = c('firm', 'dates', 'R', 'D', 'N', 'nDays')
  
  
  if (dateCutoff){
    poll.index = rawdat$nDays >= cutoffDates[1] & rawdat$nDays <= cutoffDates[2]
    if (sum(poll.index) == 0){
      earliest.avail = min(rawdat$nDays[rawdat$nDays>cutoffDates[2]])
      poll.index = rawdat$nDays == earliest.avail
    }
  } else if (numPollsLimit){
    days.order = order(rawdat$nDays)
    poll.index = days.order <= num.polls
  }
  
  dat = rawdat[poll.index, c('R', 'D', 'N')]
  dat = dat[!apply(is.na(dat), 1, sum),]
  
  nObs = dat$N
  x.rep = round(.01*dat$R*nObs, 0)
  x.dem = round(.01*dat$D*nObs, 0)
  x.und = nObs - x.rep - x.dem
  nPolls = nrow(dat)
  
  model.string = '
model {
  for ( i in 1:k){
  # Likelihood:
  x[i,] ~ dmulti(theta, n[i])
  }
  theta ~ ddirch(alpha)
}'
  
  # model.string to file:
  writeLines(model.string, con= "model.txt")
  
  state.prior.mean.target = prior.dat[state, ]
  if (state.prior.mean.target[3] == 0){
    temp = state.prior.mean.target
    temp[1:2] = temp[1:2] - .5*d.und
    temp[3] = temp[3] + d.und
    state.prior.mean.target = temp
  }
  state.alphas = as.numeric(state.prior.mean.target*c)
  
  if (uninformative.prior){
    data.list = list(alpha = c(.5, .5, .5), x = matrix(c(x.rep, x.dem, x.und), byrow = F, ncol = 3), n = nObs, k = nPolls)
  } else {
    data.list = list(alpha = state.alphas, x = matrix(c(x.rep, x.dem, x.und), byrow = F, ncol = 3), n = nObs, k = nPolls)
  }
  
  jagsModel = jags.model("model.txt" , data=data.list , # inits=initsList , 
                         n.chains = nChains , n.adapt = adaptSteps )
  # Burn-in (until convergence):
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter = burnInSteps )
  # Now sample the saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names = parameters , 
                              n.iter = nIter , thin = thinSteps )
  # codaSamples indeces: codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  # collapse three chains of 2000 samples from p(theta1, theta2, theta3|x)
  thetaSample = as.matrix( codaSamples )
  numActualSteps = nrow(thetaSample)
  cols = c(2,4,6)
  if (plot.states){
    matplot(1:numActualSteps, thetaSample, typ='l', ylab=expression(theta), col = cols,
            xlab = 'iteration', main = paste('Trace plot,', state))
    legend(numActualSteps/2.5, .3, cex = .8, legend = c('Rep', 'Dem', 'Und'), col = cols, lty = 1)
    abline(v=c(numActualSteps/3,2*numActualSteps/3),lwd=1,lty=2,col=1)
    transparent.cols = c(rgb(.8, .2, .1, alpha = .8), rgb(.1, .2, .9, alpha = .6))
    hist(thetaSample[,1], col=transparent.cols[1], freq=FALSE, breaks=30, 
         main = 'Posterior of Election results', xlab = expression(theta),
         xlim = c(min(thetaSample[,1:2]), max(thetaSample[,1:2])), .8)
    hist(thetaSample[,2], col=transparent.cols[2], freq=FALSE, breaks=30, add = T)
  }
  post.state.winner = ifelse(median(thetaSample[,1]-thetaSample[,2])>0, 1, 2)
  post.state.loser = ifelse(post.state.winner == 1, 2, 1)
  d = thetaSample[,post.state.winner] - thetaSample[,post.state.loser]
  if (plot.states){
    hist(d, col=transparent.cols[post.state.winner], freq=FALSE, breaks=30, 
         main = 'Posterior of Winning margin', xlab = expression(theta))
    quant.vec = quantile(d, probs = c(0.025, .5, .975))
    abline(v = quant.vec, lty = 2, lwd = c(1,2,1))
  }
  d.iters = thetaSample[,1] - thetaSample[,2]
  d.iter.table[, which(states == state)] = d.iters
}
##########################
###   End state loop   ###
##########################

##########################
### E vote posteriors  ###
##########################
e.votes = read.table('./Data/electoral_college_2012.dat', header = T, sep = ',')[-1,]
e.vote.list = make.e.vote.post(e.votes, states, d.iter.table)
r.total.e.votes = c(e.vote.list[[1]])
d.total.e.votes = c(e.vote.list[[2]])

##########################
### E vote post plots  ###
##########################
numBreaks = 30
mini = min(r.total.e.votes, d.total.e.votes); maxi = max(r.total.e.votes, d.total.e.votes)
hist(r.total.e.votes, breaks = numBreaks, col = rgb(.7, .2, .2, alpha = .8), xlim = c(mini, maxi),
     main = 'Posterior of final electoral vote count')
hist(d.total.e.votes, breaks = numBreaks, col = rgb(.2, .2, .8, alpha = .7), add = T)
abline(v = to.win, lty = 2, lwd = 2)
abline(v = c(median(r.total.e.votes), median(d.total.e.votes)), col = cols[1:2], lty = 3, lwd = 3)

print(paste('P(Rep wins) = ', round(mean(r.total.e.votes>to.win), 4), sep = ''))
print(paste('P(Dem wins) = ', round(mean(d.total.e.votes>to.win), 4), sep = ''))
print(paste('P(Tie) = ', round(mean(r.total.e.votes == to.win & d.total.e.votes == to.win), 4), sep = ''))
print(paste('Median Dem Electoral Vote count =', median(d.total.e.votes))) 
if (year == 2008){ print('Actual Dem Electoral Vote count = 365')}
if (year == 2008){ print(paste('Deviation =', (365-median(d.total.e.votes))/3.65,'%'))}
##########################
###     R win plot     ###
##########################
r.vote.cap = 300 # this plot intends to show the scenarios where the rep cand
# (b/c the rep is the underdog) wins the election; I omit post of e votes with
# greater than r.vote.cap b/c I assume the candidate does better in all states
# in order to win by a wide margin when he is a big underdog; this plot
# attempts to capture a look at what needs to happen for the rep to win
# and obviously if he does better than expected in all states he will win. 

if (mean(r.total.e.votes>to.win)>0){
  r.win.state.table = d.iter.table>0
  r.win.scenarios = r.win.state.table[which(r.total.e.votes>to.win & r.total.e.votes<r.vote.cap), ]
  all = apply(r.win.state.table, 2, mean)
  r.win = apply(r.win.scenarios, 2, mean)
  if (length(r.win)>0)
    plot(all, r.win, type = 'n')
  abline(0,1)
  text(all, r.win, states, cex = .7)
}
