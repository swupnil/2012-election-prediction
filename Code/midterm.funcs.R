
make.prior.table = function(file1, file2){
  # takes the election results 04 and 08 files and returns the average 
  # of the two years results for Rep%, Dem%, and Other%  
  results04 = file1
  results08 = file2
  results04 = results04[-nrow(results04), ]
  results08 = results08[-nrow(results08), ]
  rownames(results04) = as.character(results04[,1])
  rownames(results08) = as.character(results08[,1])
  results04 = results04[,-c(1, 5)]
  results08 = results08[,-c(1, 5)]
  results08 = results08[, c(2,1,3)]
  DCindex = which(rownames(results08) == 'D. C.')
  new.names = c(state.abb[1:(DCindex-1)], 'DC', state.abb[(DCindex):length(state.abb)])
  rownames(results04) = new.names; rownames(results08) = new.names
  
  prior.dat = (results04+results08)/2*.01
  names(prior.dat) = c('R', 'D', 'Other')
  
  return(prior.dat)
}

get.e.votes = function(year){
  # takes 2008 or 2012 and returns 270 or 263, based on omitted states due to no polling data
  if (!is.element(year, c(2008, 2012))){
    stop('Only 2008 and 2012 implemented')
  }
  if (year == 2008){
    to.win = 270 # later, 3 e votes given to Dem for omitted DC = blue state
  } else {
    to.win = 263 # assume 6 votes to each candidate; R gets 3 from WY and 3
                 # from AL, D gets 3 from DC and 3 from DE
  }
  return(to.win)
}

get.avail.states = function(folderName){
  # takes the name of the directory containing .dat poll data files and
  # returns the abbreviated states with a .dat file in the directory
  # ONLY THE STATES RETURNED BY THIS FUNCTION ARE LOOPED OVER IN THE STATE LOOP
  # see 'get.e.votes' function for information regarding the allocation of e 
  # votes for states that are not included in the vector returned by this function
  avail.dat.file.names = list.files(folderName)
  avail.dat.states = gsub('(*.).dat', '\\1', avail.dat.file.names)
  states = state.abb[is.element(state.abb, avail.dat.states)]
  return(states)
}

make.e.vote.post = function(e.vote.table, states = states, d.iter.table = d.iter.table){
  # takes d.iter.table, a numsteps by numstates table of posterior samples of
  # the difference between the rep and dem vote %; takes table of number of
  # e votes by state; returns numsteps samples of the posterior number of 
  # e votes for each
  
  V = e.vote.table
  V = data.frame(V)
  V[,1] = as.character(V[,1])
  V[-which(V[,1] == 'Washington D.C.'), 1] = state.abb
  V[which(V[,1] == 'Washington D.C.'), 1] = 'DC'
  
  votes = V[is.element(V[,1], states), 2]
  
  r.win.state.table = d.iter.table>0
  d.win.state.table = d.iter.table<0
  
  r.total.e.votes = r.win.state.table%*%votes
  d.total.e.votes = d.win.state.table%*%votes
  if (length(states)==50){ #missing DC
    d.total.e.votes = d.total.e.votes + 3
  }
  temp = list(r.total.e.votes, d.total.e.votes)
  return(temp)
}