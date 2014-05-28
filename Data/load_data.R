
##################################
##################################
# THIS FILE IS TO LOAD IN DATA FOR THE MIDTERM 2 PROJECT



##################################
### LOAD IN 2008 POLLING DATA
##################################

# get list of files in the polls_2008 dir
files2008 = list.files("polls_2008")

# store state abbreviations
states = substr(files2008,1,2)

# initialize list
polldata2008 = list()

# fill in list with poll data from each state
for(ii in 1:length(states)){
  polldata2008[[states[ii]]] = read.table(paste("polls_2008/",files2008[ii],sep=""),
                sep=",",header=TRUE)
}

##################################
# Example use:
# access California data
polldata2008[["CA"]]

# access Ohio data
polldata2008[["OH"]]

##################################
### LOAD IN 2012 POLLING DATA
##################################
# get list of files in the polls_2012 dir
files2012 = list.files("polls_2012")

# initialize list
polldata2012 = list()

# fill in list with poll data from each state
for(ii in 1:length(states)){
  # only open the polling data file if it exists
  if(paste(states[ii],".dat",sep="") %in% files2012){
    polldata2012[[states[ii]]] = read.table(paste("polls_2012/",states[ii],".dat",sep=""),
                  sep=",",header=TRUE)
  } else{
    polldata2012[[states[ii]]] = NULL
  }
}

##################################
# Example use:
# access California data
polldata2012[["CA"]]

# access Ohio data
polldata2012[["OH"]]


##################################
### LOAD IN 2012 ELECTORAL COLLEGE DATA
##################################
electCollege = read.table("electoral_college_2012.dat",sep=",",header=TRUE)

##################################
### LOAD IN 2004 & 2008 ELECTION RESULTS
##################################
outcome04 = read.table("election_outcome_2004.dat",sep=',',header=TRUE)
outcome08 = read.table("election_outcome_2008.dat",sep=',',header=TRUE)

##################################
### LOAD IN UNEMPLOYMENT DATA
##################################
unemp = read.table("unemployment.dat",sep=',',header=TRUE)







