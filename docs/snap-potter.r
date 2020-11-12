# Expanded from RSieana Lab 3 of IEMS 341 class
install.packages("RSiena")  ## just do it one time (the first time that you run this code)
# install.packages("xtable") ##this is optional - create a result table for LaTex
# install.packages("texreg") ##this is optional - create a result table
install.packages("statnet")
library(RSiena)
library(statnet)

?RSiena
?sienaNet
list.files() # directory check


# Load data and visualize 
book1 <- as.matrix(read.table("hpbook1.txt"))
book2 <- as.matrix(read.table("hpbook2.txt"))
book3 <- as.matrix(read.table("hpbook3.txt"))
book4 <- as.matrix(read.table("hpbook4.txt"))
book5 <- as.matrix(read.table("hpbook5.txt"))
book6 <- as.matrix(read.table("hpbook6.txt"))
hp.attributes <- as.matrix(read.table("hpattributes.txt", header=TRUE))
		

net1 <- as.network(book1)
net2 <- as.network(book2)
net3 <- as.network(book3)
net4 <- as.network(book4)
net5 <- as.network(book5)
net6 <- as.network(book6)

# plot sociomatrix - the filled cells indicate 1 (there is a tie), while blank cells indicate 0 (no tie)
plot.sociomatrix(net1,drawlab=F,diaglab=F,xlab='friendship t1') ##if drawlab = T, you'll see the labels of the nodes
plot.sociomatrix(net2,drawlab=F,diaglab=F,xlab='friendship t2')
plot.sociomatrix(net3,drawlab=F,diaglab=F,xlab='friendship t3')

# ------------------------------------------------------------------------------------
# Siena Model 
# ------------------------------------------------------------------------------------

# A number of objects need to be created in R: 
# A: dependent variables
# B: explanatory (independent) variables
# C: combination of dependent and explanatory variables
# D: model specification

# Create objects for the dependent variables.
friendship <- sienaNet(array(c(book1, book2, book3), dim=c(64, 64, 3)))
?sienaNet

# Attributes:
schoolyear <- coCovar(hp.attributes[,2])
gender     <- coCovar(hp.attributes[,3])
house      <- coCovar(hp.attributes[,4])

#my data
mydata <- sienaDataCreate(friendship, schoolyear, gender, house)

# The function getEffects creates a dataframe of effects with a number of extra
# properties for use in RSiena:
myeff <- getEffects(mydata)
mymyeff$include[]

# Before we explain the object myeff and how we shall be going to use it,
# we first produce a data description which is available now:
print01Report(mydata, modelname = 's50_3_init') ## this creates a file in your working directory

# EFFECTS
## TRY IF REDUCE BOOKS... myeff <- includeEffects(myeff, gwespFF,parameter=69)
		
# First question
myeff <- includeEffects(myeff, sameXRecip, interaction1 = "gender")

# Second question is included by default

# Third question
myeff <- includeEffects(myeff, altX, interaction1 = "gender")

# Fourth question
myeff <- includeEffects(myeff, sameX, interaction1 = "house")

#myeff <- includeEffects(myeff, AltX, interaction1 = "gender")
#myeff <- includeEffects(myeff, simX, interaction1 = "school_year")
## myeff <- includeEffects(myeff, altXOutAct, interaction1 = "gender")
#myeff <- includeEffects(myeff, sameXInPop, interaction1 = "house")
# We check the results again:
myeff
  


# Estimation of parameters
EstAlgorithm <- sienaAlgorithmCreate(projname="Hogwarts24", cond = FALSE)

ans1 <- siena07(EstAlgorithm, data=mydata, effects=myeff, batch=TRUE, verbose=FALSE, returnDeps = TRUE)

ans1

ans2 <- siena07(EstAlgorithm, data=mydata, effects=myeff, prevAns=ans1, returnDeps = TRUE)

ans2

ans3 <- siena07(EstAlgorithm, data=mydata, effects=myeff, prevAns=ans2, returnDeps = TRUE)

ans3

### MODEL IS BUILT... SECTION BELOW IS ON ESTIMATION.


myeff
# The parameter estimates in ans1 then are  extracted and used in the new estimation,
# Moreover, Phase 1 will be omitted from the algorithm, as derivatives and covariance matrix are used from the previous run.
# This should be used only if the model specification in myeff 
# has not changed, and if the provisional parameter estimates obtained
# in ans1 are resaonable; if they are not reasonable,
# omit the prevAns option, use
mymodel$useStdInits <- TRUE

# to get back on track, and return later to
mymodel$useStdInits <- FALSE

# The results of the estimation can also be accessed in various ways within R. 
# For example,
ans1$theta

# contains the vector of parameter estimates while
ans1$covtheta

# if you want to create a table of results
# Option A:
library(xtable) ## if you haven't installed it, install it first using the command on the top of the script
# A table formatted for inclusion in a LaTeX document is produced by
xtable(ans1)
# and this function can also produce a table in html, or write to file; e.g.:
# xtable(ans1, type='html')
#xtable(ans1, file='ff.tex')

# At http://cran.r-project.org/web/packages/xtable you can find
# a set of vignettes for the xtable package, the xtable gallery,
# which gives more options.

# Option B:
library(texreg) ## if you haven't installed it, install it first using the command on the top of the script
# texreg(ans1) ## for LaTex
screenreg(ans1)
?texreg

# -------------------------------------------------------------------------------------------
# More on initializing parameters for estimation 
# -------------------------------------------------------------------------------------------

# Above we treated the use of the prevAns option in siena07.
# Another and more flexible way for determining initial values is by 
# using the useStdInits element of the model object,
# and the initial values in the effects object. This is done as follows.
# The option useStdInits = TRUE in sienaModelCreate, will make
# each estimation run start with standard initial values.
# The option useStdInits = FALSE makes the estimation start
# with the initial values in the effects object.
# You can switch between these by commands such as
#mymodel$useStdInits <- FALSE
#mymodel$useStdInits <- TRUE

# Putting the estimates from the results object ans1 into the
# effects object myeff, if ans1 used conditional estimation, is done by
myeff$initialValue[myeff$include] <- ans1$theta

# and if conditional estimation was used, conditioning on the first dependent network, by
#myeff$initialValue[myeff$include] <- c(ans1$rate, ans1$theta)

# Check that the effects object contains the new initial values is made by
myeff

# By using a different vector instead of ans1$theta you can
# initialise differently.
# Note that this initial vector will be used until you change it again,
# e.g., to the results of a new run, or until you change the useStdInits option.
# Also note that you should do this before changing the model,
# because else the vectors will have incompatible lengths.

# A utility for directly extracting estimates from a sienaFit object
# and copying these estimates to initial values in a sienaEffects object
# is the function transferEstimates contained in the utilities file
# on the "RSiena scripts" page of the Siena website,
# http://www.stats.ox.ac.uk/~snijders/siena/

# When unsatisfactory convergence was obtained, the first thing to do is
# to run siena07 repeatedly with useStdInits=FALSE,
# updating the initial values with the results of the last estimation
# as indicated here (usually prevAns is the easiest way), 
# and continuing until convergence is satisfactory,
# as indicated by the t-ratios for convergence all being less than
# a value of about 0.10. 

# Goodness of fit test
gofin  <- sienaGOF(ans1, IndegreeDistribution, verbose=TRUE, join=TRUE, varName="friendship")
plot(gofin)

gofout <- sienaGOF(ans1, OutdegreeDistribution, verbose=TRUE, join=TRUE, varName="friendship")
plot(gofout)
