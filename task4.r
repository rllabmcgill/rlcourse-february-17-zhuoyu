# compare Q(sigma) with other n-step algorithms

# the example is from Example 11.8 on http://artint.info/html/ArtInt_262.html



# dostep <- function(currX, currY, D, action, P, M)  { # P = c(X, Y) , indicating the corner location of prize
dostep0 <- function(currX, currY, D, action)  { # P = c(X, Y) , indicating the corner location of prize
	# D=1 for not damaged, D=2 for damaged
	P <- c(1,1)  # fixed prize location
	M <- data.frame(x=c(2,3,4,4,4), y=c(3,5,1,2,4))	
	action <- c("N", "E", "S", "W")[action]
	reward <- 0 # default
	if((currX==1 & action=="N")|	# hit the upper wall
		(currX==5 & action=="S")|	# hit the bottom wall
		(currY==1 & action=="W")|	# hit the left wall
		(currY==5 & action=="E")|	# hit the right wall
		((currX %in% c(1,2))&(currY==1)&(action=="E"))|	# hit the interior wall
		((currX %in% c(1,2))&(currY==2)&(action=="W"))|	# hit the interior wall
		((currX==1)&(currY==2)&(action=="E"))|			# hit the interior wall
		((currX==1)&(currY==3)&(action=="W"))){ 		# hit the interior wall
			reward=-1
			newX=currX
			newY=currY
	} else {	
		if (action=="N"){
			newX=currX-1
			newY=currY			
		} else if (action=="S"){
			newX=currX+1
			newY=currY			
		}else if (action=="E"){
			newX=currX
			newY=currY+1			
		}else if(action=="W"){
			newX=currX
			newY=currY-1			
		}
	} 

	if ((newX==P[1])&(newY==P[2])){ # land on P (prize)
		reward=10
	} else if ((newX==1)&(newY==2)) { # land on R (repair station)
		D=1	# not damaged
	# } else if ((newX==M[1])&(newY==M[2])) { # land on M (monster)
		# if(D==1) {	
			# D=2	# damaged
		# } else {	
			# reward=-10
		# }		
	} else {
		monster <- 0
		for (i in 1:nrow(M)){
			if((newX==M[i,1])&(newY==M[i,2])) { # land on M (monster)
				monster <- 1
			}
		}
		if(monster==1){
			if(D==1) {	
				D=2	# damaged
			} else {	
				reward=-10
			}			
		}
	}
	# update location of monster
	# M <- monster()
	# return(list(newX=newX, newY=newY, reward=reward, D=D, M=M))
	return(list(newX=newX, newY=newY, reward=reward, D=D))
}


# a simplized version of the environment
dostep <- function(currX, currY, action)  { # P = c(X, Y) , indicating the corner location of prize
	P <- c(1,1)  # fixed prize location
	M <- data.frame(x=c(2,3,4,4,4), y=c(3,5,1,2,4))	
	action <- c("N", "E", "S", "W")[action]
	reward <- 0 # default
	if((currX==1 & action=="N")|	# hit the upper wall
		(currX==5 & action=="S")|	# hit the bottom wall
		(currY==1 & action=="W")|	# hit the left wall
		(currY==5 & action=="E")|	# hit the right wall
		((currX %in% c(1,2))&(currY==1)&(action=="E"))|	# hit the interior wall
		((currX %in% c(1,2))&(currY==2)&(action=="W"))|	# hit the interior wall
		((currX==1)&(currY==2)&(action=="E"))|			# hit the interior wall
		((currX==1)&(currY==3)&(action=="W"))){ 		# hit the interior wall
			reward=-1
			newX=currX
			newY=currY
	} else {	
		if (action=="N"){
			newX=currX-1
			newY=currY			
		} else if (action=="S"){
			newX=currX+1
			newY=currY			
		}else if (action=="E"){
			newX=currX
			newY=currY+1			
		}else if(action=="W"){
			newX=currX
			newY=currY-1			
		}
	} 

	if ((newX==P[1])&(newY==P[2])){ # land on P (prize)
		reward=10
	} else if ((newX==1)&(newY==2)) { # land on R (repair station)
		D=1	# not damaged
	# } else if ((newX==M[1])&(newY==M[2])) { # land on M (monster)
		# if(D==1) {	
			# D=2	# damaged
		# } else {	
			# reward=-10
		# }		
	} else {
		for (i in 1:nrow(M)){
			if((newX==M[i,1])&(newY==M[i,2])) { # land on M (monster)
				reward <- -10
			}
		}
	}
	# update location of monster
	# M <- monster()
	# return(list(newX=newX, newY=newY, reward=reward, D=D, M=M))
	return(list(newX=newX, newY=newY, reward=reward))
}



monster <- function(){
	Monsters <- data.frame(x=c(2,3,4,4,4), y=c(3,5,1,2,4))
	return(as.numeric(Monsters[sample(1:5,1),]))  # change in every step
}

# select action
greedy <- function(Q, epsilon){	# Q = qvalue[X,Y,D,]
	if(runif(1) > epsilon){
		A <- which(Q %in% max(Q))
		if(length(A)>1) {
			A <- sample(A,1)
		}
	} else {
		A <-sample(1:4,1)
	}
	# return(c("N", "E", "S", "W")[temp])
	return(A)
}

# when more than one optimal actions, randomly sample one
optA <- function(Q){	# Q=c(X, Y, A)
	dims <- dim(Q)
	Max <- matrix(NA, nrow=dims[1], ncol=dims[2])
	for (i in 1:dims[1]){
		for(j in 1:dims[2]){
			temp <- which(Q[i,j,]==max(Q[i,j,]))
			if (length(temp)>1){
				temp <- sample(temp,1)
			}
			Max[i,j] <- temp
		}
	}
	return(Max)
}


#=====================================================================================================================


# On-policy first-visit MC control (for epsilon-soft policy) (Page 109)
# the epsilon (greedy level) is changed with number of iterations 
# another option is to start from the point with fewest visit numbers
# Initialization
# Q <- array(data=0, dim=c(5,5,2,4)) # X, Y=c(1:5), D=(1=not_damaged, 2=damaged), A=(1:4) for (N,E,S,W) 
FVMC <- function(){
Q <- array(data=0, dim=c(5,5,4)) # X, Y=c(1:5), A=(1:4) for (N,E,S,W)
t_rewards <- array(data=0, dim=c(5,5,4)) # X, Y=c(1:5), A=(1:4) for (N,E,S,W)
t_f_visits <- array(data=0, dim=c(5,5,4)) # X, Y=c(1:5), A=(1:4) for (N,E,S,W)
t_f_visits[1,1, ] <- 10000 # never move from this point
epsilon <- 0.90 # greedy policy
n_episode <- 0
delta20 <- rep(NA,20)
repeat{
	n_episode <- n_episode +1
	# (a) generate an episode using the policy
	S <- matrix(NA, nrow=100, ncol=2) # state episode, (X, Y)
	colnames(S) <- c("X", "Y")	
	A <- rep(NA, 100) # action episode
	R <- rep(NA,100) # rewards episode
	
	S[1,] <- c(5, 5)
	A[1] <- greedy(Q[S[1,"X"], S[1,"Y"], ], epsilon)
	t <- 0
	repeat {	# index start from 1, not 0
		t <- t + 1 
		if((t+1)>length(A)){	# if the pre-defined episode is not long enough, add 100 to it
			A <- c(A, rep(NA, 100))
			S <- rbind(S, matrix(NA, nrow=100, ncol=2))
			R <- c(R, rep(NA, 100))
		}

		response <- dostep(S[t,"X"], S[t,"Y"], A[t])
		R[t+1] <- response$reward
		S[t+1,] <- c(response$newX, response$newY)
		if(R[t+1]==10) {	# if S[t+1] is terminal
			# Q[S[t+1,"X"], S[t+1,"Y"], A[t+1]] <- R[t+1]
			break
		}		
		A[t+1] <- greedy(Q[S[t+1,"X"], S[t+1,"Y"], ], epsilon)
	}
	
	# (b) For each pair of S and A apearing in the episode
	episode <- cbind(S,A)
	episode <- na.omit(episode)
	rownames(episode) <- 1:nrow(episode)	
	fv <- unique(episode)	# first visit
	fv <- as.numeric(rownames(fv)) # first visit
	delta <- 0
	for (i in 1:length(fv)){
		# G[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]] <- sum(R[(fv[i]+1):length(R)])
		t_rewards[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]] <- t_rewards[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]] + sum(R[(fv[i]:nrow(episode))+1]) # total reward of each (S, A) pair
		t_f_visits[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]] <- t_f_visits[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]] +1  # total first visit of each (S,A) pair
		Q0 <- Q[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]]
		Q[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]] <- t_rewards[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]]/t_f_visits[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]]
		delta <- max(delta, abs(Q[episode[fv[i],"X"], episode[fv[i],"Y"], episode[fv[i],"A"]]-Q0)) 
	}
	
	delta20[n_episode%%20+1] <- delta
	# if(n_episode%%20==0){
		# epsilon <- epsilon*((n_episode%/%20)/(n_episode%/%20+1))
	# }
	
	# (c) For each S in the episode:
		# update policy 
		# this is done by the greedy function which samples the optimal action with soft-greedy policy
	
	# stop criterion
	# check if the optimal actions are changed
	# or simply to see if Q changed largely
	if(max(delta20,na.rm=TRUE) < 0.001){
		break
	}
}

optA(Q)
}

# comments to the previous code:
# This game is hard to train by MC. There are many monsters among the best route. 
# Therefore, there are many chance to get damaged by monsters when walking by random. 
# This decreases the following reward of the grids on the best route.
# If we decrease the epsilon, then there is less random walking, but some grids may have very few visits. 
# If a grid on the best route get a low reward in the beginning by chance, it may never be visited later on.




sarsa_0 <- function(alpha=0.5)
	{
	#================================================================================================================
	# SARSA(0)
	# Initialization
	# Q <- array(data=0, dim=c(5,5,2,4)) # X, Y=c(1:5), D=(1=not_damaged, 2=damaged), A=(1:4) for (N,E,S,W) 
	Q <- array(data=0, dim=c(5,5,4)) # X, Y=c(1:5), A=(1:4) for (N,E,S,W) 
	epsilon <- 0.10 # greedy policy
	# alpha <- 0.5 # step size 
	# n <- 4 # n-step algorithm
	gamma <- 0.9
	totalsteps <- rep(NA,100)
	totalrewards <- rep(NA,100)
	i <- 0
	is_opt <- rep(NA,20)
	repeat {
		i <- i + 1
		if(i>length(totalsteps)){
			totalsteps <- c(totalsteps, rep(NA, 100))
			totalrewards <- c(totalrewards, rep(NA, 100))
		}	
		# new episode
		# P <- c(sample(c(1,5),1), sample(c(1,5),1)) # prize location, random
		# S <- matrix(NA, nrow=100, ncol=7) # state trajectory, currX, currY, D, pX, pY, mX, mY
		# colnames(S) <- c("currX", "currY", "D", "pX", "pY", "mX", "mY")
		# S <- matrix(NA, nrow=100, ncol=3) # state trajectory, currX, currY, D
		# colnames(S) <- c("currX", "currY", "D")
		S <- matrix(NA, nrow=100, ncol=2) # state trajectory, currX, currY, D
		colnames(S) <- c("X", "Y")	
		A <- rep(NA, 100) # action trajectory
		R <- rep(NA,100) # rewards record
		# always start in the moddle
		# D <- 1	# dot_damaged

		# M <- monster()  fixed 
		# S[1,] <- c(currX, currY, D, P, M)  # the index is different from it in the book
		# S[1,] <- c(currX, currY, D)  # the index is different from it in the book
		
		S[1,] <- c(5, 5)
		# A[1] <- greedy(Q[S[1,"currX"], S[1,"currY"], S[1, "D"], ], epsilon)
		A[1] <- greedy(Q[S[1,"X"], S[1,"Y"], ], epsilon)
		tau <- 0
		t <- 0
		Q0 <- Q
		repeat {	# index start from 1, not 0
			t <- t + 1 
			if((t+1)>length(A)){
				A <- c(A, rep(NA, 100))
				# S <- rbind(S, matrix(NA, nrow=100, ncol=7))
				S <- rbind(S, matrix(NA, nrow=100, ncol=2))
				R <- c(R, rep(NA, 100))
			}

			# response <- dostep(currX, currY, D, A[t], P, M)
			response <- dostep(S[t,"X"], S[t,"Y"], A[t])
			# D <- response$D
			# M <- response$M
			# S[t+1,] <- c(currX, currY, D, P, M)
			R[t+1] <- response$reward
			if(R[t+1]==10|t>5000) {	# if S[t+1] is terminal
				# Q[S[t+1,"X"], S[t+1,"Y"], A[t+1]] <- R[t+1]
				break
			}
			S[t+1,] <- c(response$newX, response$newY)		
			A[t+1] <- greedy(Q[S[t+1,"X"], S[t+1,"Y"], ], epsilon)
			Q[S[t,"X"], S[t,"Y"], A[t]] <- Q[S[t,"X"], S[t,"Y"], A[t]] + alpha*(R[t+1] + gamma*Q[S[t+1,"X"], S[t+1,"Y"], A[t+1]] - Q[S[t,"X"], S[t,"Y"], A[t]])
		}

		totalsteps[i] <- t
		totalrewards[i] <- sum(R,na.rm=TRUE)
		
		# stop if optA is the true opt for 10 times
		test <- optA(Q)==optA(Q0)
		test[1,1] <- TRUE	# end point, force it to be true
		is_opt[i%%length(is_opt)+1] <- (sum(test)==nrow(test)*ncol(test))
		if(sum(is_opt,na.rm=TRUE)==length(is_opt)&i>20){
			break
		}
	}
	return(list(num_episode=i, totalrewards=na.omit(totalrewards)))
	# i
	# sum(totalrewards)
}




#===============================================================================================================
sarsa_n <- function(alpha=0.5, n=4){

	# n-step SARSA
	# Initialization
	# Q <- array(data=0, dim=c(5,5,2,4)) # X, Y=c(1:5), D=(1=not_damaged, 2=damaged), A=(1:4) for (N,E,S,W) 
	Q <- array(data=0, dim=c(5,5,4)) # X, Y=c(1:5), A=(1:4) for (N,E,S,W) 
	# pr_A <- array(data=0.25, dim=c(5,5,2,4)) # probability of taking action A at a given state
	pr_A <- array(data=0.25, dim=c(5,5,4)) # probability of taking action A at a given state
	epsilon <- 0.10 # greedy policy
	# alpha <- 0.2 # step size 
	# n <- 4 # n-step algorithm
	gamma <- 0.9
	totalsteps <- rep(NA,100)
	totalrewards <- rep(NA,100)
	i <- 0
	is_opt <- rep(NA,20)
	repeat {
		i <- i + 1
		if(i>length(totalsteps)){
			totalsteps <- c(totalsteps, rep(NA, 100))
			totalrewards <- c(totalrewards, rep(NA, 100))
		}
		# new episode
		# P <- c(sample(c(1,5),1), sample(c(1,5),1)) # prize location, random
		# S <- matrix(NA, nrow=100, ncol=7) # state trajectory, currX, currY, D, pX, pY, mX, mY
		# colnames(S) <- c("currX", "currY", "D", "pX", "pY", "mX", "mY")
		# S <- matrix(NA, nrow=100, ncol=3) # state trajectory, currX, currY, D
		# colnames(S) <- c("currX", "currY", "D")
		S <- matrix(NA, nrow=100, ncol=2) # state trajectory, currX, currY, D
		colnames(S) <- c("currX", "currY")	
		A <- rep(NA, 100) # action trajectory
		R <- rep(NA,100) # rewards record
		# always start in the moddle
		currX <- 5
		currY <- 5
		# D <- 1	# dot_damaged


		# M <- monster()  fixed 
		# S[1,] <- c(currX, currY, D, P, M)  # the index is different from it in the book
		# S[1,] <- c(currX, currY, D)  # the index is different from it in the book
		S[1,] <- c(currX, currY)
		# A[1] <- greedy(Q[S[1,"currX"], S[1,"currY"], S[1, "D"], ], epsilon)
		A[1] <- greedy(Q[S[1,"currX"], S[1,"currY"], ], epsilon)
		tau <- 0
		t <- 0
		T <- 20000  # infinity. T will be updated to be the lenght of the episode in the repeat loop 
		Q0 <- Q	
		repeat {	# index start from 1, not 0
			t <- t + 1 
			if((t+1)>length(A)){
				A <- c(A, rep(NA, 100))
				# S <- rbind(S, matrix(NA, nrow=100, ncol=7))
				S <- rbind(S, matrix(NA, nrow=100, ncol=2))
				R <- c(R, rep(NA, 100))
			}
			if (t < T){
				# response <- dostep(currX, currY, D, A[t], P, M)
				response <- dostep(currX, currY, A[t])
				currX <- response$newX
				currY <- response$newY
				# D <- response$D
				# M <- response$M
				# S[t+1,] <- c(currX, currY, D, P, M)
				S[t+1,] <- c(currX, currY)
				R[t+1] <- response$reward
				if(R[t+1]==10) {	# if S[t+1] is terminal 
					T <- t + 1
				} else {
					# A[t+1] <- greedy(Q[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"], ], epsilon)
					A[t+1] <- greedy(Q[S[t+1,"currX"], S[t+1,"currY"], ], epsilon)
				}
			}
			tau <- t - n + 1 
			if(tau >= 1) {
				temp <- (tau+1):min(tau+n, T)
				G <- sum((gamma^c(1:length(temp)))*R[temp])
				if ((tau + n) < T) {
					# G <- G + (gamma^n)*Q[S[tau+n,"currX"], S[tau+n,"currY"], S[tau+n, "D"], A[tau+n]]
					G <- G + (gamma^n)*Q[S[tau+n,"currX"], S[tau+n,"currY"], A[tau+n]]
				} 
				# currentQ <- Q[S[tau,"currX"], S[tau,"currY"], S[tau, "D"], A[tau]]
				currentQ <- Q[S[tau,"currX"], S[tau,"currY"], A[tau]]
				# Q[S[tau,"currX"], S[tau,"currY"], S[tau, "D"], A[tau]] <- currentQ + alpha*(G-currentQ)
				Q[S[tau,"currX"], S[tau,"currY"], A[tau]] <- currentQ + alpha*(G-currentQ)
			}
			if ( t== (T -1) | t>5000){
				break 
			}
		}
		totalsteps[i] <- t
		totalrewards[i] <- sum(R,na.rm=TRUE)	

		# stop if optA is the true opt for 10 times
		test <- optA(Q)==optA(Q0)
		test[1,1] <- TRUE	# end point, force it to be true
		is_opt[i%%length(is_opt)+1] <- (sum(test)==nrow(test)*ncol(test))
		if(sum(is_opt,na.rm=TRUE)==length(is_opt)&i>20){
			break
		}	
	}
	return(list(num_episode=i, totalrewards=na.omit(totalrewards)))
	# i
	# sum(totalrewards)
}




#========================================================================================================
treeBackup_n <- function(alpha=0.5, n=4){
	# n-step Tree backup
	# Initialization
	# Q <- array(data=0, dim=c(5,5,2,4)) # X, Y=c(1:5), D=(1=not_damaged, 2=damaged), A=(1:4) for (N,E,S,W) 
	Q <- array(data=0, dim=c(5,5,4)) # X, Y=c(1:5), A=(1:4) for (N,E,S,W) 
	pr_A <- array(data=0.25, dim=c(5,5,4)) # probability of taking action A at a given state
	epsilon <- 0.10 # greedy policy
	# alpha <- 0.5 # step size 
	# n <- 4 # n-step algorithm
	gamma <- 0.9
	totalsteps <- rep(NA,100)
	totalrewards <- rep(NA,100)

	i <- 0
	is_opt <- rep(NA,20)
	repeat {
		i <- i + 1
		if(i>length(totalsteps)){
			totalsteps <- c(totalsteps, rep(NA, 100))
			totalrewards <- c(totalrewards, rep(NA, 100))
		}
		# new episode
		# P <- c(sample(c(1,5),1), sample(c(1,5),1)) # prize location, random
		# S <- matrix(NA, nrow=100, ncol=7) # state trajectory, currX, currY, D, pX, pY, mX, mY
		# colnames(S) <- c("currX", "currY", "D", "pX", "pY", "mX", "mY")
		S <- matrix(NA, nrow=100, ncol=2) # state trajectory, currX, currY
		colnames(S) <- c("currX", "currY")
		A <- rep(NA, 100) # action trajectory
		R <- rep(NA,100) # rewards record
		b_Q <- rep(NA, 100)
		b_Pi <- rep(NA, 100)
		delta <- rep(NA, 100)	
		# always start in the moddle
		currX <- 5
		currY <- 5
		# S[1,] <- c(currX, currY, D, P, M)  # the index is different from it in the book
		# A[1] <- greedy(Q[S[1,"currX"], S[1,"currY"], S[1, "D"], ], epsilon)
		S[1,] <- c(currX, currY)  # the index is different from it in the book
		A[1] <- greedy(Q[S[1,"currX"], S[1,"currY"], ], epsilon)
		b_Q[1] <- Q[S[1,"currX"], S[1,"currY"], A[1]]
		if (b_Q[1]==max(Q[S[1,"currX"], S[1,"currY"], ])){
			Max <- max(Q[S[1,"currX"], S[1,"currY"], ])
			Max <- which(Q[S[1,"currX"], S[1,"currY"], ]==Max)
			b_Pi[1] <- (1- epsilon + epsilon*length(Max)/length(Q[S[1,"currX"], S[1,"currY"],]))/length(Max)
		} else {
			b_Pi[1] <- epsilon/length(Q[S[1,"currX"], S[1,"currY"], ])
		}

		tau <- 0
		t <- 0
		T <- 100000
		Q0 <- Q		
		repeat {	# index start from 1, not 0
			t <- t + 1 
			if((t+1)>length(A)){
				A <- c(A, rep(NA, 100))
				#S <- rbind(S, matrix(NA, nrow=100, ncol=7))
				S <- rbind(S, matrix(NA, nrow=100, ncol=2))
				R <- c(R, rep(NA, 100))
				delta <- c(delta, rep(NA, 100))
				b_Q <- c(b_Q, rep(NA, 100))
				b_Pi <- c(b_Pi, rep(NA, 100))
			}
			if (t < T){
				# response <- dostep(currX, currY, D, A[t], P, M)
				response <- dostep(currX, currY, A[t])
				currX <- response$newX
				currY <- response$newY
				# D <- response$D
				# M <- response$M
				# S[t+1,] <- c(currX, currY, D, P, M) 
				S[t+1,] <- c(currX, currY) 
				R[t+1] <- response$reward
				if(R[t+1]==10) {	# if S[t+1] is terminal 
					T <- t + 1
					# delta[t] <- R[t+1] - Q[S[t,"currX"], S[t,"currY"], S[t, "D"], A[t]]
					delta[t] <- R[t+1] - b_Q[t]
				} else {
					# delta[t] <- R[t+1] + gamma*sum(pr_A[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"],]*Q[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"],]) - Q[S[t,"currX"], S[t,"currY"], S[t, "D"], A[t]]
					delta[t] <- R[t+1] + gamma*sum(pr_A[S[t+1,"currX"], S[t+1,"currY"],]*Q[S[t+1,"currX"], S[t+1,"currY"],]) - b_Q[t]
					# A[t+1] <- greedy(Q[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"], ], epsilon)
					A[t+1] <- greedy(Q[S[t+1,"currX"], S[t+1,"currY"], ], epsilon)
					# Store Q(S(t+1), A(t+1)) as Q(t+1):  Q[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"],A[t+1]] <- 
					b_Q[t+1] <- Q[S[t+1,"currX"], S[t+1,"currY"], A[t+1]]
					# Store policy(A(t+1)|S(t+1)) as policy(t+1)
					b_Pi[t+1] <- pr_A[S[t+1,"currX"], S[t+1,"currY"], A[t+1]]
				}
			}
			tau <- t - n + 1 
			if(tau >= 1) {
				E <- 1
				G <- b_Q[tau]
				for(k in tau:min(tau+n-1, T-1)){
					G <- G + E*delta[k]
					E <- gamma*E*b_Pi[k+1]
				}
				# update Q[S(tau),A(tau)]
				currQ <- Q[S[tau,"currX"], S[tau,"currY"], A[tau]]
				Q[S[tau,"currX"], S[tau,"currY"], A[tau]] <- currQ + alpha*(G - currQ)
				# update pi(S(tau),A(tau))
				Max <- max(Q[S[tau,"currX"], S[tau,"currY"], ])
				Max <- which(Q[S[tau,"currX"], S[tau,"currY"], ]==Max)
				pr_A[S[tau,"currX"], S[tau,"currY"], ] <- epsilon/length(Q[S[tau,"currX"], S[tau,"currY"],])
				pr_A[S[tau,"currX"], S[tau,"currY"], Max] <- (1- epsilon + epsilon*length(Max)/length(Q[S[tau,"currX"], S[tau,"currY"],]))/length(Max)
			}
			if ( t== (T -1) | t>5000){
				break 
			}
		}
		totalsteps[i] <- t
		totalrewards[i] <- sum(R,na.rm=TRUE)	
		
		# stop if optA is the true opt for 10 times
		test <- optA(Q)==optA(Q0)
		test[1,1] <- TRUE	# end point, force it to be true
		is_opt[i%%length(is_opt)+1] <- (sum(test)==nrow(test)*ncol(test))
		if(sum(is_opt,na.rm=TRUE)==length(is_opt)&i>20){
			break
		}	
	}
	return(list(num_episode=i, totalrewards=na.omit(totalrewards)))
	# i
	# sum(totalrewards)

}


#======================================================================================================================

# n-step Expected SARSA
exp_sarsa_n <- function(alpha=0.5, n=4) {
	# Q <- array(data=0, dim=c(5,5,2,4)) # X, Y=c(1:5), D=(1=not_damaged, 2=damaged), A=(1:4) for (N,E,S,W) 
	Q <- array(data=0, dim=c(5,5,4)) # X, Y=c(1:5), A=(1:4) for (N,E,S,W) 
	# pr_A <- array(data=0.25, dim=c(5,5,2,4)) # probability of taking action A at a given state
	pr_A <- array(data=0.25, dim=c(5,5,4)) # probability of taking action A at a given state
	epsilon <- 0.10 # greedy policy
	# alpha <- 0.2 # step size 
	# n <- 4 # n-step algorithm
	gamma <- 0.9
	totalsteps <- rep(NA,100)
	totalrewards <- rep(NA,100)
	i <- 0
	is_opt <- rep(NA,20)
	repeat {
		i <- i + 1
		if(i>length(totalsteps)){
			totalsteps <- c(totalsteps, rep(NA, 100))
			totalrewards <- c(totalrewards, rep(NA, 100))
		}
		# new episode
		# P <- c(sample(c(1,5),1), sample(c(1,5),1)) # prize location, random
		# S <- matrix(NA, nrow=100, ncol=7) # state trajectory, currX, currY, D, pX, pY, mX, mY
		# colnames(S) <- c("currX", "currY", "D", "pX", "pY", "mX", "mY")
		# S <- matrix(NA, nrow=100, ncol=3) # state trajectory, currX, currY, D
		# colnames(S) <- c("currX", "currY", "D")
		S <- matrix(NA, nrow=100, ncol=2) # state trajectory, currX, currY, D
		colnames(S) <- c("currX", "currY")	
		A <- rep(NA, 100) # action trajectory
		R <- rep(NA,100) # rewards record
		# always start in the moddle
		currX <- 5
		currY <- 5
		# D <- 1	# dot_damaged


		# M <- monster()  fixed 
		# S[1,] <- c(currX, currY, D, P, M)  # the index is different from it in the book
		# S[1,] <- c(currX, currY, D)  # the index is different from it in the book
		S[1,] <- c(currX, currY)
		# A[1] <- greedy(Q[S[1,"currX"], S[1,"currY"], S[1, "D"], ], epsilon)
		A[1] <- greedy(Q[S[1,"currX"], S[1,"currY"], ], epsilon)
		tau <- 0
		t <- 0
		T <- 20000  # infinity. T will be updated to be the lenght of the episode in the repeat loop 
		Q0 <- Q	
		repeat {	# index start from 1, not 0
			t <- t + 1 
			if((t+1)>length(A)){
				A <- c(A, rep(NA, 100))
				# S <- rbind(S, matrix(NA, nrow=100, ncol=7))
				S <- rbind(S, matrix(NA, nrow=100, ncol=2))
				R <- c(R, rep(NA, 100))
			}
			if (t < T){
				# response <- dostep(currX, currY, D, A[t], P, M)
				response <- dostep(currX, currY, A[t])
				currX <- response$newX
				currY <- response$newY
				# D <- response$D
				# M <- response$M
				# S[t+1,] <- c(currX, currY, D, P, M)
				S[t+1,] <- c(currX, currY)
				R[t+1] <- response$reward
				if(R[t+1]==10) {	# if S[t+1] is terminal 
					T <- t + 1
				} else {
					# A[t+1] <- greedy(Q[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"], ], epsilon)
					A[t+1] <- greedy(Q[S[t+1,"currX"], S[t+1,"currY"], ], epsilon)
				}
			}
			tau <- t - n + 1 
			if(tau >= 1) {
				temp <- (tau+1):min(tau+n, T)
				G <- sum((gamma^c(1:length(temp)))*R[temp])
				if ((tau + n) < T) {
					# G <- G + (gamma^n)*Q[S[tau+n,"currX"], S[tau+n,"currY"], S[tau+n, "D"], A[tau+n]]
					# get the pi(A(tau+n)|(S(tau+n)))
					Max <- max(Q[S[tau+n,"currX"], S[tau+n,"currY"], ])
					Max <- which(Q[S[tau+n,"currX"], S[tau+n,"currY"], ]==Max)
					pr <- rep(epsilon/length(Q[S[tau+n,"currX"], S[tau+n,"currY"],]),length(Q[S[tau+n,"currX"], S[tau+n,"currY"],]))
					pr[Max] <- (1-epsilon)/length(Max) + epsilon/length(Q[S[tau+n,"currX"], S[tau+n,"currY"],])
					G <- G + (gamma^n)*sum(pr*Q[S[tau+n,"currX"], S[tau+n,"currY"], ])
				} 
				# currentQ <- Q[S[tau,"currX"], S[tau,"currY"], S[tau, "D"], A[tau]]
				currentQ <- Q[S[tau,"currX"], S[tau,"currY"], A[tau]]
				# Q[S[tau,"currX"], S[tau,"currY"], S[tau, "D"], A[tau]] <- currentQ + alpha*(G-currentQ)
				Q[S[tau,"currX"], S[tau,"currY"], A[tau]] <- currentQ + alpha*(G-currentQ)
			}
			if ( t== (T -1) | t>5000){
				break 
			}
		}
		totalsteps[i] <- t
		totalrewards[i] <- sum(R,na.rm=TRUE)	

		# stop if optA is the true opt for 10 times
		test <- optA(Q)==optA(Q0)
		test[1,1] <- TRUE	# end point, force it to be true
		is_opt[i%%length(is_opt)+1] <- (sum(test)==nrow(test)*ncol(test))
		if(sum(is_opt,na.rm=TRUE)==length(is_opt)&i>20){
			break
		}	
	}
	return(list(num_episode=i, totalrewards=na.omit(totalrewards)))
	# i
	# sum(totalrewards)
}


# =================================================================================================

# n-step Q(sigma)
Q_sigma <- function(alpha=0.5, n=4){
	# Initialization
	# Q <- array(data=0, dim=c(5,5,2,4)) # X, Y=c(1:5), D=(1=not_damaged, 2=damaged), A=(1:4) for (N,E,S,W) 
	Q <- array(data=0, dim=c(5,5,4)) # X, Y=c(1:5), A=(1:4) for (N,E,S,W) 
	pr_A <- array(data=0.25, dim=c(5,5,4)) # probability of taking action A at a given state
	epsilon <- 0.10 # greedy policy
	# alpha <- 0.5 # step size 
	# n <- 4 # n-step algorithm
	gamma <- 0.9
	totalsteps <- rep(NA,100)
	totalrewards <- rep(NA,100)

	i <- 0
	is_opt <- rep(NA,20)
	repeat {
		i <- i + 1
		if(i>length(totalsteps)){
			totalsteps <- c(totalsteps, rep(NA, 100))
			totalrewards <- c(totalrewards, rep(NA, 100))
		}
		# new episode
		# P <- c(sample(c(1,5),1), sample(c(1,5),1)) # prize location, random
		# S <- matrix(NA, nrow=100, ncol=7) # state trajectory, currX, currY, D, pX, pY, mX, mY
		# colnames(S) <- c("currX", "currY", "D", "pX", "pY", "mX", "mY")
		S <- matrix(NA, nrow=100, ncol=2) # state trajectory, currX, currY
		colnames(S) <- c("currX", "currY")
		A <- rep(NA, 100) # action trajectory
		R <- rep(NA,100) # rewards record
		b_Q <- rep(NA, 100)
		b_Pi <- rep(NA, 100)
		delta <- rep(NA, 100)	
		sigma <- rep(NA, 100)
		# always start in the moddle
		currX <- 5
		currY <- 5
		# S[1,] <- c(currX, currY, D, P, M)  # the index is different from it in the book
		# A[1] <- greedy(Q[S[1,"currX"], S[1,"currY"], S[1, "D"], ], epsilon)
		S[1,] <- c(currX, currY)  # the index is different from it in the book
		A[1] <- greedy(Q[S[1,"currX"], S[1,"currY"], ], epsilon)
		b_Q[1] <- Q[S[1,"currX"], S[1,"currY"], A[1]]
		if (b_Q[1]==max(Q[S[1,"currX"], S[1,"currY"], ])){
			Max <- max(Q[S[1,"currX"], S[1,"currY"], ])
			Max <- which(Q[S[1,"currX"], S[1,"currY"], ]==Max)
			b_Pi[1] <- (1- epsilon + epsilon*length(Max)/length(Q[S[1,"currX"], S[1,"currY"],]))/length(Max)
		} else {
			b_Pi[1] <- epsilon/length(Q[S[1,"currX"], S[1,"currY"], ])
		}

		tau <- 0
		t <- 0
		T <- 100000
		Q0 <- Q		
		repeat {	# index start from 1, not 0
			t <- t + 1 
			if((t+1)>length(A)){
				A <- c(A, rep(NA, 100))
				#S <- rbind(S, matrix(NA, nrow=100, ncol=7))
				S <- rbind(S, matrix(NA, nrow=100, ncol=2))
				R <- c(R, rep(NA, 100))
				delta <- c(delta, rep(NA, 100))
				b_Q <- c(b_Q, rep(NA, 100))
				b_Pi <- c(b_Pi, rep(NA, 100))
				sigma <- c(sigma, rep(NA, 100))
			}
			if (t < T){
				# response <- dostep(currX, currY, D, A[t], P, M)
				response <- dostep(currX, currY, A[t])
				currX <- response$newX
				currY <- response$newY
				# D <- response$D
				# M <- response$M
				# S[t+1,] <- c(currX, currY, D, P, M) 
				S[t+1,] <- c(currX, currY) 
				R[t+1] <- response$reward
				if(R[t+1]==10) {	# if S[t+1] is terminal 
					T <- t + 1
					# delta[t] <- R[t+1] - Q[S[t,"currX"], S[t,"currY"], S[t, "D"], A[t]]
					delta[t] <- R[t+1] - b_Q[t]
				} else {
					# A[t+1] <- greedy(Q[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"], ], epsilon)
					A[t+1] <- greedy(Q[S[t+1,"currX"], S[t+1,"currY"], ], epsilon)
					sigma[t+1] <- sample(c(0,1),1)
					# Store Q(S(t+1), A(t+1)) as Q(t+1):  Q[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"],A[t+1]] <- 
					b_Q[t+1] <- Q[S[t+1,"currX"], S[t+1,"currY"], A[t+1]]				
					# delta[t] <- R[t+1] + gamma*sum(pr_A[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"],]*Q[S[t+1,"currX"], S[t+1,"currY"], S[t+1, "D"],]) - Q[S[t,"currX"], S[t,"currY"], S[t, "D"], A[t]]
					delta[t] <- R[t+1] + gamma*sigma[t+1]*b_Q[t+1] + gamma*(1-sigma[t+1])*sum(pr_A[S[t+1,"currX"], S[t+1,"currY"],]*Q[S[t+1,"currX"], S[t+1,"currY"],]) - b_Q[t]

					# Store policy(A(t+1)|S(t+1)) as policy(t+1)
					b_Pi[t+1] <- pr_A[S[t+1,"currX"], S[t+1,"currY"], A[t+1]]
				}
			}
			tau <- t - n + 1 
			if(tau >= 1) {
				E <- 1
				G <- b_Q[tau]

				for(k in tau:min(tau+n-1, T-1)){
					G <- G + E*delta[k]
					E <- gamma*E*((1-sigma[k+1])*b_Pi[k+1]+sigma[k+1])
				}

				# update Q[S(tau),A(tau)]
				currQ <- Q[S[tau,"currX"], S[tau,"currY"], A[tau]]
				Q[S[tau,"currX"], S[tau,"currY"], A[tau]] <- currQ + alpha*(G - currQ)
				# update pi(S(tau),A(tau))
				Max <- max(Q[S[tau,"currX"], S[tau,"currY"], ])
				Max <- which(Q[S[tau,"currX"], S[tau,"currY"], ]==Max)
				pr_A[S[tau,"currX"], S[tau,"currY"], ] <- epsilon/length(Q[S[tau,"currX"], S[tau,"currY"],])
				pr_A[S[tau,"currX"], S[tau,"currY"], Max] <- (1- epsilon + epsilon*length(Max)/length(Q[S[tau,"currX"], S[tau,"currY"],]))/length(Max)
			}
			if ( t== (T -1) | t> 500){
				break 
			}
		}
		totalsteps[i] <- t
		totalrewards[i] <- sum(R,na.rm=TRUE)	
		
		# stop if optA is the true opt for 10 times
		test <- optA(Q)==optA(Q0)
		test[1,1] <- TRUE	# end point, force it to be true
		is_opt[i%%length(is_opt)+1] <- (sum(test)==nrow(test)*ncol(test))
		if(sum(is_opt,na.rm=TRUE)==length(is_opt)&i>20){
			break
		}	
	}
	return(list(num_episode=i, totalrewards=na.omit(totalrewards)))
	# i
	# sum(totalrewards)
}


num_episode <- matrix(NA, nrow=50, 5)
totalrewards <- array(NA, dim=c(50, 5, 500))
n=8
alpha=0.25
for (i in 1:50) {
	cat("i=",i, "\n")
	temp <- sarsa_0(alpha)
	num_episode[i, 1] <- temp$num_episode
	totalrewards[i, 1, 1:min(length(temp$totalrewards),dim(totalrewards)[3])] <- temp$totalrewards[1:min(length(temp$totalrewards),dim(totalrewards)[3])]
	temp <- sarsa_n(alpha, n)
	num_episode[i, 2] <- temp$num_episode
	totalrewards[i, 2, 1:min(length(temp$totalrewards),dim(totalrewards)[3])] <- temp$totalrewards[1:min(length(temp$totalrewards),dim(totalrewards)[3])]	
	temp <- exp_sarsa_n(alpha, n)
	num_episode[i, 3] <- temp$num_episode
	totalrewards[i, 3, 1:min(length(temp$totalrewards),dim(totalrewards)[3])] <- temp$totalrewards[1:min(length(temp$totalrewards),dim(totalrewards)[3])]	
	temp <- treeBackup_n(alpha, n)
	num_episode[i, 4] <- temp$num_episode
	totalrewards[i, 4, 1:min(length(temp$totalrewards),dim(totalrewards)[3])] <- temp$totalrewards[1:min(length(temp$totalrewards),dim(totalrewards)[3])]	
	temp <- Q_sigma(alpha, n)
	num_episode[i, 5] <- temp$num_episode
	totalrewards[i, 5, 1:min(length(temp$totalrewards),dim(totalrewards)[3])] <- temp$totalrewards[1:min(length(temp$totalrewards),dim(totalrewards)[3])]	
}



saveRDS(num_episode, "num_episode_n8_a0.25.rds")
saveRDS(totalrewards, "totalrewards_n8_a0.25.rds")

num_episodeC <- NULL
totalrewardsC <- NULL
N <- NULL
a <- NULL
method <- NULL
num_episode_n2_a0.25 <- readRDS("num_episode_n2_a0.25.rds")
totalrewards_n2_a0.25 <- readRDS("totalrewards_n2_a0.25.rds")
num_episodeC <- c(num_episodeC, colMeans(num_episode_n2_a0.25))
totalrewardsC <- cbind(totalrewardsC, apply(totalrewards_n2_a0.25, c(2,3), mean, na.rm=TRUE))

num_episode_n4_a0.25 <- readRDS("num_episode_n4_a0.25.rds")
totalrewards_n4_a0.25 <- readRDS("totalrewards_n4_a0.25.rds")
num_episodeC <- c(num_episodeC, colMeans(num_episode_n4_a0.25))
totalrewardsC <- cbind(totalrewardsC, apply(totalrewards_n4_a0.25, c(2,3), mean, na.rm=TRUE))

num_episode_n6_a0.25 <- readRDS("num_episode_n6_a0.25.rds")
totalrewards_n6_a0.25 <- readRDS("totalrewards_n6_a0.25.rds")
num_episodeC <- c(num_episodeC, colMeans(num_episode_n6_a0.25))
totalrewardsC <- cbind(totalrewardsC, apply(totalrewards_n6_a0.25, c(2,3), mean, na.rm=TRUE))

num_episode_n8_a0.25 <- readRDS("num_episode_n8_a0.25.rds")
totalrewards_n8_a0.25 <- readRDS("totalrewards_n8_a0.25.rds")
num_episodeC <- c(num_episodeC, colMeans(num_episode_n8_a0.25))
totalrewardsC <- cbind(totalrewardsC, apply(totalrewards_n8_a0.25, c(2,3), mean, na.rm=TRUE))


num_episode_n2_a0.5 <- readRDS("num_episode_n2_a0.5.rds")
totalrewards_n2_a0.5 <- readRDS("totalrewards_n2_a0.5.rds")
num_episodeC <- c(num_episodeC, colMeans(num_episode_n2_a0.5))
totalrewardsC <- cbind(totalrewardsC, apply(totalrewards_n2_a0.5, c(2,3), mean, na.rm=TRUE))

num_episode_n4_a0.5 <- readRDS("num_episode_n4_a0.5.rds")
totalrewards_n4_a0.5 <- readRDS("totalrewards_n4_a0.5.rds")
num_episodeC <- c(num_episodeC, colMeans(num_episode_n4_a0.5))
totalrewardsC <- cbind(totalrewardsC, apply(totalrewards_n4_a0.5, c(2,3), mean, na.rm=TRUE))

num_episode_n6_a0.5 <- readRDS("num_episode_n6_a0.5.rds")
totalrewards_n6_a0.5 <- readRDS("totalrewards_n6_a0.5.rds")
num_episodeC <- c(num_episodeC, colMeans(num_episode_n6_a0.5))
totalrewardsC <- cbind(totalrewardsC, apply(totalrewards_n6_a0.5, c(2,3), mean, na.rm=TRUE))

num_episode_n8_a0.5 <- readRDS("num_episode_n8_a0.5.rds")
totalrewards_n8_a0.5 <- readRDS("totalrewards_n8_a0.5.rds")
num_episodeC <- c(num_episodeC, colMeans(num_episode_n8_a0.5))
totalrewardsC <- cbind(totalrewardsC, apply(totalrewards_n8_a0.5, c(2,3), mean, na.rm=TRUE))


N <- rep(rep(c(2,4,6,8),each=5),2)
a <- rep(c(0.25，5),each=5*4)
method <- rep(c("sarsa0", "sarsan", "exp_sarsan", "treeBackupn", "Q_sigma"), 8)
df1 <- data.frame(N=N, a=a, method=method, num_episode=num_episodeC)


N <- rep(rep(rep(c(2,4,6,8),each=500),2),5)
a <- rep(rep(c(0.25，5),each=500*4),5)
method <- rep(c("sarsa0", "sarsan", "exp_sarsan", "treeBackupn", "Q_sigma"), 8*500)
totalrewards1 <- as.vector(t(totalrewardsC))
df2 <- data.frame(N=N, a=a, method=method, totalrewards1=totalrewards1)
