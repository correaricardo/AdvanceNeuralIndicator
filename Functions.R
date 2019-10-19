# Return a matrix with Yd and p delays. Also normalize the data
getDataBase <- function(Data, p){
  if (p<1){
    stop("It is necessary more than one delay. Check the 'p' value")
  }
  numberRow = length(Data[,1]) - p; # The new number of rows
  numberCol = p + 1; # The new number of columns
  maxOpen = max(Data[,2])
  maxClose = max(Data[,1])
  newData = matrix(0, numberRow, numberCol)
  # Auxiliar variables
  aux = 1
  index = p + aux
  newData[,1] = Data[index:length(Data[,1]),2] / maxOpen
  # Close k-n
  for (i in 2:(p+1)){
    lowLimit = index - aux;
    upLimit = length(Data[,1])-aux
    newData[,i] = Data[lowLimit:upLimit,1] / maxClose
    aux = aux + 1
  }
  return(newData)
}

# Return the best combination of coefficients for the AR model based on a genetic algorithm
getARCoefficients <- function(Data, p) {
  # Necessary values
  n = 10 # Number of individuals
  N = 10 # Max number of populations to generate
  i = 1 # Iterator
  pi = 1 # Population indicator
  P = matrix(-Inf, n, (p+2)) # Local population
  cp = 0.3 # Crossover probability
  mp = 0.2 # Mutation probability
  # Generate the initial population with n individuals
  P0 = runif(((p+2)*n), -1, 1)
  P0 = matrix(P0, n, (p+2)) # Close k-1, ..., Close k-p, ek2, Aptitude function
  # Get ek2 and Aptitude function
  P0 = evaluatePopulation(P0, Data)
  # Find the best individual
  best = findBest(P0)
  # Get the roulette probability (For selection operation)
  R = roulette(P0)
  while (i<=N) {
    rand = runif(1, 0, 1)
    if (rand < cp){ # If we can cross individuals
      i1 = selectIndividual(R, P0)
      i2 = selectIndividual(R, P0)
      newInd = cross(i1, i2)
      P = addIndividual(P, newInd, pi, n)
      if (pi<=length(P[,1]) && P[pi, length(P[1,])]!=-Inf){
        pi = pi + 1
      }
    }
    rand = runif(1, 0, 1)
    if (rand < mp){ # If we can mutate an individual
      genPos = sample(1:(length(P0[1,])-2), 1) # The gen to will be mutated
      ind = sample(1:length(P0[,1]), 1) 
      ind = P0[ind,] # The individual to mutate
      newInd = mutate(ind, genPos, Data)
      P = addIndividual(P, newInd, pi, n)
      if (pi<=length(P[,1]) && P[pi, length(P[1,])]!=-Inf){
        pi = pi + 1
      }
    }
    if (pi == n+1){ # When the local population has the same number of individuals
      i = i + 1
      pi = 1 # Reset the population indicator
      P0 = P # Pass the new population
      P = matrix(0, n, (p+2)) # Reset the Local population
      P0 = evaluatePopulation(P0, Data) # Evaluate the new individuals
      bestLocal = findBest(P0)
      if (bestLocal[length(bestLocal)] > best[length(best)]){
        best = bestLocal
      }
      R = roulette(P0) # Update the roulette
    }
  }
  return(best)
}
# -- AUXILIARY METHODS FOR THE GA -- 
# Evaluate the individuals of the population
evaluatePopulation <- function(P, Data){
  for (i in 1:length(P[,1])){
    sumProduct = Data[,2:length(Data[1,])] %*% t(t(P[i,1:(length(P[1,])-2)]))
    ek = (sumProduct - Data[,1])^2
    # Calculate ek2
    P[i,(length(P[1,])-1)] = mean(ek)
    # Calculate the aptitude function
    P[i,length(P[1,])] = 1 / P[i,(length(P[1,])-1)]
  }
  return(P)
}
# Evaluate an individual from the population and return its AF
evaluateIndividual <- function(i, Data){
  sumProduct = Data[,2:length(Data[1,])] %*% t(t(i[1:(length(i)-2)]))
  ek = (sumProduct - Data[,1])^2
  # Calculate ek2
  i[length(i)-1] = mean(ek)
  # Calculate the aptitude function
  i[length(i)] = 1 / i[length(i)-1]
  return(i[length(i)])
}
# Find the best individual in the population
findBest <- function(P){
  maxAF = max(P[,length(P[1,])]) # The max aptitude function
  i = 1
  while(i<=length(P[,1])){
    if (P[i,length(P[1,])] == maxAF){
      return(t(P[i,]))
    }else{
      i = i + 1
    }
  }
}
# Generate the roulette of probabilities for every individual
roulette <- function(P){
  r = matrix(0, length(P[,1]), 2)
  sumAF = sum(P[,length(P[1,])]) # Sum all the aptitude functions
  prob = P[,length(P[1,])] / sumAF
  # Set the first range
  r[1,1] = 0
  r[1,2] = prob[1]
  # Set the ranges missing
  for (i in 2:length(prob)){
    r[i,1] = r[i-1,2]
    r[i,2] = r[i,1] + prob[i]
  }
  return(r)
}
# Select an individual from the popularion
selectIndividual <- function(r, p){
  rand = runif(1, 0, 1)
  i = 1
  while(i<=length(p[,1])){
    if (r[i,1]<rand && rand<=r[i,2]){
      return(p[i,])
    }else{
      i = i + 1
    }
  }
  return(-1)
}
# Crossover between two individuals
cross <- function(i1, i2){
  newI = vector(length = length(i1))
  rand = runif(1, 0, 1)
  for (i in 1:length(newI)){
    newI[i] = rand*i1[i]+(1-rand)*i2[i]
  }
  return(newI)
}
# Mutate one gen to the one individual
mutate <- function(i, genPos, Data){
  dx = runif(1, -1, 1) / 100
  prevAF = i[length(i)]
  newI = i
  newI[genPos] = newI[genPos] + dx
  actualAF = evaluateIndividual(newI, Data)
  while (actualAF > prevAF) {
    prevAF = actualAF
    newI[length(newI)] = actualAF
    i = newI
    newI[genPos] = newI[genPos] + dx
    actualAF = evaluateIndividual(newI, Data)
  }
  return(i)
}
# Verify if an individual can be added to the population and add it if it is true
addIndividual <- function(p, i, pi, n){
  if (pi > n){
    return(p)
  }else{
    p[pi,] = i
    return(p)
  }
}
# ----------------------------------

# This method runs a MADALINE model and return the Wij and Cj values
runMadaline <- function(X, yd, NE, NO, ND, alfa, NIT){
  # Define necessary variables
  yr = matrix(0, ND, 1)
  ek = matrix(0, ND, 1)
  ys = matrix(0, ND, 1)
  W = rnorm(NO*NE, mean = 0, sd =  1) # Vector
  C = rnorm(NO, mean = 0, sd =  1) # Vector
  W = matrix(W, NO, NE) # Matriz
  C = matrix(C, NO, 1) # Matriz
  h = matrix(0, NO, 1)
  # Scroll through the equations
  for (i2 in 1:NIT){
    for (k in 1:ND){
      # Feedforward
      h = W %*% X[k,]
      ys[k,] = t(h) %*% C
      # Use the needed activation function
      yr[k,] = activationFunction(ys[k,])
      der = derFunction(ys[k,]) # Derivative of the activation function
      # Backpropagation
      ek[k,] = yd[k,] - yr[k,]
      C = C + alfa * ek[k,] * der * h
      W = W + ek[k,] * alfa * der * C %*% X[k,]
    }
  }
  # Correlation and Ek2
  c = cor(yd, yr)
  cat("Correlation of the Madaline model: ", c, "\n")
  e = calculateEk2(yd, yr, ND)
  cat("Ek2 od the Madaline model: ", e)
  model = matrix(0, length(W[,1]),length(W[1,])+2)
  model[,1:length(W[1,])] = W
  model[,length(model[1,])-1] = C
  model[,length(model[1,])] = alfa
  return(model)
}
# -- AUXILIARY METHODS FOR THE MADALINE MODEL -- 
activationFunction <-function(ys){
  # return(1/(1+exp(-ys)))
  return(ys)
}
derFunction <- function(ys){
  # return(exp(-ys)/(1+exp(-ys))^2)
  return(1)
}
calculateEk2 <- function(yd, yr, ND){
  ek2 = 0
  for (k in 1:ND){
    ek2 = (yd[k,] - yr[k,])^2
  }
  ek2 = ek2 / ND
  return(ek2)
}