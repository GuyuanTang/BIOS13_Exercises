#Advanced level
f = function(x,y) x*sin(4*x)+1.1*y*sin(2*y)
x = seq(0,10, length=101)
y = seq(0,10, length=101)
z = outer(x,y,f)
persp(x,y,z, theta = 30, phi = 30, expand = 0.6, ticktype = 'detailed')

#Expert level

# Main variables:
pop_size <- 50            # popsize is number of chromosomes in the population
gene_number <- 2         # gene number is number of genes in each chromosome, should be one in these simple tasks
max_generations <- 1000    # number of iterations
num_breeed <- 20          # out of the 50 chromosomes (= possible solutions) the 20 best are allowed to reproduce

# genetic operators
mutation_rate <- 0.01    #how large percentage of the genes experience mutations
mutation_free <- 10      # number of top ranked individuals that should be saved from mutation


# Define Functions

Create_pop <- function(pop_size,gene_number) {    # This function creates a random population of pop_size chromosomes
  
  Genes <- matrix(nrow=pop_size, ncol=gene_number)
  for(ind in 1:pop_size) {              # For each individual:
    for(gene_index in 1:gene_number) {      # For each gene in the chromosome
      Genes[ind,gene_index] <- runif(1,max = 10,min = 0)
      }
    }
  fitness <- rep(NA,pop_size)
  # pop is a list, a structured variable with information about the population:
  pop <- list(size=pop_size, L=gene_number, Genes=Genes, fitness=fitness)
  return(pop) 
}

# Function to calculate fitness for each individual:
Evaluate_fitness <- function(pop) {
  f <- function(x,y) x*sin(4*x)+1.1*y*sin(2*y)
  for(ind in 1:pop$size) {
    pop$fitness[ind] <- f(pop$Genes[ind,1], pop$Genes[ind,2])
  }
  return(pop)
}


# Sort the population by fitness:
Sort <- function(pop) {
  # Find order of decreasing fitness:
  new_order <- order(pop$fitness, decreasing=FALSE)
  # Next, rearrange the population gene matrix and fitness vector:
  pop$Genes <- pop$Genes[new_order,]
  pop$fitness <- pop$fitness[new_order]
  return(pop)
}


# Let individuals with the highest fitness reproduce:
Reproduce <- function(pop,num_breeed) {
  # The population reproduces in pairs, in fitness order
  # First indidivudals 1 and 2 mate, next individuals 3 and 4, and so on
  # Offspring replace the parent population from the bottom of the list.
  offspring_pos <- pop$size # where to put the offspring, starting from bottom
  for(parent1 in seq(1, num_breeed, by=2)) {
    parent2 <- parent1 + 1
    # make two offspring per pair
    # for each offspring, I choose a different blending rule 
    for (offspring_index in 1:2) {
      blending <- sample(c(1,2,3), 1)
      if(blending==1) {
        # Blend the whole chromosome at once:
        offspring_genes <- 0.5*pop$Genes[parent1,] + 0.5*pop$Genes[parent2,];
      } else if(blending==2) {
        offspring_genes <- 1.5*pop$Genes[parent1,] - 0.5*pop$Genes[parent2,];
      } else {
        offspring_genes <- 1.5*pop$Genes[parent2,] - 0.5*pop$Genes[parent1,];
      }
      # Check that we are within bounds:
      if( offspring_genes[1] < 0) offspring_genes[1] <- 0
      if( offspring_genes[1] > 10) offspring_genes[1] <- 10
      if( offspring_genes[2] < 0) offspring_genes[2] <- 0
      if( offspring_genes[2] > 10) offspring_genes[2] <- 10
      
      pop$Genes[offspring_pos,] <- offspring_genes
      offspring_pos <- offspring_pos - 1
    }
  }
  return(pop)
}

# Mutate the whole population, accept the top fitness individuals
Mutate <- function(pop, mutation_free, mutation_rate) {
  for(ind in (mutation_free+1):pop$size) {
    for(gene_index in 1 : pop$L) {
      if(runif(1) <= mutation_rate) {
          pop$Genes[ind,gene_index] <- runif(1, 0, 10)
        }
      }
    }
  return(pop)
}

##############################
# Main program starts here

pop <- Create_pop(pop_size,gene_number)  # calls function that generates random chromosomes
pop <- Evaluate_fitness(pop)   # calculate fitness for all individuals
pop <- Sort(pop) # Sort according to fitness

# main loop over all of our functions

meantopfitness <- c(0,max_generations) # empty vectors to store results
meanfitness <- c(0,max_generations)

for(generation in 1:max_generations) {  
  pop <- Reproduce(pop,num_breeed)
  pop <- Mutate(pop, mutation_free, mutation_rate)
  pop <- Evaluate_fitness(pop)   # give back altered population with fitness
  pop <- Sort(pop)
  
  # save statistics:
  meantopfitness[generation] <- mean(pop$fitness[1:5])
  meanfitness[generation] <- mean(pop$fitness)
}

cat("generation:",max_generations,"\n")
cat("mean top fitness:",meantopfitness[max_generations],"\n")
cat("mean fitness",meanfitness[max_generations],"\n")

# plot results
plot(1:max_generations, meanfitness, ylim=c(min(meantopfitness),max(meantopfitness)),type="l")
lines( 1:max_generations, meantopfitness, col="red")

