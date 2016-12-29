#!/usr/bin/r

library(inline)

#sequential implementation
playSeq <- function(t = 20, ncols = 10,   nrows = 10) 
{
  rowM <- ncols + 2
  colM <- ncols + 2
  initGrid <- matrix(rbinom(rowM*colM, 1, 0.5), nrow=rowM, ncol=colM)
  nextGrid <- matrix(0L, rowM,colM)
  
  #setting borders to 0
  initGrid[1,] <- 0
  initGrid[,1] <- 0
  initGrid[rowM,] <- 0
  initGrid[,colM] <- 0
  
  for(t in seq_len(t)){
    #print(initGrid)
    for(j in 2:(colM-1))
    {
      for(i in 2:(rowM-1))
      {
        nextGrid[i,j] <- getNextState(initGrid, i, j)
      }
    }
    initGrid <- nextGrid
  }
}

#function to get next state
getNextState <- function(initGrid, i,j) {
  numberOfNei <- sum(initGrid[(i-1):(i+1),(j-1):(j+1)]) - initGrid[i,j]
  if (numberOfNei == 3 || initGrid[i,j] == 1 && numberOfNei == 2)
    1
  else
    0
}

##############################################################
# Implementation with C++ OpenMP

#header
inc <- '
#include <iostream>
using namespace std;
#include <omp.h>
'

#function body
gameOfLifePar <- '
  Rcpp::IntegerMatrix Am(A);
  Rcpp::IntegerMatrix tempM(temp);
  int nt = as<int>(n);

  int nrows = Am.nrow();
  int ncolumns = Am.ncol();

  int nthreads = as<int>(nth);
  int i,j,nei,t;
  for (t = 0; t < nt; t++) {
  //Rcpp::Rcout << Am << "\\n";
  
    #pragma omp parallel for private(i,j, nei) shared(Am,tempM) num_threads(nthreads)
    for (j = 1; j < nrows-1; j++) {
      //cout<<"threads="<<omp_get_num_threads()<<endl;
      for (i = 1; i < ncolumns-1; i++) {
        nei = Am(j-1,i-1) + Am(j-1,i) + Am(j-1,i+1) + Am(j,i-1) + Am(j,i+1) + Am(j+1,i-1) + Am(j+1,i) + Am(j+1,i+1);
        if (nei == 3 || Am(j,i) == 1 && nei == 2) {
          tempM(j,i) = 1;
        } else {
          tempM(j,i)= 0;
        }
      }
    }
    Am = tempM;
  }
  return Am;
'

# settings of compilation
settings <- getPlugin("Rcpp")
settings$env$PKG_CXXFLAGS <- paste('-fopenmp', settings$env$PKG_CXXFLAGS)
settings$env$PKG_LIBS <- paste('-fopenmp -lgomp', settings$env$PKG_LIBS)
gameOfLifeMore <- cxxfunction(signature(A="integer", temp="integer", n="integer", nth="integer"),inc=inc, body=gameOfLifePar, plugin="Rcpp", settings=settings)

# main function
playGOLParallel <- function(nrows=100, ncols=100, t=50, nthreads=1) {
  rowM <- nrows+2
  rowC <- ncols+2
  grid <- matrix(rbinom(rowM*rowC, 1, 0.5), rowM, rowC)
  grid[1,] <- 0
  grid[,1] <- 0
  grid[rowM,] <- 0
  grid[,rowC] <- 0
  nextGrid <- grid
  # using R function to call C function 
  m<- gameOfLifePar(grid, nextGrid,t, nthreads)
}

  