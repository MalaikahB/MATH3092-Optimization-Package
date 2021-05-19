## ---- load
library('plotly')
library('ggplot2')
library("shiny")
library("numDeriv")



## ---- GSS -----

gss <- function(f, a, b, it = 10, tol = 1e-05 ){

#' Golden-section search
#' @param f A function that takes one argument x that is to be optimized.
#' The function must be strictly unimodal on the interval [a, b], if there is a unique value in [a,b] such that  is the minimum of f is on [a,b]
#' @param a The lower bound of the function, in which the optimal value is larger than. This value must also be real and scalar.
#' @param b The upper bound of the function, in which the optimal value is smaller than. This value must also be real and scalar.
#' @param it Number of iterations that the algorithm should run. Default is set at 10.
#' @param tol A tolerance level of which is used to show that the function is convergingtowards the optimal solution. Set at 0.00001
#'
#' @return
#' \code{gss} returns an object of \code{class}.
#'
#' The functions \code{summary}, \code{print} and \code{plot} are used to obtain a summary table of results,
#' returning the final result and plot correspondingly.
#'
#' The object of class \code{"golden"} is a list containing the following components:
#'
#' \code{a}   the lower bound of the initial lower bound inputted
#'
#' \code{b}   the upper bound of the initial upper bound inputted
#'
#' \code{func}    the function that is to be optimized
#'
#' \code{c}   the final lower bound found by the algorithm
#'
#' \code{d}   the final upper bound found by the algorithm
#'
#' \code{upperinterval}   upper bound found after each iteration
#'
#' \code{lowerinterval}   lower bound found after each iteration
#'
#' \code{optimal_x}   the optimal point found after each iteration
#'
#' \code{iteration}   the number of iterations run by the algorithm
#'
#' @export
#' @examples
#' f <- function(x){return((x-2)**2)}
#' > gss(f, 1, 5)
#'  Interation number 10
#'  The subset interval 1.989217 1.996894
#'  The estimated minimizer 1.993056


  #calculate the golden ratio
  golratio <- (sqrt(5) - 1)/2

  #set initial points using golden ratio
  c <- b - golratio * (b - a)
  d <- a + golratio * (b - a)

  #evaluate the fn at the test points
  f_c = f(c)
  f_d = f(d)
  iteration = 0

  min <- rep(0, length(it))
  upper <- rep(0, length(it))
  lower <- rep(0, length(it))

  oglower <- a
  ogupper <- b
  func <- f

  while (abs(b - a) > tol && iteration < it){
    iteration = iteration + 1

    if (f_d > f_c)
      # min is then smaller than d
      # let d be the new upper bound
      # let c be the upper test point
      {
      # set d as new upper bound
      b = d

      # set new upper test point
      d = c
      f_d = f_c

      #set new lower test point
      c <- b - golratio * (b - a)
      f_c = f(c)
    }
    else
      # min is then smaller than c
      # let c be the new lower bound
      # let d be the lower test point
      {
        # set c as new lower bound
        a = c
        #set new lower test point
        c = d
        f_c = f_d
        # set new upper test point
        d <- a + golratio * (b - a)
        f_d = f(d)
      }

    #create classes for the upper interval,
    #lower interval and minimum of each iteration of the algorithm

    upper[iteration] =  d
    lower[iteration] = c
    min[iteration] = (c(c+d)/2)

    golden = list(a = oglower , b = ogupper, func = func, c = c, d = d, upperinterval = upper,
                  lowerinterval = lower, optimal_x = min, iteration = iteration)
    class(golden) = c("golden", class(golden))
  }
   golden
}

## ---- GoldenPlot
plot.golden <- function(x, ...) {

  #create a loop for the plot of f
  xi <- seq(x$a, x$b, by = ((x$b-x$a)/100))
  fx <- rep(0, length(xi))
  for (i in 1:length(xi)) {
    fx[i] = x$func(xi[i])
  }
  xy <- setNames(
    data.frame(xi, fx),
    c("x", "y")
    )
  min <- (x$c + x$d)/ 2



  #create the plot for function f
 # for(i in 1:length(x$minimum)){
    fig <- plot_ly(xy, x= ~x, y = ~y, mode = 'lines', type = 'scatter', name = "Function")
    fig <- fig %>% add_trace(x = min, y = x$func(min), mode = 'marker', name = "Minimum point")
    fig

     #plot( x= xi, y = fx, type = 'l', xlab = 'x', ylab = 'f(x)', main = "Golden Section Search Algorithm") +
    #points(x = (x$lowerinterval[i]), y = f(x$lowerinterval[i]), pch = '|', col = 'green' )
    #points(x = (x$upperinterval[i]), y = f(x$upperinterval[i]), pch = '|', col = 'green')
    #points(x = (x$minimum[i]), y = x$fun((x$minimum[i])), pch = 18, col = 'blue')
  #  readline('Press ENTER to continue...')

  # }



}

## ---- GoldenSummary
summary.golden <- function(x,...){

  result_it <- setNames(
    data.frame(x$lowerinterval, x$upperinterval, x$optimal_x),
    c("","Subset Interval", "Minimum" )
  )

  cat("Results after each iteration", '\n')
  print(result_it)
}

## ---- GoldenPrint
print.golden <- function(x,...){

  cat(
    'Iteration number', x$iteration, '\n',
    'The subset interval', c(x$c, x$d), '\n',
    'The estimated minimizer', (x$c + x$d)/ 2, '\n')
}

## ---- GSSfun
test <- function(x){
  return(x**4 - 14*x**3 + 60*x**2 - 70*x)
}

## ---- GSSimplement
goldtest <- gss(test, 0, 2)


## ---- GSSprint
goldtest <- gss(test, 0, 2)
print(goldtest)

## ---- GSSsum
summary(goldtest)

## ---- GSSplot
plot(goldtest)


## ---- Brents
brents <- function(f, a, b, iteration = 20, tol = 1e-05){

#' Brent's Method
#' @param f A function that takes one argument x that is to be optimized
#' Note that the function has to be continuous over the interval stated and must have a Lagrange interpolating polynomial of 2 or more
#' @param a Known as the contrapoint such that f(\code{a}) and f(\code{b}) need to contain different signs to each other and the interval contains the solution.
#' @param b The current iterate of the algorithm that is the current guess for the root of the function.
#' @param iteration Number of iterations that the algorithm should run. Default is set at 20.
#' @param tol A tolerance level of which is used to show that the function is convergingtowards the optimal solution. Default is set at 1e-05
#' @export
#'
#' @return
#' \code{brents} returns an object of \code{class}.
#'
#' The functions \code{summary}, \code{print} and \code{plot} are used to obtain a summary table of results,
#' returning the final result and plot correspondingly.
#'
#' The object of class \code{"brents"} is a list containing the following components:
#'
#' \code{lower}   initial lower bound inputted
#'
#' \code{upper}   initial upper bound inputted
#'
#' \code{root_x}    root of function after each iteration
#'
#' \code{func}    function needed to be optimized
#'
#' \code{final_root}    final root of the function found by Brent's method
#'
#' \code{iterations}    Number of iterations ran by the algorithm
#'
#' @examples
#'
#' g <- function(x){
#'  return((x**2)-20)
#'  }
#'
#'
#' > brents(g, 2, 5, 16)
#'  Number of iterations 16
#'  Root 4.472153
#'
#'


  #calculate f(a) and f(b)
  f_a = f(a)
  f_b = f(b)

  func <- f
  lower <- a
  current_guess <- b

  if(f_a * f_b >= 0){
    return("Signs of f(a) and f(b) must be opposites !") #throws exception if root isn't bracketed
  }

  if(abs(f_a) < abs(f_b)){
    a = current_guess
    b = lower
    f_a = f_b
    f_b = f_a
  }
  c = a  #c = largest of the upper and lower bounds
  mflag = TRUE
  steps = 0
  f_c = f_a
  value <- rep(0, length(iteration))


   while(steps < iteration && abs(b - a) > tol ){

     steps = steps + 1

     if(f_a != f_c && f_b != f_c){
      l0 = (a * f_b * f_c) / ((f_a - f_b) * (f_a - f_c))
      l1 = (b * f_a * f_c) / ((f_b - f_a) * (f_b - f_c))
      l2 = (c * f_a * f_b) / ((f_c - f_a) * (f_c - f_b))
      s = l0 + l1 + l2 #inverse quadratic interpolation
      }
     else{
       s = b - (f_b*((b - a)/ (f_b - f_a))) #secant
       }
     if ( (s < ((3*a + b)/ 4) | s > b) |
          (mflag = TRUE && (abs(s - a) >= (abs(b - c)/2))) |
          (mflag = FALSE && (abs(s - b) >= (abs(c - d)/2))) |
          (mflag = TRUE && (abs(b - c)< tol)) |
          (mflag = FALSE && (abs(c - d) < tol)) ) {
       s = (a + b)/ 2 #bisection
       mflag = TRUE
       }
     else{
       mflag = FALSE
       }
     f_s = f(s)
     d = c
     c = b
     if(f_a * f_s < 0){
       b = s
       }
     else{
       a = s
       }
     if(abs(f_a) < abs(f_b)){
       a = b
       b = a
     }
     value[steps] = c(s)
     bmethod = list(lower = lower, current_guess = current_guess, root_x = value,
                    func = func, final_root = s, iterations = steps)
     class(bmethod) = c("BrentsMethod", class(bmethod))
    }
  bmethod
}

## ---- BrentsPlot
plot.BrentsMethod <- function(x, ...) {

  #create a loop for the plot of f
  xi <- seq(x$lower-1, x$current_guess+1, by = ((x$current_guess - x$lower)/100))
  fx <- rep(0, length(xi))
  for (i in 1:length(xi)) {
    fx[i] = x$func(xi[i])
  }
  xy <- setNames(
    data.frame(xi, fx),
    c("x", "y")
  )

  #create the plot for function f
   fig <- plot_ly(data =xy, x = ~x, y = ~y, mode = 'lines', type = 'scatter', name = "Function")
   #for(i in 1:length(x$root)){
   fig <- fig %>% add_trace(x = x$final_root, y = x$func(x$final_root), mode = 'marker', name = "Root")


     # points(x = (x$root[i]), y = x$func(x$root[i]), pch = 'x', col = 'red' )


   # readline('Press ENTER to continue...')


  # }
  fig

}


## ---- BrentsSummary
summary.BrentsMethod <- function(x,...){

  result_it <- setNames(
    data.frame(x$root),
    c("Root after each iteration" )
  )
  print(result_it)
}

## ---- BrentsPrint
print.BrentsMethod <- function(x,...){

  cat('Number of iterations', x$iterations, '\n')
  cat('Root', x$final_root, '\n')


}

#test fn 1
g <- function(x){
  return((x**2)-20)
}
bq <- brents(g, -2, 5, 16)



## ---- bfun
#test functiom
brentex <- function(x){
  return((x+3)*((x+1)**2))
}


## ---- bimplement
exampbrents <- brents(brentex, -4, 4/3)


## ---- bprint
exampbrents <- brents(brentex, -4, 4/3)
print(exampbrents)

## ---- bsum
summary(exampbrents)

## ---- bplot
plot(exampbrents)


## ---- Newtons
newtons <- function(f, x0, it = 20, tol = 1e-07 ){

  #' Newton's Method
  #' @description
  #' @param f A differentiable function on \code{(a,b)} on a real interval. Note that the function has to be continuous over an interval (a,b) with x0 ∈ (a,b)
  #' @param x0 he initial guess of the root of the functionif using a multi-dimensional function write in initial guess such that c(x0, y0,..)
  #' @param it Number of iterations the algorithm should run, default set at \code{20}
  #' @param tol Tolerance level, default set at \code{1e-07}
  #'
  #' @return \code{newtons} returns an object of \code{class}.
  #'
  #' The functions \code{summary}, \code{print} and \code{plot} are used to obtain a summary table of results,
  #' returning the final result and plot correspondingly.
  #'
  #' The object of class \code{"newtons"} is a list containing the following components:
  #'
  #' \code{func}  Function inputted that is to be optimized
  #'
  #' \code{optimal_x}   the optimal value of x_1 found after each iteration
  #'
  #' \code{optimal_y}   the optimal value of x_2 found after each iteration
  #'
  #' \code{intial_x}    the intial root guess for x_1
  #'
  #' \code{intial_y}    the intual root guess for x_2 in 3-D functions and beyond
  #'
  #'  \code{sol}    the optimal point of the function found using Newton's method
  #'
  #'  \code{it}   number of iterations the algorithm ran in total
  #' @export
  #'
  #' @examples
  #'
  #' z <- function(x){
  #'       return(-x*sin(x))
  #'        }
  #'
  #' > newtons(z, 9)
  #' Number of iterations taken 5 
  #' Solution 7.978666 
  #'


  #intial values
  h <- 0.001
  iteration = 0

  #set up variables for s3 methods
  optimal_x <- rep(0, length(it))
  optimal_y<- rep(0, length(it))
  df_iter <- rep(0, length(it))
  fun <- f

  while (iteration != it) {

    #calculate y value
    iteration = iteration + 1

    #save each root for s3 methods
    optimal_x[iteration] = x0[1]
    optimal_y[iteration] = x0[2]

    #calculate the first differential for the algorithm
    firstdf = t(jacobian(f, c(x0), "Richardson"))

    #second differential
    ddf = solve(hessian(f, c(x0)))

    #newtons function
    x1 <- x0 - (ddf %*% firstdf)
    solfound = TRUE

    #check if solution is optimal
    for(i in 1:length(x1)){
      if(abs(x1[i] - x0[i]) <= tol){
        sol = list(func = f, optimal_x = optimal_x,
                   optimal_y = optimal_y, intial_x = x0[1], intial_y = x0[2],
                   sol = x0, it = iteration)

        class(sol) = c('newtons', class(sol))
        return(sol)
      }
    }

    #check if solution is optimal
    for(i in 1:length(firstdf)){
      if(abs(firstdf[i]) <= tol){
        #stop algorithm when the differential is within the tolerance
        sol = list(func = f, optimal_x = optimal_x,
                   optimal_y = optimal_y, intial_x = x0[1], intial_y = x0[2],
                   sol = x0, it = iteration)

        class(sol) = c('newtons', class(sol))
        return(sol)
      }
      break
    }

    x0 = x1 #update the value of x0 to current root value
    sol = list(func = f, optimal_x = optimal_x,
               optimal_y = optimal_y, intial_x = x0[1], intial_y = x0[2],
               sol = x0, it = iteration)

  }
  if(solfound == TRUE ){
    class(sol) = c('newtons', class(sol))

    return(sol)

  }
  else{
    print('Algorithm did not converge')
  }
}

## ---- NewtonsPlot
plot.newtons <- function(x, ...) {

  #generates plotfor 2D functions
  if(is.na(x$intial_y) == TRUE){

    #create a loop for the plot of f
    xi <- seq(x$intial_x-5, x$intial_x+5, by = 0.01)
    fx <- rep(0, length(xi))
    for (i in 1:length(xi)) {
      fx[i] = x$func(xi[i])
    }

    xy <- setNames(
      data.frame(xi, fx),
      c("x", "y")
    )

    fig <- plot_ly(data =xy, x = ~x, y = ~y, mode = 'lines', type = 'scatter', name = "Function")
    fig <- fig %>% add_trace(x = x$sol, y = x$func(x$sol), mode = 'marker', name = "Root")
    fig

  }

   #generates 3d plots

  else{
    #access required for 3d plotting
    library(plotly)

    #create a loop for the plot of f
    xi <- seq(x$intial_x-1, x$intial_x+1, by = 0.1)
    yi <- seq(x$intial_y-1, x$intial_y+1, by = 0.1)
    xy <- expand.grid(xi, yi)

    #create list of z values using function
    f_xy <- rep(0, length(xi))
    for (i in 1:nrow(xy)) {
      f_xy[i] <- x$func(xy[i,])
    }
    zi <- data.matrix(f_xy)

    #merge all data into one data frame
    plotdata <- setNames(
      data.frame(xy, zi),
      c("x", "y", "z")
    )

    #create the plot for function f
    library(plotly)
    plot_ly(plotdata, x = ~x, y = ~y, z = ~z, type="mesh3d",
            intensity = ~z,
            colorscale = 'Viridis')
  }

}

## ---- NewtonsSummary
summary.newtons <- function(x,...){

  result_it <- setNames(
    data.frame(x$optimal_x, x$optimal_y),
    c("x", "y" )
  )

  cat("Results after each iteration", '\n')
  print(result_it)

}

## ---- NewtonsPlot
print.newtons <- function(x,...){

  cat('Number of iterations taken', x$it, '\n')
  cat('Solution', x$sol, '\n')

}


## ---- newtest
#beale function
beale <- function(x){
  return((1.5 - x[1] + x[1]*x[2])**2
         + (2.25 - x[1] + x[1]*(x[2]**2))**2
         + (2.625 - x[1] + x[1]*(x[2]**3))**2)
}

## ---- newtimplement
newtexample <- newtons(beale, c(3.1,0.55))


## ---- newtprint
newtexample <- newtons(beale, c(3.1,0.55))
print(newtexample)

## ---- newtsum
summary(newtexample)

## ---- newtplot
plot(newtexample)

## ---- end

#matyas function
matyas <- function(x){
  return(0.26*(x[1]**2 + x[2]**2) - 0.48*x[1]*x[2] )
}
newtex <- newtons(matyas, c(1, 1))



## ---- lbfunc

#' Log-Barrier Method
#'
#' @param f Objective function f to which the user is trying to optimize
#' @param x0 Intial guess of the solution to the optimization problem
#' @param it Number of iterations the algorithm should run, default is set at 10
#' @param m Total number of constraints in the problem
#' @param mu multiple to increase t by, must be greater than 1
#' @param epsilon Level of which is used to show that the function is converging towards the optimal solution. Default is set at e= 1e-07
#' @param A Sum of inequality constraint functions. So for a constraint Ax ≤ b this argument takes the sum of Ax
#' @param b Sum of the right hand side of all inequality constraints from the optimization problem. So for the constraint Ax ≤ b this is the sum of all b
#' @param t The initial t value used to transform the the problem to unconstrained. Note that the larger the t value the higher accuracy the problem has, typical values lie between t = 10-20. Default set at t = 1
#'
#' @return
#' \code{logbarrier} returns an object of \code{class}.
#'
#' The functions \code{summary}, \code{print} and \code{plot} are used to obtain a summary table of results,
#' returning the final result and plot correspondingly.
#'
#' The object of class \code{"barrier"} is a list containing the following components:
#'
#' \code{func}    objective function to be optimized
#'
#' \code{cons}    Inequality constraints in the form Ax - b
#'
#' \code{x_optimal}   optimal x_1 found after each outer iteration
#'
#' \code{y_optimal}   optimal x_2 found after each outer iteration
#'
#' \code{sol}   final optimal values found by Log-Barrier Method
#'
#' \code{result_function}   The value of objective function using the \code{x_optimal} and \code{y_optimal} values
#'
#' \code{it}    number of iterations run by the algorithm
#'
#' \code{newton}    the optimal result found after each inner iteration
#'
#'
#' @export
#'
#' @examples
#'
#' objective <- function(x){
#'         return((1 - x[1])**2 + 100*(x[2] - x[1]**2)**2)
#'                                               }
#' inequality <- function(x){
#'      return((x[1]**2 + x[2]**2))
#'            }
#'
#' lbexample <- logbarrier(objective, inequality, b = 2, c(0.9, 0.9),
#'                         m = 1, mu = 10, t = 10)
#' print(lbexample)
#'
#' > print(lbexample)
#'    log-barrier solution 0.8036947 0.645237
#'     Number of iterations 1
#'


logbarrier <- function(f, A, b, x0, it = 10,  m, mu, epsilon = 1e-03, t = 10){


  #intial values
  x_t = x0
  iteration = 0
  f = f
  cons = function(x){return(A(x) - b)}
  opt_x <- rep(0, length(it))
  opt_y <- rep(0, length(it))
  resultf <- rep(0, length(it))

  #create log-barrier function of problem to be solved
  problem <- function(x){
    return(f(x) - (1/t)*(log(-(cons(x)))))
  }

  while(iteration != it){

    #set up iteration counter
    iteration = iteration + 1

    resultf[iteration] = problem(x_t)

    #use newtons method to extract the root
    newt <- newtons(problem, x_t, it = it)
    x <- newt$sol

    #update central point and t value before next iteration
    x_t <- c(x)
    t <- mu * t

    opt_x[iteration] = x_t[1]
    opt_y[iteration] = x_t[2]

    barrier = list(func = f, constraint = cons, intial = x0,
                 x_optimal = opt_x, y_optimal = opt_y, sol = x_t,
                 result_function = resultf, it = iteration,
                 newton = newt )
    class(barrier) = c('barrier', class(barrier))

    ## KKT CONDITIONS

    if (m/t <= epsilon){
      class(barrier) = c('barrier', class(barrier))
      return(barrier)
    }

    if(A(x_t) > 0){
      class(barrier) = c('barrier', class(barrier))
      return(barrier)
    }#solution is optimal

    #KKT stationary conditions
    KKTst <- jacobian(f, x_t, "Richardson") - (jacobian(A, x_t, "Richardson")/ (t*A(x_t)))

    for(i in 1:length(KKTst)){
      if(KKTst[i] <= epsilon){
        #stationary = solution is optimal
        class(barrier) = c('barrier', class(barrier))
        return(barrier)
      }

    }
  }
  barrier
}

## ---- lbplot
plot.barrier <- function(x, ...) {

  library(plotly)

  if(length(x$intial) == 1){

  ##2D PLOTS

  #create a loop for the plot of f
  xi <- seq(x$intial-5, x$intial+5, by = 0.01)
  fx <- rep(0, length(xi))
  for (i in 1:length(xi)) {
    fx[i] = x$func(xi[i])
  }

  xy <- setNames(
    data.frame(xi, fx),
    c("x", "y")
  )

  fig <- plot_ly(data =xy, x = ~x, y = ~y, mode = 'lines', type = 'scatter', name = "Function")
  #for(i in 1:length(x$root)){
  fig <- fig %>% add_trace(x = x$sol, y = x$func(x$sol), mode = 'marker', name = "Root")
  fig
  }

  else{

  #3d plots
  #create a loop for the plot of f
  xi <- seq(x$intial[1] -3, x$intial[1]+3, by = 0.1)
  yi <- seq(x$intial[2]-3, x$intial[2]+3, by = 0.1)
  xy <- expand.grid(xi, yi)

  #create list of z values for main function
  f_xy <- rep(0, length(xi))
  for (i in 1:nrow(xy)) {
    f_xy[i] <- x$func(xy[i,])
  }
  zi <- data.matrix(f_xy)

  #merge all data into one data frame
  plotdata <- setNames(
    data.frame(xy, zi),
    c("x", "y", "z")
  )

  #create the plot for function f
  library(plotly)
   fig <- plot_ly(plotdata, x = ~x, y = ~y, z = ~z, type="mesh3d",
           intensity = ~z,
           colorscale = 'Viridis')
   fig

  }
}


## ---- lbsummary
summary.barrier <- function(x,...){

  result_it <- setNames(
    data.frame(x$x_optimal, x$y_optimal, x$func(c(x$x_optimal, x$y_optimal))),
    c("x", "y", "f(x)")
  )
  cat("Result after each outer (Barrier) iteration", '\n')
  print(result_it)

  cat("Result after each inner (Newton) iteration", '\n')
  summary(x$newton)


}


## ---- lbprint
print.barrier <- function(x,..){

  cat("log-barrier solution", x$sol, '\n')
  cat("Number of iterations", x$it)

}

## ---- lbexample
#Rosenbrock function constrained to a disk

objective <- function(x){
  return((1 - x[1])**2 + 100*(x[2] - x[1]**2)**2)
  }

inequality <- function(x){
  return((x[1]**2 + x[2]**2))
  }

## ---- lbimplement
lbexample <- logbarrier(objective, inequality, b = 2, c(0.9, 0.9),
                        m = 1, mu = 10, t = 10)

## ---- lbexampleprint
lbexample <- logbarrier(objective, inequality, b = 2, c(0.9, 0.9),
                        m = 1, mu = 10, t = 10)

print(lbexample)

## ---- lbexamplesum
summary(lbexample)

## ---- lbexampleplot
plot(lbexample)

## ---- end
