


#' @export
resetOptStateItersTerminator <- function(opt.state, iters, save.on.disk.at) {
  opt.state$opt.problem$control <- setMBOControlTermination(opt.state$opt.problem$control, iters = iters)
  opt.state$opt.problem$control$save.on.disk.at <- save.on.disk.at
  opt.state$progress <- 0
  opt.state$state <- "iter"
  opt.state
}

#' @export
resetOptStateParamSet <- function(opt.state, par.set) {
  opt.state$opt.path$par.set <- par.set
  attributes(opt.state$opt.problem$fun)$par.set <- par.set
  opt.state
}
