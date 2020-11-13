


#' @export
resetOptStateItersTerminator <- function(opt.state, iters, save.on.disk.at) {
  assertClass(opt.state, "OptState")
  assertInt(iters)
  assertIntegerish(save.on.disk.at)
  opt.state$opt.problem$control <- setMBOControlTermination(opt.state$opt.problem$control, iters = iters)
  opt.state$opt.problem$control$save.on.disk.at <- save.on.disk.at
  opt.state$progress <- 0
  opt.state$state <- "iter"
  opt.state
}

#' Adds 'default' / NA to new column if new column is added. Removing columns is an error
#' @export
resetOptStateParamSet <- function(opt.state, par.set, defaults = getDefaults(par.set)) {
  assertClass(opt.state, "OptState")
  assertClass(par.set, "ParamSet")
  assertList(defaults)
  if (length(defaults)) assertNames(names(defaults), subset.of = getParamIds(par.set))

  op <- opt.state$opt.path
  ## virgin.op <- makeOptPathDF(par.set = par.set, y.names = op$y.names, minimize = op$minimize,
  ##   add.transformed.x = op$add.transformed.x, include.error.message = !is.null(op$env$error.message),
  ##   include.exec.time = !is.null(op$env$exec.time), include.extra = !is.null(op$env$extra))
  virgin.op <- makeOptPathDF(par.set = par.set, y.names = op$y.names, minimize = op$minimize)

  addOptPathEl(virgin.op,
    x = insert(lapply(par.set$pars, ParamHelpers:::getParamNA), defaults),
    y = NA, check.feasible = FALSE)
  assertSubset(colnames(op$env$path), colnames(virgin.op$env$path))  # error if params are subtracted
  op$env$path <- insert(virgin.op$env$path[rep(1, nrow(op$env$path)), ], op$env$path)
  opt.state$opt.path$par.set <- par.set
  attributes(opt.state$opt.problem$fun)$par.set <- par.set
  opt.state
}

#' @export
resetOptStateObjective <- function(opt.state, fun, defaults = getDefaults(getParamSet(fun))) {
  assertClass(opt.state, "OptState")
  assertClass(fun, "smoof_function")
  opt.state <- resetOptStateParamSet(opt.state, getParamSet(fun), defaults = defaults)
  opt.state$opt.problem$fun <- fun
  opt.state
}
