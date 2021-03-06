# show info without if-statement
showInfo = function(show.info, ...) {
  if (show.info)
    messagef(...)
}


# load required extra packages
loadPackages = function(control) {
  if (control$infill.opt == "cmaes")
    requirePackages("cmaesr", why = "proposePoints")
  if (control$n.objectives == 1L && control$propose.points > 1L && control$multipoint.method == "moimbo")
    requirePackages("emoa", why = "proposePoints")
}


# for Parego: calculate all integer vectors of length k with sum n
combWithSum = function(n, k) {
  fun = function(n, k) {
    if (k == 1L)
      list(n)
    else
      unlist(lapply(0:n, function(i) Map(c, i, fun(n - i, k - 1L))),
        recursive = FALSE)
  }
  matrix(unlist(fun(n, k)), ncol = k, byrow = TRUE)
}

getFileBackupName = function(fn) {
  file.path(dirname(fn), sprintf(".~%s", basename(fn)))
}

getRandomSeed = function() {
  if (!exists(".Random.seed", .GlobalEnv))
    set.seed(NULL)
  get(".Random.seed", .GlobalEnv)
}

measureTime = function(expr, ee = parent.frame()) {
  before = proc.time()[3L]
  force(expr)
  as.numeric(proc.time()[3L] - before)
}

# checks if a parameter or par.set is only numeric and has no dependencies/requires
# FIXME: remove as soon as this is in ParamHelper
isSimpleNumeric = function(par) {
  isNumeric(par, include.int = TRUE) && !hasRequires(par)
}