
library("DiceKriging")
devtools::load_all()

options(mlrMBO.debug.mode = FALSE)
options(width = 170)

library("ParamHelpers")
library("mlrCPO")


par.set <- pSS(x: numeric[-4, 4], x2: numeric[-4, 4], x3: numeric[-4, 4], noise: numeric[1, 1])
par.set2 <- pSS(x: numeric[-4, 4], x2: numeric[-4, 4], x3: numeric[-4, 4], noise: numeric[0.5, 0.5])
par.set3 <- pSS(x: numeric[-4, 4], x2: numeric[-4, 4], x3: numeric[-4, 4], noise: numeric[0.1, 0.1])

par.set4 <- pSS(x: numeric[-4, 4], x2: numeric[-4, 4], x3: numeric[-4, 4], noise: numeric[0.1, 0.1], extra.x: numeric[1, 2] [[trafo = function(x) -x]])

obj <- makeSingleObjectiveFunction(name = "test",
  fn = function(x) {
    res <- rnorm(1, x$x^2, x$noise)
    attr(res, "extras") <- list(a = 1, .b = list(1, 2, 3))
    res
  },
  par.set = par.set,
  noisy = TRUE,
  has.simple.signature = FALSE,
  minimize = TRUE
)

obj2 <- makeSingleObjectiveFunction(name = "test",
  fn = function(x) {
    res <- rnorm(1, x$x^2 + x$extra.x, x$noise)
    attr(res, "extras") <- list(a = 1, .b = list(1, 2, 3))
    res
  },
  par.set = par.set4,
  noisy = TRUE,
  has.simple.signature = FALSE,
  minimize = TRUE
)




MAXITERATIONS <- 10
designsize <- 5

unlink("out.rout")

ctrl <- makeMBOControl(
    propose.points = 1,
    impute.y.fun = function(x, y, opt.path, ...) {
      res <- 4
      attr(res, "extras") <- list(a = 2, .b = list(1))
      res
    },
    suppress.eval.errors = TRUE,
    save.on.disk.at = seq_len(MAXITERATIONS + 1),
    save.file.path = "out.rout",
    on.surrogate.error = "warn") %>%
  setMBOControlInfill(
    crit = makeMBOInfillCritCB(),
    opt.focussearch.maxit = 10,
    opt.focussearch.points = 2000) %>%
  setMBOControlMultiPoint(method = "cb") %>%
  setMBOControlTermination(iters = MAXITERATIONS)

mboresult <- mbo(
  fun = obj,
  design = generateRandomDesign(n = designsize, par.set = getParamSet(obj)),
  control = ctrl,
  show.info = TRUE
)

load("out.rout")
opt.state <- resetOptStateItersTerminator(opt.state, MAXITERATIONS * 2, seq_len(MAXITERATIONS * 3 + 1))
opt.state <- resetOptStateParamSet(opt.state, par.set2)

mboContinue(opt.state)

load("out.rout")
opt.state <- resetOptStateItersTerminator(opt.state, MAXITERATIONS * 3, seq_len(MAXITERATIONS * 3 + 1))
opt.state <- resetOptStateParamSet(opt.state, par.set3)

mboContinue(opt.state)

load("out.rout")
opt.state <- resetOptStateItersTerminator(opt.state, MAXITERATIONS * 5, seq_len(MAXITERATIONS * 5 + 1))
opt.state <- resetOptStateObjective(opt.state, obj2, defaults = list(extra.x = 0))
mboContinue(opt.state)

load("out.rout")



library("ggplot2")

ggplot(as.data.frame(opt.state$opt.path), aes(x = x, y = y, color = as.factor(noise))) + geom_point()
ggplot(as.data.frame(opt.state$opt.path), aes(x = x, y = y, color = extra.x)) + geom_point()

ggplot(as.data.frame(opt.state$opt.path), aes(x = extra.x, y = y, color = x)) + geom_point()

ggplot(as.data.frame(opt.state$opt.path), aes(x = x2, y = y, color = log(noise))) + geom_point()
ggplot(as.data.frame(opt.state$opt.path), aes(x = x3, y = y, color = log(noise))) + geom_point()
