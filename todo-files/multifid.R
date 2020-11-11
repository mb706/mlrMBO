
library("DiceKriging")
devtools::load_all()

options(mlrMBO.debug.mode = FALSE)
options(width = 170)

library("ParamHelpers")
library("mlrCPO")


par.set <- pSS(x: numeric[-4, 4], x2: numeric[-4, 4], x3: numeric[-4, 4], noise: numeric[1, 1])
par.set2 <- pSS(x: numeric[-4, 4], x2: numeric[-4, 4], x3: numeric[-4, 4], noise: numeric[0.5, 0.5])
par.set3 <- pSS(x: numeric[-4, 4], x2: numeric[-4, 4], x3: numeric[-4, 4], noise: numeric[0.1, 0.1])

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


MAXITERATIONS <- 20
designsize <- 10

ctrl <- makeMBOControl(
    propose.points = 1,
    impute.y.fun = function(x, y, opt.path, ...) {
      res <- 4
      attr(res, "extras") <- list(a = 2, .b = list(1))
      res
    },
    suppress.eval.errors = TRUE,
    save.on.disk.at = seq_len(MAXITERATIONS + 1),
    save.file.path = "out.rout2",
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

load("out.rout2")
opt.state <- resetOptStateItersTerminator(opt.state, 30, 1:31)
opt.state <- resetOptStateParamSet(opt.state, par.set2)

mboContinue(opt.state)

load("out.rout2")
opt.state <- resetOptStateItersTerminator(opt.state, 40, 1:41)
opt.state <- resetOptStateParamSet(opt.state, par.set3)

mboContinue(opt.state)

library("ggplot2")

ggplot(as.data.frame(opt.state$opt.path), aes(x = x, y = y, color = log(noise))) + geom_point()
ggplot(as.data.frame(opt.state$opt.path), aes(x = x2, y = y, color = log(noise))) + geom_point()
ggplot(as.data.frame(opt.state$opt.path), aes(x = x3, y = y, color = log(noise))) + geom_point()
