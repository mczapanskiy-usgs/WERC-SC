# mlogit tutorial script taken from Viton 2015
# http://facweb.knowlton.ohio-state.edu/pviton/courses2/crp5700/5700-mlogit.pdf

# script for mlogit document. Note that the file locations refer to
# my hard disk; and you will need to alter these for your use
# this works in mlogit 0.2-2 and involves a change of syntax for the
# mxl, where the alt specific dummys are random. See model 5 below.
# read in the data
library(foreign)
clogit <- read.table("clogit.dat",
                     col.names=c("mode","ttme","invc","invt","gc",
                                 "chair","hinc","psize","indj","indi",
                                 "aasc","tasc","basc","casc","hinca",
                                 "psizea","z","nij","ni"),
                     na.strings="-999")

# check the data
summary(clogit)
str(clogit)
head(clogit)

# save a version of the data in internal format
save(clogit,file="c:/work/clogit/clogit.rdata")

# read in the mlogit package
library(mlogit)

# read in the data we saved
load(file="c:/work/clogit/clogit.rdata")

# provide a choice indicator, with names
clogit$mode.ids<-factor(rep(1:4,210),labels=c("air","train","bus","car"))

# for convenience, create a special form of the dataset:
# note that we exploit the case sensititivty here: clogit is the original
# dataset, while CLOGIT (all-caps) is the version for use with the mlogit package.
CLOGIT<-mlogit.data(clogit,shape="long",choice="mode",alt.var="mode.ids")

# first model : standard logit. We save the results into a variable
# and then view them. The first command uses the un-fixed dataset, while the
# second uses the mlogit-specific dataset, and is clearly easier to type in.
# Both produce the same output.
res1<-mlogit(mode~ttme+gc, data=clogit, shape="long",
             alt.var="mode.ids")
summary(res1)

res2<-mlogit(mode~ttme+gc, data=CLOGIT)
summary(res2)

# model with income interacted with the mode-specific dummys
res3<-mlogit(mode~ttme+gc | hinc, data=CLOGIT)
summary(res3)

# model with gc varying by mode
res4<-mlogit(mode~ttme | hinc | gc, data=CLOGIT)
summary(res4)

# mixed logit model in which the alt-specific vars
# have independent normal distributions. We use Halton numbers for
# efficiency in computation, and use R=500 in our simulations
# note that the syntax for specifying random alt-specific dummys has changed.
# we set print.level to 1 to get some feedback
res5<-mlogit(mode~ttme+gc, data=CLOGIT, reflevel="car",
             rpar=c("air:(intercept)"="n","bus:(intercept)"="n",
                    "train:(intercept)"="n"),
             R=500, halton=NA, print.level=1)
summary(res5)

# print info on what was estimated for a random parameter
rpar(res5,"air:(intercept)")

# same model, but multinomial probit (not discussed in the text)
res6<-mlogit(mode~ttme+gc, data=CLOGIT, reflevel="car",
             R=500, halton=NA, probit=TRUE, print.level=1)
summary(res6)

# Omega-sub-i for each mode:
res6$omega

