.profileExplicitDefine = c("Matrix", "Scalar", "subRef", ":", "seq", 
    "t_nocpy")
# If the variable does not exist 
# The inherit table determine whether the property can be passed through the equal sign 
# If the variable exist 
# Determine whether the property needs to be checked by the profiler 
# If the properties are not the same on both sides, an action
# is needed to fix it.

inherit = list()
inherit$newVar = list()
inherit$extVar = list()
inherit$newVar$explicit = list()
inherit$newVar$implicit = list()
inherit$extVar$explicit = list()
inherit$extVar$implicit = list()

inherit$newVar$implicit = c("precisionType", "dataType", "size1", "size2", 
    "value")


# default

# special treatment
var = "precisionType"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = FALSE
inherit$extVar$implicit[[var]] = FALSE

var = "var"
inherit$newVar$explicit[[var]] = FALSE
inherit$extVar$explicit[[var]] = FALSE
inherit$extVar$implicit[[var]] = FALSE



var = "dataType"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = TRUE


var = "size1"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = TRUE

var = "size2"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = TRUE

var = "value"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = TRUE

var = "transpose"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = TRUE

var = "shared"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = FALSE


var = "location"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = FALSE


var = "constVal"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = FALSE


var = "constDef"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = FALSE


var = "initialization"
inherit$newVar$explicit[[var]] = TRUE
inherit$extVar$explicit[[var]] = TRUE
inherit$extVar$implicit[[var]] = FALSE






# What should the profiler do when variable exist structure var-- act
# warningLevel act: no action, version bump, version bump with
# definiton changes, rename var version bump: no definition changes,
# just give a version bump and change the property rename var: create a
# new variable warningLevel: if this changes are applied inside the
# loop, a warning(=1) or an error(=2) will be given By default, no
# action, version bump can be applied inside the loop, rename variable
# is not allowed version bump will give a warning and rename variable
# will have an error

inheritAct = list()
inheritAct$explicit = list()
inheritAct$implicit = list()



var = "dataType"
inheritAct$explicit[[var]] = list(act = "rename var")
inheritAct$implicit[[var]] = list(act = "rename var")

var = "size1"
inheritAct$explicit[[var]] = list(act = "rename var")
inheritAct$implicit[[var]] = list(act = "rename var")

var = "size2"
inheritAct$explicit[[var]] = list(act = "rename var")
inheritAct$implicit[[var]] = list(act = "rename var")

var = "value"
inheritAct$explicit[[var]] = list(act = "version bump", warningLevel = 0)
inheritAct$implicit[[var]] = list(act = "version bump", warningLevel = 0)

var = "transpose"
inheritAct$explicit[[var]] = list(act = "version bump")
inheritAct$implicit[[var]] = list(act = "version bump")

var = "shared"
inheritAct$explicit[[var]] = list(act = "rename var")

var = "location"
inheritAct$explicit[[var]] = list(act = "rename var")

var = "constVal"
inheritAct$explicit[[var]] = list(act = "rename var")

var = "constDef"
inheritAct$explicit[[var]] = list(act = "rename var")

var = "initialization"
inheritAct$explicit[[var]] = list(act = "rename var")

var = "isSpecial"
inheritAct$explicit[[var]] = list(act = "rename var")

var = "specialType"
inheritAct$explicit[[var]] = list(act = "rename var")

var = "specialContent"
inheritAct$explicit[[var]] = list(act = "rename var")
# explicit TRUE implicit TRUE var='dataType' var='size1' var='size2'
# var='value' var='compileSize1' var='compileSize2' var='compileValue'
# var='transpose'

# explicit TRUE implicit FALSE var='shared' var='location'
# var='constVal' var='constDef' var='initialization' var='isRef'
# var='ref' var='isSeq' var='seq'

