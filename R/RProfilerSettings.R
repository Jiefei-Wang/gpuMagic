#If the variable does not exist
#The inherit table determine whether the property can be passed through the equal sign
#If the variable exist
#Determine whether the property needs to be checked by the profiler
#If the properties are not the same on both sides, an action is needed to fix it.

inherit=list()
inherit$newVar=list()
inherit$extVar=list()
inherit$newVar$explicit=list()
inherit$newVar$implicit=list()
inherit$extVar$explicit=list()
inherit$extVar$implicit=list()

#Default setting
inherit$newVar$explicit$default=TRUE
inherit$newVar$implicit$default=FALSE
inherit$extVar$explicit$default=TRUE
inherit$extVar$implicit$default=FALSE

#default

#special treatment
var="precisionType"
inherit$newVar$explicit[[var]]=FALSE
inherit$newVar$implicit[[var]]=FALSE
inherit$extVar$explicit[[var]]=FALSE
inherit$extVar$implicit[[var]]=FALSE

var="var"
inherit$newVar$explicit[[var]]=FALSE
inherit$newVar$implicit[[var]]=FALSE
inherit$extVar$explicit[[var]]=FALSE
inherit$extVar$implicit[[var]]=FALSE



var="dataType"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=TRUE
inherit$extVar$explicit[[var]]=TRUE
inherit$extVar$implicit[[var]]=TRUE


var="size1"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=TRUE
inherit$extVar$explicit[[var]]=TRUE
inherit$extVar$implicit[[var]]=TRUE

var="size2"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=TRUE
inherit$extVar$explicit[[var]]=TRUE
inherit$extVar$implicit[[var]]=TRUE

var="value"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=TRUE
inherit$extVar$explicit[[var]]=TRUE
inherit$extVar$implicit[[var]]=TRUE

var="shared"

var="location"

var="version"
inherit$newVar$explicit[[var]]=FALSE
inherit$newVar$implicit[[var]]=FALSE
inherit$extVar$explicit[[var]]=FALSE
inherit$extVar$implicit[[var]]=FALSE

var="address"
inherit$newVar$explicit[[var]]=FALSE
inherit$newVar$implicit[[var]]=FALSE
inherit$extVar$explicit[[var]]=FALSE
inherit$extVar$implicit[[var]]=FALSE


var="compileSize1"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=TRUE
inherit$extVar$explicit[[var]]=TRUE
inherit$extVar$implicit[[var]]=TRUE

var="compileSize2"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=TRUE
inherit$extVar$explicit[[var]]=TRUE
inherit$extVar$implicit[[var]]=TRUE

var="compileValue"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=TRUE
inherit$extVar$explicit[[var]]=TRUE
inherit$extVar$implicit[[var]]=TRUE

var="transpose"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=TRUE
inherit$extVar$explicit[[var]]=TRUE
inherit$extVar$implicit[[var]]=TRUE

var="require"
inherit$newVar$explicit[[var]]=FALSE
inherit$newVar$implicit[[var]]=FALSE
inherit$extVar$explicit[[var]]=FALSE
inherit$extVar$implicit[[var]]=FALSE

var="constVal"

var="constDef"

var="initialization"

var="isRef"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=FALSE
inherit$extVar$explicit[[var]]=FALSE
inherit$extVar$implicit[[var]]=FALSE

var="ref"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=FALSE
inherit$extVar$explicit[[var]]=FALSE
inherit$extVar$implicit[[var]]=FALSE

var="isSeq"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=FALSE
inherit$extVar$explicit[[var]]=FALSE
inherit$extVar$implicit[[var]]=FALSE

var="seq"
inherit$newVar$explicit[[var]]=TRUE
inherit$newVar$implicit[[var]]=FALSE
inherit$extVar$explicit[[var]]=FALSE
inherit$extVar$implicit[[var]]=FALSE



#What should the profiler do when variable exist
#structure
#var--
#     act
#     validInLoop
#
#act: no action, version bump, version bump with definiton changes, rename var
#version bump: no definition changes, just give a version bump and change the property
#rename var: create a new variable
#
#validInLoop: if this changes can be applied inside the loop
#By default, no action, version bump can be applied inside the loop

inheritAct=list()
inheritAct$explicit=list()
inheritAct$implicit=list()

var="dataType"
inheritAct$explicit[[var]]=list(act="rename var")
inheritAct$implicit[[var]]=list(act="rename var")

var="size1"
inheritAct$explicit[[var]]=list(act="rename var")
inheritAct$implicit[[var]]=list(act="rename var")

var="size2"
inheritAct$explicit[[var]]=list(act="rename var")
inheritAct$implicit[[var]]=list(act="rename var")

var="value"
inheritAct$explicit[[var]]=list(act="version bump")
inheritAct$implicit[[var]]=list(act="version bump")

var="compileSize1"
inheritAct$explicit[[var]]=list(act="version bump")
inheritAct$implicit[[var]]=list(act="version bump")

var="compileSize2"
inheritAct$explicit[[var]]=list(act="version bump")
inheritAct$implicit[[var]]=list(act="version bump")

var="compileValue"
inheritAct$explicit[[var]]=list(act="version bump")
inheritAct$implicit[[var]]=list(act="version bump")

var="transpose"
inheritAct$explicit[[var]]=list(act="version bump")
inheritAct$implicit[[var]]=list(act="version bump")

var="shared"
inheritAct$explicit[[var]]=list(act="rename var")

var="location"
inheritAct$explicit[[var]]=list(act="rename var")

var="constVal"
inheritAct$explicit[[var]]=list(act="rename var")

var="constDef"
inheritAct$explicit[[var]]=list(act="rename var")

var="initialization"
inheritAct$explicit[[var]]=list(act="rename var")


# explicit TRUE
# implicit TRUE
# var="dataType"
# var="size1"
# var="size2"
# var="value"
# var="compileSize1"
# var="compileSize2"
# var="compileValue"
# var="transpose"

# explicit TRUE
# implicit FALSE
# var="shared"
# var="location"
# var="constVal"
# var="constDef"
# var="initialization"
# var="isRef"
# var="ref"
# var="isSeq"
# var="seq"

