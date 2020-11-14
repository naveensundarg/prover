from interface import *

assumptions = [
    "(Believes! a P)", 
    "(Believes! a Q)" ]


goal = "(Believes! a (and P Q))"

prove(assumptions, goal)
