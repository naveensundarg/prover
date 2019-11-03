
##
ShadowProver is a theorem prover (multi-modal logic with quantifiers). 

```python

from interface import prove

assumptions = [
    "(Believes! john happy)", 
    "(Believes! john smiling)" ]

goal = "(Believes! john (and happy smiling))"

prove(assumptions, goal)

```

### Under the hood

ShadowProver is a novel multi-modal + extensional logic theorem prover that uses a technique called **shadowing** to achieve speed without sacrificing consistency in the system. Extant first-order modal logic theorem provers that can work with arbitrary inference schemata are built upon first-order theorem provers. They achieve the reduction to first-order logic via two methods.
In the first method, modal operators are simply represented by first-order predicates. This approach is the fastest but can quickly lead to well-known inconsistencies as demonstrated
in [Bringsjord and Govindarajulu, 2012] (see the Slate proof below). In the second
method, the entire proof theory is implemented intricately
in first-order logic, and the reasoning is carried out
within first-order logic. Here, the first-order theorem prover
simply functions as a declarative programming system. This
approach, while accurate, can be excruciatingly slow.

![inconsistency](https://raw.githubusercontent.com/naveensundarg/prover/master/docs/inconsistency.png) 


We use
a different approach, in which we alternate between calling
a first-order theorem prover and applying modal inference
schemata. When we call the first-order prover, all modal
atoms are converted into propositional atoms (i.e., shadowing),
to prevent substitution into modal contexts. This approach
achieves speed without sacrificing consistency. The
prover also lets us add arbitrary inference schemata to the
calculus by using a special-purpose language. While we use
the prover in our simulations, describing the prover in more
detail is out of scope for the present paper.

![concept](https://raw.githubusercontent.com/naveensundarg/prover/master/docs/concept.png) 


### References

[Bringsjord and Govindarajulu, 2012] Selmer Bringsjord and
Naveen Sundar Govindarajulu. Given the Web, What is
Intelligence, Really? Metaphilosophy, 43(4):361–532, 2012.


### Resources
For more details see:

https://drive.google.com/open?id=0B9Vb2O21ibhaZTNLd21wd21adjg
