package com.naveensundarg.shadow.prover.core.ccprovers;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.internals.AgentSnapShot;
import com.naveensundarg.shadow.prover.core.internals.ConsistentSubsetFinder;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.time.Duration;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;


public class mCCounterFactualProver implements Prover {

    /*
     *
     */
    private static boolean defeasible = false;
    private static boolean verbose = false;
    private final boolean reductio;
     private final mCCounterFactualProver parent;
    private final Set<Expander> expanders;
    private Set<Formula> currentAssumptions;
    private Set<Formula> prohibited;
    private Problem currentProblem;

    static  String indent = "";

    static ColoredPrinter coloredPrinter = new ColoredPrinter.Builder(1, false)
            .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.BLACK)   //setting format
            .build();

    public mCCounterFactualProver() {

        prohibited = Sets.newSet();
        parent = null;
        reductio = false;
        expanders = CollectionUtils.newEmptySet();
    }

    private mCCounterFactualProver(mCCounterFactualProver parent) {

        prohibited = CollectionUtils.setFrom(parent.prohibited);
        this.parent = parent;
        reductio = false;
        expanders = CollectionUtils.newEmptySet();
    }

    private mCCounterFactualProver(mCCounterFactualProver parent, boolean reductio) {

        prohibited = CollectionUtils.setFrom(parent.prohibited);
        this.parent = parent;
        this.reductio = reductio;
        expanders = CollectionUtils.newEmptySet();
    }

    private static void log(String message) {

        if (verbose) {

            System.out.println(message);
        } else {


        }
    }

    private static <T> Set<T> formulaeOfTypeWithConstraint(Set<Formula> formulas, Class c, Predicate<Formula> constraint) {

        return formulas.
                stream().
                filter(c::isInstance).
                filter(constraint).
                map(f -> (T) f).
                collect(Collectors.toSet());
    }

    private static <T> Set<T> formulaOfType(Set<Formula> formulas, Class c) {

        return formulaeOfTypeWithConstraint(formulas, c, f -> true);

    }

    private static <T> Set<T> level2FormulaeOfTypeWithConstraint(Set<Formula> formulas, Class c, Predicate<Formula> constraint) {

        return formulas.
                stream().
                filter(a -> a.getLevel() == 2).
                filter(c::isInstance).
                filter(constraint).
                map(f -> (T) f).
                collect(Collectors.toSet());
    }

    private static <T> Set<T> level2FormulaeOfType(Set<Formula> formulas, Class c) {

        return level2FormulaeOfTypeWithConstraint(formulas, c, f -> true);

    }

    private static mCCounterFactualProver root(mCCounterFactualProver cognitiveCalculusProver) {


        mCCounterFactualProver current = cognitiveCalculusProver.parent;

        if (current == null) {
            return cognitiveCalculusProver;
        }
        while (current.parent != null) {

            current = current.parent;
        }

        return current;

    }

    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        return prove(assumptions, formula, CollectionUtils.newEmptySet());
    }

    private boolean alreadySeen(Set<Formula> assumptions, Formula formula) {


        if (parent == null) {
            return false;
        }


        mCCounterFactualProver node = parent;

        while (node != null) {

            if (node.currentProblem.equals(currentProblem)) {

                return true;
            }

            node = node.parent;

        }

        return false;

    }

    private synchronized Optional<Justification> prove(Set<Formula> assumptions, Formula formula, Set<Formula> added) {


        currentAssumptions = assumptions;


        if (defeasible) {

            Optional<Justification> got = defeasible(formula);

            if (got != null) return got;

        } else {


        }


        Prover folProver = SnarkWrapper.getInstance();

        Set<Formula> base = CollectionUtils.setFrom(assumptions);

        Formula shadowedGoal = formula.shadow(1);

        Optional<Justification> shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);



        while (!shadowedJustificationOpt.isPresent()) {

            int sizeBeforeExpansion = base.size();
            base = expand(base, added, formula);
            int sizeAfterExpansion = base.size();

            if (base.contains(formula)) {
                return Optional.of(TrivialJustification.trivial(assumptions, formula));
            }

            Optional<Justification> andProofOpt = tryAND(base, formula, added);


            if (andProofOpt.isPresent()) {
                return andProofOpt;
            }

            if (!reductio) {


                Optional<Justification> forAllIntroOpt = tryForAllIntro(base, formula, added);

                if (forAllIntroOpt.isPresent()) {
                    return forAllIntroOpt;
                }

                Optional<Justification> existsIntroOpt = tryExistsIntro(base, formula, added);

                if (existsIntroOpt.isPresent()) {
                    return existsIntroOpt;
                }


                Optional<Justification> ifIntroOpt = tryIfIntro(base, formula, added);

                if (ifIntroOpt.isPresent()) {
                    return ifIntroOpt;
                }


                Optional<Justification> counterFacIntroOpt = tryCounterFactIntro(base, formula, added);

                if (counterFacIntroOpt.isPresent()) {
                    return counterFacIntroOpt;
                }


            }


            Optional<Justification> caseProofOpt = tryOR(base, formula, added);


            if (caseProofOpt.isPresent()) {
                return caseProofOpt;
            }
            if (base.size() < 50 && !reductio) {

                Optional<Justification> reductioProofOpt = tryReductio(base, formula, added);


                if (reductioProofOpt.isPresent()) {
                    return reductioProofOpt;
                }
            }


            if (sizeAfterExpansion <= sizeBeforeExpansion) {
                return Optional.empty();
            }

            shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);
         }

        if (shadowedJustificationOpt.isPresent()) {

          //  logFOLCall(true, shadow(base), shadowedGoal);
            return shadowedJustificationOpt;
        }

        return Optional.empty();
    }

    private Optional<Justification> tryIfIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Implication) {

            Implication implication = (Implication) formula;

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            indent = indent + "\t";
            tryLog("Tying if intro", formula);
            Optional<Justification> consOpt = this.prove(Sets.add(base, antecedent), consequent);
            indent = indent.substring(0, indent.length()-1);

            if (consOpt.isPresent()) {

                return Optional.of(new CompoundJustification("If Intro", CollectionUtils.listOf(consOpt.get())));

            } else {

                return Optional.empty();
            }
            //TODO: the reverse

        } else {

            return Optional.empty();
        }
    }
    private Optional<Justification> tryCounterFactIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof CounterFactual) {

            CounterFactual counterFactual = (CounterFactual) formula;

            Formula antecedent = counterFactual.getAntecedent();
            Formula consequent = counterFactual.getConsequent();

            indent = indent + "\t";
            tryLog("Tying counterfactual intro", formula);
            Optional<Justification> counterfactualIntroOpt = (new ConsistentSubsetFinder()).find(this, base, antecedent, consequent);
            indent = indent.substring(0, indent.length()-1);

            if (counterfactualIntroOpt.isPresent()) {

                return Optional.of(new CompoundJustification("Counterfactual Intro", CollectionUtils.listOf(counterfactualIntroOpt.get())));

            } else {

                return Optional.empty();
            }
            //TODO: the reverse

        } else {

            return Optional.empty();
        }
    }
    private Optional<Justification> tryExistsIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Existential) {

            Existential existential = (Existential) formula;
            Variable[] vars = existential.vars();

            if (vars.length == 1) {

                Map<Variable, Value> subs = CollectionUtils.newMap();
                subs.put(vars[0], Constant.newConstant());

                tryLog("Trying to prove existential", formula);
                return this.prove(base, ((Existential) formula).getArgument().apply(subs));

            } else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }


        } else {

            return Optional.empty();
        }
    }

    private Optional<Justification> tryForAllIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Universal) {

            Universal universal = (Universal) formula;
            Variable[] vars = universal.vars();

            tryLog("Trying to prove universal" , universal);
            if (vars.length == 1) {

                Map<Variable, Value> subs = CollectionUtils.newMap();
                subs.put(vars[0], Constant.newConstant());
                //TODO: Verify this.
                Optional<Justification> ansOpt = this.prove(base, ((Universal) formula).getArgument().apply(subs));

                if(ansOpt.isPresent()){

                    return Optional.of(new CompoundJustification("ForAllIntro", CollectionUtils.listOf(ansOpt.get())));
                }

                else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }
            } else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }


        } else if (formula instanceof Not && ((Not) formula).getArgument() instanceof Existential) {

            //formula = (not (exists [vars] kernel)) == (forall [vars] (not kernel))
            Formula kernel = ((Existential) ((Not) formula).getArgument()).getArgument();
            Variable[] vars = ((Existential) ((Not) formula).getArgument()).vars();


            if (vars.length == 1) {

                Map<Variable, Value> subs = CollectionUtils.newMap();
                subs.put(vars[0], Constant.newConstant());
                            tryLog("Trying to prove " , (new Not(kernel)).apply(subs));

                return this.prove(base, (new Not(kernel)).apply(subs));

            } else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }
        } else {

            return Optional.empty();
        }
    }

    private Optional<Justification> tryNEC(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Necessity) {

            tryLog("Trying to prove necessity", formula);
            Optional<Justification> innerProof = this.prove(Sets.newSet(), ((Necessity) formula).getFormula());

            if (innerProof.isPresent()) {

                return Optional.of(new CompoundJustification("Nec Intro", CollectionUtils.listOf(innerProof.get())));
            } else {

                return Optional.empty();
            }

        } else {

            return Optional.empty();
        }
    }

    private Optional<Justification> tryPOS(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Not && ((Not) formula).getArgument() instanceof Possibility) {

            Formula core = ((Possibility) ((Not) formula).getArgument()).getFormula();

            tryLog("Trying to prove necessity", new Necessity(new Not(core)));

            Optional<Justification> innerProof = this.prove(Sets.newSet(), new Necessity(new Not(core)));

            if (innerProof.isPresent()) {

                return Optional.of(new CompoundJustification("Pos Intro", CollectionUtils.listOf(innerProof.get())));
            } else {

                return Optional.empty();
            }

        } else {

            return Optional.empty();
        }
    }

    private Optional<Justification> defeasible(Formula formula) {
        if (formula instanceof CanProve) {

            mCCounterFactualProver outer = this;

            final Duration timeout = Duration.ofSeconds(10);
            ExecutorService executor = Executors.newSingleThreadExecutor();

            final Future<Optional<Justification>> handler = executor.submit((Callable) () -> {

                mCCounterFactualProver root = root(outer);

                mCCounterFactualProver cognitiveCalculusProver = new mCCounterFactualProver();

                return cognitiveCalculusProver.prove(root.currentAssumptions.stream().filter(x -> !x.subFormulae().contains(formula)).collect(Collectors.toSet()),
                        ((CanProve) formula).getFormula());
            });

            try {
                Optional<Justification> got = handler.get(timeout.toMillis(), TimeUnit.MILLISECONDS);

                return got;

            } catch (TimeoutException e) {
                handler.cancel(true);
                return Optional.empty();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } catch (ExecutionException e) {
                e.printStackTrace();
            }

            executor.shutdownNow();

        }

        if (formula instanceof Not) {

            Formula argument = ((Not) formula).getArgument();
            if (argument instanceof CanProve) {

                mCCounterFactualProver outer = this;

                final Duration timeout = Duration.ofSeconds(5);
                ExecutorService executor = Executors.newSingleThreadExecutor();

                final Future<Optional<Justification>> handler = executor.submit((Callable) () -> {

                    mCCounterFactualProver root = root(outer);

                    mCCounterFactualProver cognitiveCalculusProver = new mCCounterFactualProver();

                    return cognitiveCalculusProver.prove(root.currentAssumptions.stream().filter(x -> !x.subFormulae().contains(argument)).collect(Collectors.toSet()),
                            (((CanProve) argument).getFormula()));
                });

                try {
                    Optional<Justification> got = handler.get(timeout.toMillis(), TimeUnit.MILLISECONDS);
                    executor.shutdownNow();

                    return Optional.empty();


                } catch (TimeoutException | InterruptedException | ExecutionException e) {
                    handler.cancel(true);
                    executor.shutdownNow();

                    return Optional.of(TrivialJustification.trivial(currentAssumptions, formula));
                }

            }

        }
        return null;
    }

    private Optional<Justification> tryAND(Set<Formula> base, Formula formula, Set<Formula> added) {

        if (formula instanceof And) {

            And and = (And) formula;
            tryLog("Trying to prove conjunction", and);


            Formula conjuncts[] = and.getArguments();

            List<Optional<Justification>> conjunctProofsOpt = Arrays.stream(conjuncts).map(conjunct -> {

                mCCounterFactualProver cognitiveCalculusProver = new mCCounterFactualProver(this);
                return cognitiveCalculusProver.prove(base, conjunct);
            }).collect(Collectors.toList());


            if (conjunctProofsOpt.stream().allMatch(Optional::isPresent)) {

                return Optional.of(
                        new CompoundJustification("andIntro",
                                conjunctProofsOpt.stream().map(Optional::get).collect(Collectors.toList())));
            }
        }
        return Optional.empty();
    }

    private Optional<Justification> tryOR(Set<Formula> base, Formula formula, Set<Formula> added) {

        Set<Or> level2ORs = level2FormulaeOfType(base, Or.class);

        Optional<Or> someOrOpt = level2ORs.stream().findAny();

        if (someOrOpt.isPresent()) {

            Or someOr = someOrOpt.get();
            Formula[] disjuncts = someOr.getArguments();

            Set<Formula> reducedBase = CollectionUtils.setFrom(base);
            reducedBase.remove(someOr);

            List<Optional<Justification>> casesOpt = Arrays.stream(disjuncts).map(disjunct -> {
                mCCounterFactualProver cognitiveCalculusProver = new mCCounterFactualProver(this);

                Set<Formula> newBase = CollectionUtils.setFrom(reducedBase);
                newBase.add(disjunct);

                return cognitiveCalculusProver.prove(newBase, formula, CollectionUtils.setFrom(added));

            }).collect(Collectors.toList());

            boolean proved = casesOpt.stream().allMatch(Optional::isPresent);

            if (proved) {
                return Optional.of(new CompoundJustification("ORIntro", casesOpt.stream().map(Optional::get).collect(Collectors.toList())));
            } else {
                return Optional.empty();
            }

        } else {

            return Optional.empty();
        }

    }

    private Optional<Justification> tryReductio(Set<Formula> base, Formula formula, Set<Formula> added) {

        Formula negated = Logic.negated(formula);
        if (base.contains(negated) || formula.toString().startsWith("$")) {
            return Optional.empty();
        }

        Atom atom = Atom.generate();

        tryLog("Reductio on", negated);


        Set<Formula> augmented = CollectionUtils.setFrom(base);

        augmented.add(negated);
        indent = indent + "\t";
        mCCounterFactualProver cognitiveCalculusProver = new mCCounterFactualProver(this, true);
        Optional<Justification> reductioJustOpt = cognitiveCalculusProver.prove(augmented, atom, added);
        indent = indent.substring(0,indent.length()-1);

        return reductioJustOpt.isPresent() ? Optional.of(new CompoundJustification("Reductio", CollectionUtils.listOf(reductioJustOpt.get()))) :
                Optional.empty();
    }

    private Optional<Justification> proveAgentClosure(Set<Formula> base, Formula goal) {

        if (goal instanceof Belief) {

            Belief belief = (Belief) goal;
            Value agent = belief.getAgent();
            Value time = belief.getTime();
            Formula goalBelief = belief.getFormula();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> allBelievedTillTime = agentSnapShot.allBelievedByAgentTillTime(agent, time);


            mCCounterFactualProver cognitiveCalculusProver = new mCCounterFactualProver(this);
            Optional<Justification> inner = cognitiveCalculusProver.prove(allBelievedTillTime, goalBelief);
            if (inner.isPresent()) {
                //TODO: Augment this

                return inner;
            }

        }

        if (goal instanceof Intends) {


            Intends intends = (Intends) goal;
            Value agent = intends.getAgent();
            Value time = intends.getTime();
            Formula goalKnowledge = intends.getFormula();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> allIntendedByTillTime = agentSnapShot.allIntendedByAgentTillTime(agent, time);


            mCCounterFactualProver cognitiveCalculusProver = new mCCounterFactualProver(this);
            Optional<Justification> inner = cognitiveCalculusProver.prove(allIntendedByTillTime, goalKnowledge);
            if (inner.isPresent()) {
                //TODO: Augment this

                return inner;
            }
        }

        if (goal instanceof Knowledge) {


            Knowledge knowledge = (Knowledge) goal;
            Value agent = knowledge.getAgent();
            Value time = knowledge.getTime();
            Formula goalKnowledge = knowledge.getFormula();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> allKnownByTillTime = agentSnapShot.allKnownByAgentTillTime(agent, time);


            mCCounterFactualProver cognitiveCalculusProver = new mCCounterFactualProver(this);
            Optional<Justification> inner = cognitiveCalculusProver.prove(allKnownByTillTime, goalKnowledge);
            if (inner.isPresent()) {
                //TODO: Augment this

                return inner;
            }
        }

        return Optional.empty();

    }

    private Set<Formula> expand(Set<Formula> base, Set<Formula> added, Formula goal) {


        breakUpBiConditionals(base);

        expandModalConjunctions(base, added);
        expandModalImplications(base, added);
        expandCounterToMaterial(base, added);


        if (prohibited != null) base.removeAll(prohibited);
        return base;
    }

    private void expandCounterToMaterial(Set<Formula> base, Set<Formula> added) {

        Set<Formula> counters = base.stream().filter(x-> x instanceof CounterFactual).collect(Collectors.toSet());


        base.addAll(counters.stream().map(c->{
            CounterFactual counterFactual = (CounterFactual) c;

            return new Implication(((CounterFactual) c).getAntecedent(), ((CounterFactual) c).getConsequent());

        }).collect(Collectors.toSet()));


    }


    private void breakUpBiConditionals(Set<Formula> base) {


        Set<BiConditional> biConditionals = formulaOfType(base, BiConditional.class);

        biConditionals.forEach(biConditional -> {

            base.add(new Implication(biConditional.getLeft(), biConditional.getRight()));
            base.add(new Implication(biConditional.getRight(), biConditional.getLeft()));
            base.add(new Implication(Logic.negated(biConditional.getLeft()), Logic.negated(biConditional.getRight())));
            base.add(new Implication(Logic.negated(biConditional.getRight()), Logic.negated(biConditional.getLeft())));

        });


    }


    private void expandR11a(Set<Formula> base, Set<Formula> added) {

        Set<Belief> implicationBeliefs =
                level2FormulaeOfTypeWithConstraint(base, Belief.class, b -> ((Belief) b).getFormula() instanceof Implication);


        Set<Formula> validConsequentBeliefss = implicationBeliefs.stream().
                filter(b -> base.contains(new Belief(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getAntecedent()))).
                map(b -> new Belief(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getConsequent())).
                filter(f -> !added.contains(f)).
                collect(Collectors.toSet());

        base.addAll(validConsequentBeliefss);
        added.addAll(added);

    }




    private void expandModalConjunctions(Set<Formula> base, Set<Formula> added) {

        Set<And> level2Ands = level2FormulaeOfType(base, And.class);

        for (And and : level2Ands) {

            Set<Formula> level2Conjuncts = Arrays.stream(and.getArguments()).
                    filter(conjunct -> conjunct.getLevel() == 2).
                    filter(x -> !added.contains(x)).
                    collect(Collectors.toSet());

            added.addAll(level2Conjuncts);
            base.addAll(level2Conjuncts);


        }
    }

    private void expandModalImplications(Set<Formula> base, Set<Formula> added) {


        Set<Implication> level2Ifs = level2FormulaeOfType(base, Implication.class);

        for (Implication implication : level2Ifs) {

            if (prohibited.contains(implication)) {
                continue;
            }

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            mCCounterFactualProver cognitiveCalculusProver = new mCCounterFactualProver(this);
            cognitiveCalculusProver.prohibited.addAll(prohibited);
            cognitiveCalculusProver.prohibited.add(implication);

            Set<Formula> reducedBase = CollectionUtils.setFrom(base);

            reducedBase.remove(implication);

            //TODO: use actual ancestors

            Optional<Justification> antecedentJustificationOpt = cognitiveCalculusProver.prove(reducedBase, antecedent, CollectionUtils.setFrom(added));
            if (antecedentJustificationOpt.isPresent()) {
                if (!added.contains(consequent)) {
                    base.add(consequent);
                    added.add(consequent);
                }
            }

            Set<Formula> newReducedBase = CollectionUtils.setFrom(base);
            newReducedBase.remove(implication);


            Optional<Justification> negatedConsequentJustificationOpt = cognitiveCalculusProver.prove(newReducedBase, Logic.negated(consequent), CollectionUtils.setFrom(added));
            if (negatedConsequentJustificationOpt.isPresent()) {
                if (!added.contains(consequent)) {
                    base.add(Logic.negated(antecedent));
                    added.add(Logic.negated(antecedent));
                }
            }


        }


    }

    private void expandUniversalElim(Set<Formula> base, Set<Formula> added, Formula goal) {

        //TODO: Less stupid elimination

        Set<Formula> formulae = CollectionUtils.setFrom(base);
        formulae.add(goal);

        Set<Universal> universals = base.stream().filter(f -> f instanceof Universal).map(f -> (Universal) f).collect(Collectors.toSet());

/*
        Set<Value> values = Logic.baseFormulae(formulae).stream().
                map(Predicate::allValues).
                reduce(Sets.newSet(), Sets::union).stream().filter(v -> !(v instanceof Variable) ).

                collect(Collectors.toSet());
*/


        universals.stream().forEach(universal -> {

            Formula formula = universal.getArgument();

            List<Set<Value>> smartValues = UniversalInstantiation.smartHints(universal, formulae);

            Set<List<Value>> substitutions = cartesianProduct(smartValues);
            Variable[] vars = universal.vars();

            Map<Variable, Value> mapping = CollectionUtils.newMap();
            substitutions.stream().forEach(substitution -> {

                        for (int i = 0; i < vars.length; i++) {

                            mapping.put(vars[i], substitution.get(vars.length - 1 - i));


                        }

                        Formula derived = universal.getArgument().apply(mapping);

                        if (!added.contains(derived)) {
                            base.add(derived);
                            added.add(derived);
                        }

                    }

            );


        });

    }

    private Set<Formula> shadow(Set<Formula> formulas) {
        return formulas.stream().map(f -> f.shadow(1)).collect(Collectors.toSet());
    }


    private static void expansionLog(String principle, Set<Formula> newSet) {
        if(!verbose) return;

        if (!newSet.isEmpty()) {

            coloredPrinter.print(indent+ "Forward Reasoning: ", Ansi.Attribute.BOLD, Ansi.FColor.BLUE, Ansi.BColor.NONE);
            coloredPrinter.print(principle, Ansi.Attribute.BOLD, Ansi.FColor.WHITE, Ansi.BColor.BLUE);
            coloredPrinter.clear();
            coloredPrinter.print(newSet, Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
            coloredPrinter.println("");
        }
        else {

        }

    }

    private static void tryLog(String principle, Formula goal) {
        if(!verbose) return;


            coloredPrinter.print(indent+ "Backward Reasoning: ", Ansi.Attribute.BOLD, Ansi.FColor.GREEN, Ansi.BColor.NONE);
            coloredPrinter.print(principle, Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.GREEN);
            coloredPrinter.clear();
            coloredPrinter.print(goal, Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
            coloredPrinter.println("");

    }

    private static void tryAgentClosure(Formula formula){
                if(!verbose) return;

        coloredPrinter.clear();
        coloredPrinter.print(indent+ "Trying", Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);

         coloredPrinter.print(" Agent Closure: ", Ansi.Attribute.BOLD, Ansi.FColor.WHITE, Ansi.BColor.BLACK);
            coloredPrinter.clear();
            coloredPrinter.print(formula, Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
            coloredPrinter.println("");
    }

    private static void logFOLCall(boolean success, Set<Formula> newSet, Formula goal) {

        if(!verbose) return;
        if(success){

            coloredPrinter.print(indent+ "First-Order Prover Call: ", Ansi.Attribute.BOLD, Ansi.FColor.MAGENTA, Ansi.BColor.NONE);
            coloredPrinter.print("Succeeded", Ansi.Attribute.BOLD, Ansi.FColor.GREEN, Ansi.BColor.NONE);
            coloredPrinter.println("");

        } else{

            coloredPrinter.print("First-Order Prover Call: ", Ansi.Attribute.BOLD, Ansi.FColor.MAGENTA, Ansi.BColor.NONE);
            coloredPrinter.print("Failed", Ansi.Attribute.BOLD, Ansi.FColor.RED, Ansi.BColor.NONE);
            coloredPrinter.println("");

        }

    }

}
