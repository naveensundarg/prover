package com.naveensundarg.shadow.prover.representations.cnf;

import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.core.rules.ForwardClauseRule;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Not;
import com.naveensundarg.shadow.prover.representations.formula.Or;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.binaryProduct;
import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class Clause {

    private final Set<Literal> literals;

    private final List<Literal> sortedLiterals;
    private final int weight;

    public  static AtomicInteger count = new AtomicInteger(0);


    private final Optional<List<Clause>> parents;
    private final Optional<ForwardClauseRule> originRule;
    private int ID;


    static Comparator<Literal> literalComparator = (x, y)-> {
        int xWeight =  x.getWeight();
        int yWeight = y.getWeight();

        if(xWeight!=yWeight) {

            return xWeight - yWeight;
        }
         else {

            return x.getPredicate().getName().compareTo(y.getPredicate().getName());
        }



    };


    public Clause(Predicate P){
        this.literals = Sets.with(new Literal(P, false));


        this.weight = P.getWeight();

        ID = count.getAndIncrement();


        Set<Variable> variables = P.variablesPresent();

        List<Literal>  tempLiterals = literals.stream().sorted(literalComparator).collect(Collectors.toList());

        Map<Variable, Value> mapping = Logic.simplify(variables);


        this.sortedLiterals = tempLiterals.stream().map(x->x.apply(mapping)).collect(Collectors.toList());


        this.originRule = Optional.empty();
        this.parents = Optional.empty();
    }



    public static Clause fromClauses(List<Clause> clauses){

        return new Clause(clauses.stream().map(Clause::getLiterals).reduce(newSet(), Sets::union));
    }

    public Clause(Set<Literal> literals){

        literals = literals.stream().filter(Logic::canKeepEquality).collect(Collectors.toSet());
        this.weight = literals.stream().mapToInt(Literal::getWeight).reduce(0, Integer::sum);
        ID = count.getAndIncrement();



        List<Literal>  tempLiterals = literals.stream().sorted(literalComparator).collect(Collectors.toList());

        Set<Variable> variables = tempLiterals.stream().map(x->x.getPredicate().variablesPresent()).reduce(Sets.newSet(), Sets::union);
        Map<Variable, Value> mapping = Logic.simplify(variables);

        this.sortedLiterals = tempLiterals.stream().map(x->x.apply(mapping)).collect(Collectors.toList());
        this.literals  = Collections.unmodifiableSet(sortedLiterals.stream().collect(Collectors.toSet()));

        this.parents = Optional.empty();
        this.originRule = Optional.empty();

    }
    public Clause(Set<Literal> literals, List<Clause> parents){

        literals = literals.stream().filter(Logic::canKeepEquality).collect(Collectors.toSet());
        this.weight = literals.stream().mapToInt(Literal::getWeight).reduce(0, Integer::sum);
        ID = count.getAndIncrement();



        List<Literal>  tempLiterals = literals.stream().sorted(literalComparator).collect(Collectors.toList());

        Set<Variable> variables = tempLiterals.stream().map(x->x.getPredicate().variablesPresent()).reduce(Sets.newSet(), Sets::union);
        Map<Variable, Value> mapping = Logic.simplify(variables);

        this.sortedLiterals = tempLiterals.stream().map(x->x.apply(mapping)).collect(Collectors.toList());
        this.literals  = Collections.unmodifiableSet(sortedLiterals.stream().collect(Collectors.toSet()));

        this.parents = Optional.of(parents);
        this.originRule = Optional.empty();

    }

    public Clause(boolean simplify, Set<Literal> literals){

        this.weight = literals.stream().mapToInt(Literal::getWeight).reduce(0, Integer::sum);
        ID = count.getAndIncrement();



        List<Literal>  tempLiterals = literals.stream().sorted(literalComparator).collect(Collectors.toList());

        Set<Variable> variables = tempLiterals.stream().map(x->x.getPredicate().variablesPresent()).reduce(Sets.newSet(), Sets::union);
        Map<Variable, Value> mapping = Logic.simplify(variables);

        if(simplify){
            this.sortedLiterals = tempLiterals.stream().map(x->x.apply(mapping)).collect(Collectors.toList());

        } else {

            this.sortedLiterals = tempLiterals;

        }
        this.literals  = Collections.unmodifiableSet(sortedLiterals.stream().collect(Collectors.toSet()));
        this.parents = Optional.empty();
        this.originRule = Optional.empty();

    }

    public int getID() {
        return ID;
    }

    public Set<Literal> getLiterals() {
        return literals;
    }

    public List<Literal> getSortedLiterals() {
        return sortedLiterals;
    }


    @Override
    public String toString() {
        return sortedLiterals.stream().map(Literal::toString).reduce("", (x,y)-> x.isEmpty()? y: x+ " \u2228 " + y);
    }


    public Clause replace(Value value1, Value value2){

        return new Clause(literals.stream().map(literal -> literal.replace(value1, value2)).collect(Collectors.toSet()));
    }



    public Clause apply(Map<Variable, Value> substitution){

        return new Clause(literals.stream().map(l->l.apply(substitution)).collect(Collectors.toSet()), CollectionUtils.listOf(this));
    }

    public int getWeight() {
        return weight;
    }

    public Optional<List<Clause>> getParents() {
        return parents;
    }

    public Optional<ForwardClauseRule> getOriginRule() {
        return originRule;
    }

    public Clause refactor(){

        Set<Literal> tempLiterals = newSet();
        tempLiterals.addAll(literals);
        boolean changed = true;
        while(changed){

            Set<List<Literal>> literalPairs = binaryProduct(tempLiterals);
            changed = false;
            for(List<Literal> literalPair: literalPairs){

                Literal first = literalPair.get(0);
                Literal second = literalPair.get(1);

                if(!first.equals(second)){

                   Map<Variable, Value> theta = first.unify(second);

                    if(theta!=null){

                       // tempLiterals.remove(first);
                      //  tempLiterals.remove(second);
                     //   tempLiterals.add(first.apply(theta));

                        tempLiterals = tempLiterals.stream().map(x->x.apply(theta)).collect(Collectors.toSet());
                        changed = true;
                    }
                }



            }

        }
        return new Clause(tempLiterals);
    }


    public boolean subsumes(Clause other){

        if(this.sortedLiterals.size()>= other.sortedLiterals.size()){

            return false;
        }

        Map<Variable, Value> possibleAnswer = CollectionUtils.newMap();
        for(int i = 0; i < this.sortedLiterals.size(); i++){


            Literal myLiteral = sortedLiterals.get(i);
            Literal otherLiteral = other.sortedLiterals.get(i);

            Optional<Map<Variable, Value>> literalSubsumptionResultOpt = myLiteral.subsubmes(otherLiteral);

            if(!literalSubsumptionResultOpt.isPresent()){
                return false;
            }
            else {

                Map<Variable, Value> literalSubsumptionResult = literalSubsumptionResultOpt.get();

                Optional<Map<Variable, Value>> augmentationOpt  = Unifier.addTo(possibleAnswer, literalSubsumptionResult);

                if(!augmentationOpt.isPresent()){
                    return  false;
                }
                else {

                    possibleAnswer = augmentationOpt.get();
                }
            }


        }

        return true;

    }

    public Formula toFormula(){

        List<Formula> disjuncts = sortedLiterals.stream()
                .map(x-> x.isNegated()? new Not(x.getPredicate()):
                        x.getPredicate())
                .collect(Collectors.toList());
        if(disjuncts.size()==1){

            return  disjuncts.get(0);
        }
        return new Or(disjuncts);
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Clause clause = (Clause) o;

        return sortedLiterals.equals(clause.sortedLiterals);
    }


    @Override
    public int hashCode() {
        return sortedLiterals.hashCode();
    }
}
