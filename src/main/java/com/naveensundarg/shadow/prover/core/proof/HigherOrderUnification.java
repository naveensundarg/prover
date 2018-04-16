package com.naveensundarg.shadow.prover.core.proof;

import clojure.lang.Var;
import com.naveensundarg.shadow.prover.representations.Expression;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import javax.swing.text.html.Option;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public final class HigherOrderUnification {


    private static boolean agreesWith(Map<Variable, Expression> bindings, Variable variable, Expression expression) {


        if (bindings.containsKey(variable)) {

            Expression priorValue = bindings.get(variable);

            return priorValue.equals(expression);

        } else {

            return true;
        }

    }

    private static boolean agreesWith(Map<Variable, Expression> bindings, Map<Variable, Value> newFirstOrderBinding) {

        for(Map.Entry<Variable, Value> entry: newFirstOrderBinding.entrySet()){

            Variable variable = entry.getKey();
            Value value = entry.getValue();
            if (bindings.containsKey(variable)) {

                Expression priorValue = bindings.get(variable);

                 if(!priorValue.equals(value)){

                     return false;
                 }
            }
        }

        return false;


    }

    private static Map<Variable, Value> firstOrderBindings(Map<Variable, Expression> bindings) {


        Map<Variable, Value> answer = CollectionUtils.newMap();

        bindings.forEach((variable, expression) -> {

            if(expression instanceof Value){

                answer.put(variable, (Value) expression);

            }
        });


        return answer;

    }

    public static Optional<Map<Variable, Expression>> unify(Expression e1, Expression e2){

        return unify(e1, e2, Sets.newSet(), Sets.newSet(), CollectionUtils.newMap());
    }

    public static Optional<Map<Variable, Expression>> unify(Expression e1, Expression e2,
                                                            Set<Variable> universalVars,
                                                            Set<Variable> existentialVars,
                                                            Map<Variable, Expression> bindings) {

        /*
         * Expression: Value  | Formula
         *
         */


        if (e1 instanceof Value && e2 instanceof Value) {

            if (e1 instanceof Variable || e2 instanceof Variable) {

                Variable variable;
                Expression expression;

                if (e1 instanceof Variable) {

                    variable = (Variable) e1;
                    expression = e2;

                } else {

                    variable = (Variable) e2;
                    expression = e1;
                }

                if (agreesWith(bindings, variable, expression)) {

                    bindings.put(variable, expression);
                    return Optional.of(bindings);

                } else {

                    return Optional.empty();
                }

            } else {


                Map<Variable, Value> valAnswers = Unifier.unify((Value) e1, (Value) e2, firstOrderBindings(bindings));


                if(valAnswers!=null){

                    //TODO: Check for agreement
                    if(!agreesWith(bindings, valAnswers)){

                        return  Optional.empty();

                    } else {

                        bindings.putAll(valAnswers);
                        return Optional.of(bindings);

                    }

                } else {

                    return  Optional.empty();
                }

            }


        }

        if (e1 instanceof Value && e2 instanceof Formula) {

            return matchValueFormula(e1, e2, bindings);

        }

        if (e1 instanceof Formula && e2 instanceof Value) {

            return matchValueFormula(e2, e1, bindings);

        }

        if (e1 instanceof Formula && e2 instanceof Formula) {


            if(e1 instanceof Predicate ) {



                return matchPredicateFormulae(e1, e2, universalVars, existentialVars, bindings);


            }
            if(e2 instanceof Predicate){

                return matchPredicateFormulae(e2, e1, universalVars, existentialVars, bindings);


            }

            if(e1 instanceof Not && e2 instanceof Not){

                Formula f1 = ((Not) e1).getArgument();

                Formula f2 = ((Not) e2).getArgument();

                return unify(f1, f2, universalVars, existentialVars, bindings);

            }

            if(e1 instanceof And && e2 instanceof And){


                Formula[] args1  = ((And) e1).getArguments();
                Formula[] args2  = ((And) e2).getArguments();


                return matchArgsList(args1, args2, universalVars, existentialVars, bindings);

            }

            if(e1 instanceof Or && e2 instanceof Or){

                Formula[] args1  = ((Or) e1).getArguments();
                Formula[] args2  = ((Or) e2).getArguments();

                return matchArgsList(args1, args2, universalVars, existentialVars, bindings);

            }

            if(e1 instanceof Implication && e2 instanceof Implication){

                Formula f1Ant = ((Implication) e1).getAntecedent();
                Formula f1Cons = ((Implication) e1).getConsequent();

                Formula f2Ant = ((Implication) e2).getAntecedent();
                Formula f2Cons = ((Implication) e2).getConsequent();


                Optional<Map<Variable, Expression>> part1 = unify(f1Ant, f2Ant, universalVars, existentialVars, bindings);

                if(part1.isPresent()){


                    return unify(f1Cons, f2Cons, universalVars, existentialVars, part1.get());

                } else {

                    return Optional.empty();
                }


            }

            if(e1 instanceof BiConditional && e2 instanceof BiConditional){

                Formula f1Left = ((BiConditional) e1).getLeft();
                Formula f1Right = ((BiConditional) e1).getRight();

                Formula f2Left = ((BiConditional) e2).getLeft();
                Formula f2Right = ((BiConditional) e2).getRight();


                Optional<Map<Variable, Expression>> part1 = unify(f1Left, f2Left, universalVars, existentialVars, bindings);

                if(part1.isPresent()){


                    return unify(f1Right, f2Right, universalVars, existentialVars, part1.get());

                } else {

                    return Optional.empty();
                }

            }

            if(e1 instanceof Universal && e2 instanceof Universal){

                Universal universal1 = (Universal) e1;
                Universal universal2 = (Universal) e2;


                if(!Arrays.equals(universal1.vars(), universal2.vars())){

                    return Optional.empty();

                }

                Formula arg1 = universal1.getArgument();
                Formula arg2 = universal2.getArgument();

                Optional<Map<Variable, Expression>> mapOpt = unify(arg1, arg2, Sets.union(universalVars, Sets.fromArray(universal1.vars())), existentialVars, bindings);

                if(!mapOpt.isPresent()){

                    return  Optional.empty();

                }

                Set<Variable> thisVars = Arrays.stream(universal1.vars()).collect(Collectors.toSet());

                Map<Variable, Expression> map = mapOpt.get();

                return Optional.of( map.entrySet().stream().filter(variableExpressionEntry -> !thisVars.contains(variableExpressionEntry.getKey())).collect(Collectors.toMap(
                        Map.Entry::getKey, Map.Entry::getValue)));

            }


            if(e1 instanceof Existential && e2 instanceof Existential){

                Existential existential1 = (Existential) e1;
                Existential existential2 = (Existential) e2;


                if(!Arrays.equals(existential1.vars(), existential2.vars())){

                    return Optional.empty();

                }

                Formula arg1 = existential1.getArgument();
                Formula arg2 = existential2.getArgument();

                Optional<Map<Variable, Expression>> mapOpt = unify(arg1, arg2, universalVars, Sets.union(existentialVars, Sets.fromArray(existential1.vars())), bindings);

                if(!mapOpt.isPresent()){

                    return  Optional.empty();

                }

                Set<Variable> thisVars = Arrays.stream(existential1.vars()).collect(Collectors.toSet());

                Map<Variable, Expression> map = mapOpt.get();

                return Optional.of( map.entrySet().stream().filter(variableExpressionEntry -> !thisVars.contains(variableExpressionEntry.getKey())).collect(Collectors.toMap(
                        Map.Entry::getKey, Map.Entry::getValue)));

            }




            return Optional.empty();



        }


        return Optional.empty();
    }


    private static Optional<Map<Variable, Expression>> matchValueFormula(Expression e1, Expression e2, Map<Variable, Expression> bindings){


        if(e1 instanceof Variable){

            Variable variable  = (Variable) e1;

            if(agreesWith(bindings, variable, e2)){

                bindings.put(variable, e2);

                return Optional.of(bindings);
            }

        }


        return Optional.empty();

    }

    private static Optional<Map<Variable, Expression>> matchArgsList(Formula[] args1, Formula[] args2, Set<Variable> universalVars, Set<Variable> existentialVars, Map<Variable, Expression> bindings){

        if(args1.length!=args2.length){
            return Optional.empty();
        }
        else {

            Map<Variable, Expression> currentBindings = bindings;

            for(int i = 0; i< args1.length; i++){


                Optional<Map<Variable, Expression>> thisAnswer = unify(args1[i], args2[i], universalVars, existentialVars, currentBindings);

                if(!thisAnswer.isPresent()){

                    return Optional.empty();
                }else {

                    currentBindings = thisAnswer.get();
                }


            }


            return Optional.of(currentBindings);
        }
    }

    private static Optional<Map<Variable, Expression>> matchPredicateFormulae(Expression e1, Expression e2, Set<Variable> universalVars, Set<Variable> existentialVars, Map<Variable, Expression> bindings){

        String name = ((Predicate) e1).getName();

        if(isVariable((Predicate) e1, universalVars, existentialVars)){

            Variable variable = new Variable(name);

            if(e2 instanceof Predicate){
                Value[] args1 = ((Predicate) e1).getArguments();
                Value[] args2 = ((Predicate) e2).getArguments();

                Map<Variable, Value> argBindings = Unifier.unify(args1, args2, firstOrderBindings(bindings));
                if(argBindings==null){

                    bindings.put(variable, e2);

                    return Optional.of(bindings);

                } else {

                    bindings.putAll(argBindings);

                    bindings.put(variable, new Constant(((Predicate) e2).getName()));

                    return Optional.of(bindings);
                }


            } else {

                if(agreesWith(bindings, variable, e2)){

                    bindings.put(variable, e2);
                    return Optional.of(bindings);


                }
        }

        } else if(e2 instanceof Predicate) {


            if(((Predicate) e2).getName().equals(((Predicate) e1).getName())){

                Map<Variable, Value> valAnswers = Unifier.unify(((Predicate) e1).getArguments(), ((Predicate) e2).getArguments(), firstOrderBindings(bindings));

                if(valAnswers==null){

                    return Optional.empty();

                } else {

                    bindings.putAll(valAnswers);
                    return Optional.of(bindings);
                }


            } else {

                return Optional.empty();
            }

        } else {

            return Optional.empty();
        }

        return Optional.empty();
    }

    private static boolean isVariable(Predicate predicate, Set<Variable> universalVars, Set<Variable> existentialVars){


        return  Sets.union(universalVars, existentialVars).stream().anyMatch(x-> x.getName().equals(predicate.getName()));

    }


}
