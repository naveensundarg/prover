package com.naveensundarg.shadow.prover.core.internals;

import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.BaseFormula;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Universal;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 12/1/16.
 */
public final class UniversalInstantiation {

    public static final List<Set<Value>> smartHints(Universal universal, Set<Formula> formulae) {

        List<Set<Value>> smartHints = CollectionUtils.newEmptyList();

        for(int i = 0; i<universal.vars().length; i++){
            smartHints.add(i, smartHintsForVariableAt(i, universal,formulae));
        }

        return smartHints;

    }
    public static final Set<Value> smartHintsForVariableAt(int position, Universal universal, Set<Formula> formulae){

        Variable firstVariable = universal.vars()[position];

        Set<BaseFormula> allBaseFormulae = Logic.baseFormulae(formulae);

        Set<BaseFormula> baseFormulaeContainingVariable = Logic.baseFormulae(universal).
                stream().
                filter(baseFormulae -> baseFormulae.allValues().
                        contains(firstVariable)).
                collect(Collectors.toSet());



        Set<Value>   allValues = baseFormulaeContainingVariable.stream().
                map(predicate -> {
                   Set<BaseFormula> matches = allBaseFormulae.stream().
                           filter(predicate1 -> predicate.getName().
                                   equals(predicate1.getName())).collect(Collectors.toSet());


                    Set<Value> values = matches.stream().
                            map(predicate1 -> {

                                Map<Variable, Value> map = Unifier.unify(predicate1, predicate);

                                        if(map==null){
                                            return null;
                                        } else {

                                          return  map.getOrDefault(firstVariable, null);
                                        }

                            }).
                            filter(Objects::nonNull).
                            collect(Collectors.toSet());
                    return values;

                }).reduce(Sets.newSet(),Sets::union);




        return allValues;

    }
}
