package com.naveensundarg.shadow.prover.core;

import clojure.lang.Compiler;
import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.representations.formula.Common;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Universal;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

/**
 * Created by naveensundarg on 12/10/16.
 */
class PreProcessor {



    public static Set<Formula> preProcess(Set<Formula> base){

        return base.stream().map(formula -> preProcess(base, formula)).reduce(Sets.newSet(), Sets::union);

    }
    private static Set<Formula> preProcess(Set<Formula> base, Formula f){

        return expandCommon(base, f);
    }

    private static Set<Formula> expandCommon(Set<Formula> base, Formula formula) {


        if(formula instanceof Common){
            Common common = (Common) formula;

            if(common.getFormula() instanceof Universal){

                Set<Formula> output = Sets.newSet();
                Universal universal = (Universal) common.getFormula();


                List<Set<Value>> smartValues = UniversalInstantiation.smartHints(universal, base);

                Set<List<Value>> substitutions = cartesianProduct(smartValues);
                Variable[] vars = universal.vars();

                Map<Variable, Value> mapping = CollectionUtils.newMap();
                substitutions.stream().forEach(substitution -> {

                            for (int i = 0; i < vars.length; i++) {

                                mapping.put(vars[i],substitution.get(vars.length-1-i));


                            }

                            Formula derived = universal.getArgument().apply(mapping);

                            if(!output.contains(derived)){
                                output.add(derived);
                            }

                        }

                );


                return output;
            }

        }


            return Sets.with(formula);


    }
}
