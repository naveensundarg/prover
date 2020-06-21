package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.representations.formula.Universal;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Constants;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

public enum UniversalElim implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        //TODO: Less stupid elimination

        Set<Formula> formulae = CollectionUtils.setFrom(base);
        formulae.add(goal);

        Set<Universal> universals = base.stream().filter(f -> f instanceof Universal).map(f -> (Universal) f).collect(Collectors.toSet());


        universals.forEach(universal -> {


            List<Set<Value>> smartValues = UniversalInstantiation.smartHints(universal, formulae);

            Set<List<Value>> substitutions = cartesianProduct(smartValues);
            Variable[]       vars          = universal.vars();

            substitutions.forEach(substitution -> {
                Map<Variable, Value> mapping = CollectionUtils.newMap();

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


}
