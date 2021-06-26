package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Universal;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

public enum UniversalElimHigherOrder implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        UniversalElim universalElim = UniversalElim.INSTANCE;

        Set<Formula> order1ModalAssumptions = base.stream().map(Logic::transformSecondOrderToFirstOrder).collect(Collectors.toSet());
        Formula order1Goal = Logic.transformSecondOrderToFirstOrder(goal);

        universalElim.expand(prover, order1ModalAssumptions, added, order1Goal);

    }


}
