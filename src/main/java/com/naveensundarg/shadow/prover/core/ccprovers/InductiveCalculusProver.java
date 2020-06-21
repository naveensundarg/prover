package com.naveensundarg.shadow.prover.core.ccprovers;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus.BreakupBiConditionals;
import com.naveensundarg.shadow.prover.core.expanders.inductivecalculus.Generalize;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Exemplar;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Logger;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class InductiveCalculusProver implements Prover {

    public SecondOrderCognitiveCalculusProver secondOrderCognitiveCalculusProver;
    private final List<Expander> expanders;
    private static int MAX_EXPAND_FACTOR = 100;
    protected Logger logger;

    public InductiveCalculusProver() {

        secondOrderCognitiveCalculusProver = new SecondOrderCognitiveCalculusProver();

        expanders = CollectionUtils.newEmptyList();

        expanders.add(Generalize.INSTANCE);

        logger = new Logger();

    }

    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        return prove(assumptions, formula, Sets.newSet());

    }

    public synchronized Optional<Justification> prove(Set<Formula> assumptions, Formula formula, Set<Formula> added) {

        Set<Formula> base = CollectionUtils.setFrom(assumptions);

        while (true) {
            int sizeBeforeExpansion = base.size();
            base = expand(base, added, formula);
            int sizeAfterExpansion = base.size();
            if (sizeAfterExpansion > MAX_EXPAND_FACTOR * assumptions.size()) {
                return Optional.empty();
            }
            if (sizeAfterExpansion <= sizeBeforeExpansion) {
                return Optional.empty();
            }

            Optional<Justification> optionalJustification = secondOrderCognitiveCalculusProver.prove(base, formula);
            if (optionalJustification.isPresent()) {
                return optionalJustification;
            }

        }


    }


    public Set<Formula> expand(Set<Formula> base, Set<Formula> added, Formula goal) {

        expanders.forEach(expander -> expander.expand(this, base, added, goal));

        return base;
    }
}
