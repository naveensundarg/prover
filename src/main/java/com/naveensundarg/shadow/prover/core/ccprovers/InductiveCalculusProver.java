package com.naveensundarg.shadow.prover.core.ccprovers;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus.*;
import com.naveensundarg.shadow.prover.core.expanders.inductivecalculus.AntiUnification;
import com.naveensundarg.shadow.prover.core.expanders.inductivecalculus.Generalize;
import com.naveensundarg.shadow.prover.core.expanders.inductivecalculus.ShallowImplication;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.*;
import com.naveensundarg.shadow.prover.representations.formula.Exemplar;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.method.ModusPonens;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Logger;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class InductiveCalculusProver implements Prover {

    public Prover prover;
    private final List<Expander> expanders;
    private static int MAX_EXPAND_FACTOR = 2000;
    protected Logger logger;

    public InductiveCalculusProver() {

        prover = SnarkWrapper.getInstance();

        expanders = CollectionUtils.newEmptyList();

        expanders.add(Generalize.INSTANCE);
        expanders.add(AntiUnification.INSTANCE);
        expanders.add(BreakupBiConditionals.INSTANCE);
        expanders.add(UniversalElim.INSTANCE);
        expanders.add(NotExistsToForallNot.INSTANCE);
        expanders.add(ShallowImplication.INSTANCE);

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

            Set<Formula> order1ModalAssumptions = base.stream().map(Logic::transformSecondOrderToFirstOrder).collect(Collectors.toSet());
            Formula order1Goal = Logic.transformSecondOrderToFirstOrder(formula);

            try {
                order1ModalAssumptions.add(Reader.readFormulaFromString("(forall [?x] (= (ARGS) (ARGS ?x)))"));
            } catch (Reader.ParsingException e) {
                e.printStackTrace();
            }

            Set<Formula> shadowedOrder1ModalAssumptions = shadow(order1ModalAssumptions, 1);
            Formula shadowedOrder1Goal = shadow(order1Goal, 1);
            if(shadowedOrder1ModalAssumptions.contains(shadowedOrder1Goal)){
                return Optional.of(new AtomicJustification("",
                        shadowedOrder1ModalAssumptions.stream().filter(f->f.equals(shadowedOrder1Goal)).findAny().get()
                        ));
            }
            Optional<Justification> optionalJustification = prover.prove(shadowedOrder1ModalAssumptions, shadowedOrder1Goal);
            if (optionalJustification.isPresent()) {
                return optionalJustification;
            }

        }


    }

    private static Formula shadow(Formula formula, int level){

        Formula shadowed = formula.shadow(level);
        shadowed.setJustificationLabelAndAncestors("shadowed(" + level + ")", CollectionUtils.listOf(formula));
        return shadowed;
    }


    public Set<Formula> expand(Set<Formula> base, Set<Formula> added, Formula goal) {

        expanders.forEach(expander -> expander.expand(this, base, added, goal));

        return base;
    }

    @Override
    public Logger getLogger() {
        return logger;
    }

    protected Set<Formula> shadow(Set<Formula> formulas, int level) {
        return formulas.stream().map(formula -> shadow(formula, level)).collect(Collectors.toSet());
    }
}
