package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Constants;

import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public enum OughtSchema implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {

        Predicate<Formula> filterSelfOughts = f -> {

            if (!(f instanceof Belief)) {

                return false;
            }

            Belief  b        = (Belief) f;
            Formula believed = b.getFormula();
            if (!(believed instanceof Ought)) {
                return false;
            }
            Ought   ought        = (Ought) believed;
            Formula precondition = ought.getPrecondition();
            Value   outerAgent   = b.getAgent();
            Value   innerAgent   = ought.getAgent();
            return innerAgent.equals(outerAgent);
        };

        Set<Belief> obligationBeliefs =
                CommonUtils.level2FormulaeOfTypeWithConstraint(base,
                        Belief.class,
                        filterSelfOughts);

        if(!(prover instanceof CognitiveCalculusProver)){
            throw new AssertionError("Ought Schema should be used only in a cognitive calculi.");
        }

        for (Belief b : obligationBeliefs) {
            Ought                   ought                   = (Ought) b.getFormula();
            Formula                 precondition            = ought.getPrecondition();
            Value                   outerTime               = b.getTime();
            Value                   agent                   = b.getAgent();
            Belief                  preConditionBelief      = new Belief(agent, outerTime, precondition);
            CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver( (CognitiveCalculusProver) prover);
            Set<Formula>            smaller                 = CollectionUtils.setFrom(base);
            smaller.remove(b);
            smaller = smaller.stream().filter(x -> !x.subFormulae().contains(ought)).collect(Collectors.toSet());
            Optional<Justification> preconditionBelievedOpt = cognitiveCalculusProver.prove(smaller, preConditionBelief);

            if (preconditionBelievedOpt.isPresent()) {
                Formula oughtAction = ought.getOught();
                //  Value action = new Compound("action", new Value[]{agent, actionType});
                //Formula happens = new Predicate("happens", new Value[]{action, ought.getTime()});
                added.add(oughtAction);
                base.add(oughtAction);

                base.add(new Intends(agent, outerTime, oughtAction));
                added.add(new Intends(agent, outerTime, oughtAction));
            }


        }

    }


}
