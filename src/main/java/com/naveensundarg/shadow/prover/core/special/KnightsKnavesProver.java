package com.naveensundarg.shadow.prover.core.special;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Not;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.formula.Says;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 5/9/17.
 */
public class KnightsKnavesProver implements Prover {

    static SnarkWrapper snarkWrapper = new SnarkWrapper();
    boolean insideRefutation = false;
    @Override
    public synchronized Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        assumptions = CollectionUtils.setFrom(assumptions);

        Optional<Justification> base = snarkWrapper.prove(assumptions, formula);
        if (base.isPresent()) {
            return base;

        }

        int startSize, endSize = assumptions.size();

        do {

            startSize = assumptions.size();
            expand(assumptions);
            endSize = assumptions.size();

            base = snarkWrapper.prove(assumptions, formula);
            if (base.isPresent()) {
                return base;

            }

            try {
                if(!insideRefutation){
                    insideRefutation = true;
                    base = this.prove(Sets.add(assumptions, negate(formula)), Reader.readFormulaFromString("(and p (not p))"));
                    insideRefutation = false;
                }
            } catch (Reader.ParsingException e) {
                e.printStackTrace();
            }
            if (base.isPresent()) {
                return base;

            }

        } while (startSize != endSize);

        return Optional.empty();
    }


    private void expand(Set<Formula> assumptions) {


        Set<Formula> says = assumptions.stream().filter(x -> x.getClass().equals(Says.class)).map(x -> (Says) x).collect(Collectors.toSet());
        Set<Value> agents = Logic.allAgents(says);

        for (Formula formula : says) {

            Says sayFormula = (Says) formula;
            if (contains(assumptions, makeKnightFormula(sayFormula.getAgent())).isPresent()) {

                assumptions.add(sayFormula.getFormula());
            }

            if (contains(assumptions, new Not(makeKnightFormula(sayFormula.getAgent()))).isPresent()) {

                assumptions.add(new Not(sayFormula.getFormula()));
            }

            if (contains(assumptions, new Not(makeKnightFormula(sayFormula.getAgent()))).isPresent()) {

                assumptions.add(sayFormula.getFormula());
            }

            if (contains(assumptions, makeKnightFormula(sayFormula.getAgent())).isPresent()) {

                assumptions.add(new Not(sayFormula.getFormula()));
            }
        }

    }

    private static <T> Optional contains(Set<T> things, T a){
        if(things.contains(a)){
            return Optional.of(0);
        }

        return Optional.empty();
    }
    private Formula makeKnightFormula(Value agent) {
        return new Predicate("Knight", new Value[]{agent});
    }

    private Formula makeKnaveFormula(Value agent) {
        return new Predicate("Knave", new Value[]{agent});
    }

    private Formula negate(Formula f){

        if(f instanceof Predicate){
            if(((Predicate) f).getName().equals("Knave")){
                return makeKnightFormula(((Predicate) f).getArguments()[0]);
            }
            if(((Predicate) f).getName().equals("Knight")){
                return makeKnaveFormula(((Predicate) f).getArguments()[0]);
            }

                        return new Not(f);


        } else {

            return new Not(f);
        }
    }
}
