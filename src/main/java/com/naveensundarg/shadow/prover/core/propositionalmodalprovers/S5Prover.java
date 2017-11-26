package com.naveensundarg.shadow.prover.core.propositionalmodalprovers;

import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.sortsystem.SortSystem;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 1/1/17.
 */
public class S5Prover implements Prover {

    static Formula reflexivity, euclidean;

    static {

        try {
            reflexivity = Reader.readFormulaFromString("(forall (?x) (!R! ?x ?x))");
            euclidean = Reader.readFormulaFromString("(forall (?u ?t ?w) (iff (and (!R! ?w ?u) (!R! ?w ?t)) (!R! ?u ?t)))");
        } catch (Reader.ParsingException e) {
            e.printStackTrace();
        }

    }


    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        Problem problem = new Problem("S5-PROVER", "S5-PROVER FOL CONVERSION", assumptions, formula);


        Set<Formula> convertedAssumptions = assumptions.stream().map(x -> PropositionalModalConverter.convert(x, problem)).collect(Collectors.toSet());
        Formula convertedGoal = PropositionalModalConverter.convert(formula, problem);

        convertedAssumptions.add(reflexivity);
        convertedAssumptions.add(euclidean);

        Prover firstOrderHalo = SnarkWrapper.getInstance();

        return firstOrderHalo.prove(convertedAssumptions, convertedGoal);

    }


    @Override
    public Optional<Justification> prove(SortSystem sortSystem, Set<Formula> assumptions, Formula formula) {
        return null;
    }

    //Set<Formula> getAxioms();

}
