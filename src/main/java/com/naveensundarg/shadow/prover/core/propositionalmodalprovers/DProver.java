package com.naveensundarg.shadow.prover.core.propositionalmodalprovers;

import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.sortsystem.SortSystem;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 1/1/17.
 */
public class DProver implements Prover {

    static Formula serial;

    static {

        try {
            serial = Reader.readFormulaFromString("(forall (?x) (exists (?y) (!R! ?x ?y)))");
        } catch (Reader.ParsingException e) {
            e.printStackTrace();
        }

    }


    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        Problem problem = new Problem("DPROVER", "DPROVER FOL CONVERSION", assumptions, formula);



        Map<Variable, Value> map = CollectionUtils.newMap();



        map.put(PropositionalModalConverter.WORLD, PropositionalModalConverter.W);

        Set<Formula> convertedAssumptions = assumptions.stream().map(x -> PropositionalModalConverter.convert(x, problem)).
                map(x->x.apply(map)).
                collect(Collectors.toSet());

        convertedAssumptions.add(serial);
        Formula convertedGoal = PropositionalModalConverter.convert(formula, problem).apply(map);


        Prover firstOrderHalo = SnarkWrapper.getInstance();

        return firstOrderHalo.prove(convertedAssumptions, convertedGoal);

    }


    @Override
    public Optional<Justification> prove(SortSystem sortSystem, Set<Formula> assumptions, Formula formula) {
        return null;
    }

    //Set<Formula> getAxioms();

}
