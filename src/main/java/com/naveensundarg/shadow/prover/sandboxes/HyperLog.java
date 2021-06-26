package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.hyperlog.Evaluator;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.NamedLambda;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

public class HyperLog {

    public static void main(String[] args) throws Reader.ParsingException {

        NamedLambda def = (NamedLambda) Reader.readFormulaFromString("(defn firstifeight [n m] (if (= m 8) n 0))");
        Formula f1 = Reader.readFormulaFromString("(exists [x] (O x))");
        Formula f2 = Reader.readFormulaFromString("(forall [x] (O x))");

        Object answer = (Evaluator.evaluate(Sets.with(def), Reader.readLogicValueFromString("(firstifeight 8 4)")));

        System.out.println(answer);
    }
}
