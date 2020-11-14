package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Reader;

public class HyperLog {

    public static void main(String[] args) throws Reader.ParsingException {

        Formula exemplar = Reader.readFormulaFromString("(defn parent [x]  (+ 1 x))");
        Formula f1 = Reader.readFormulaFromString(" (Knows! a now Q)");

    }
}
