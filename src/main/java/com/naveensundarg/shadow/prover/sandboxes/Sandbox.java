package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.proof.HigherOrderUnification;
import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

public class Sandbox {

    public static void main(String[] args) throws Reader.ParsingException {

        Formula a = Reader.readFormulaFromString("(if (X a) (X b)))");
        Formula b = Reader.readFormulaFromString("(if (Q a) (X b)))");
        Variable X = new Variable("X");

        System.out.println(HigherOrderUnification.unify(a, b, Sets.from(X), Sets.from(), CollectionUtils.newMap()));

    }
}
