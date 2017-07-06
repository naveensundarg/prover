package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.special.ColorShadowProver;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception {

        Prover colorShadowProver = new ColorShadowProver();


        Formula f1 = Reader.readFormulaFromString("(Believes! a (forall [?x] (if (Man ?x) (Mortal ?x))))");
        Formula f2 = Reader.readFormulaFromString("(Believes! a (Man socrates))");
        Formula f3 = Reader.readFormulaFromString("(Believes! a (Man plato))");

        Formula f4 = Reader.readFormulaFromString("(Believes! a (Mortal ?y))");

        Variable y = (Variable) Reader.readLogicValueFromString("?y");

        Set<Formula> assumptions = CollectionUtils.newEmptySet();

        assumptions.add(f1);
        assumptions.add(f2);
        assumptions.add(f3);


        // Gets only one value for ?y
        System.out.println(colorShadowProver.proveAndGetBinding(assumptions,f4, y));

        // Gets multiple values for ?y
        System.out.println(colorShadowProver.proveAndGetMultipleBindings(assumptions,f4, CollectionUtils.listOf(y)));

    }
}
