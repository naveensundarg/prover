package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ColoredConverter;
import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.special.ColorShadowProver;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception {

        SnarkWrapper snarkWrapper = new SnarkWrapper();



        Set<Formula> assumptions = Sets.newSet();

        Formula a1 = Reader.readFormulaFromString("(Believes! a P)");
        assumptions.add(a1);

        Formula a2 = Reader.readFormulaFromString("(if  (Believes! a (exists (x) (Happy x))) (Believes! a (Believes! b (forall (?x) (Happy ?x)))))");
       // assumptions.add(a2);


        Formula goal = Reader.readFormulaFromString("(or P (Believes! a P)) ");
        Problem problem = new Problem("", "", Sets.newSet(), a1);



        CognitiveCalculusProver prover = new CognitiveCalculusProver();


        System.out.println(prover.prove(assumptions, goal));


    }
}
