package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.PropositionalResolutionProver;
import com.naveensundarg.shadow.prover.representations.Formula;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

import static com.naveensundarg.shadow.prover.utils.Reader.read;
import static com.naveensundarg.shadow.prover.utils.Reader.readFormula;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception{

        Formula f1 = readFormula(read("(P x)"));

        System.out.println(f1);
/*
        PropositionalResolutionProver propositionalResolutionProver = new PropositionalResolutionProver();



        Set<Formula> assumptions = Sets.newSet();

       // assumptions.add(f1);

        System.out.println(propositionalResolutionProver.prove(assumptions, f1));
*/

//        System.out.println(Common.readCases(Sandbox.class.getResourceAsStream("propositional-completness-tests.clj")));


    }
}
