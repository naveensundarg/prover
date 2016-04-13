package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.Converter;
import com.naveensundarg.shadow.prover.core.FirstOrderResolutionProver;
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

        Formula f1 = readFormula(read("(forall (?x) (if (man ?x) (mortal ?x)))"));
        Formula f2 = readFormula(read("(man socrates)"));
        Formula f3 = readFormula(read("(mortal socrates)"));

        System.out.println(Converter.convertToCNF(f3));

        FirstOrderResolutionProver firstOrderResolutionProver = new FirstOrderResolutionProver();



        Set<Formula> assumptions = Sets.newSet();

       assumptions.add(f1);
        assumptions.add(f2);

        System.out.println(firstOrderResolutionProver.prove(assumptions, f3));

//        System.out.println(Common.readCases(Sandbox.class.getResourceAsStream("propositional-completness-tests.clj")));


    }
}
