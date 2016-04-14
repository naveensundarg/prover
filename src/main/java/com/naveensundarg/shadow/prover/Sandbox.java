package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.Converter;
import com.naveensundarg.shadow.prover.core.FirstOrderResolutionProver;
import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.PropositionalResolutionProver;
import com.naveensundarg.shadow.prover.representations.Formula;
import com.naveensundarg.shadow.prover.utils.Common;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;
import java.util.Set;

import static com.naveensundarg.shadow.prover.utils.Reader.read;
import static com.naveensundarg.shadow.prover.utils.Reader.readFormula;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception{
        List<Pair<Set<Formula>, Formula>> tests = Common.readCases(Sandbox.class.getResourceAsStream("firstorder-completness-tests.clj"));

        Problem problem = new Problem();
        Formula f = readFormula(read("(exists (?x) (Q ?x))"));

        Formula f1 = readFormula(read("(and (exists (?x) (Q ?x)) (forall (?x) (or (P ?x) (not (P ?x)))))"));
        System.out.println( Converter.convertToCNF(f1,problem));

        Formula f2 = readFormula(read("(forall (?y) (if (human ?x) (or (man ?x) (woman ?x))))"));
        Formula f3 = readFormula(read("(human socrates)"));

        Formula f4 = readFormula(read("(mortal socrates)"));


        FirstOrderResolutionProver firstOrderResolutionProver = new FirstOrderResolutionProver();


        Set<Formula> assumptions = Sets.newSet();

        assumptions.add(f1);
        assumptions.add(f2);
        assumptions.add(f3);

        long start = System.currentTimeMillis();
        boolean answer = firstOrderResolutionProver.prove(assumptions, f4).isPresent();
        long end = System.currentTimeMillis();

        System.out.println(answer + ": "+(end-start) + " ms");

//        System.out.println(Common.readCases(Sandbox.class.getResourceAsStream("propositional-completness-tests.clj")));


    }
}
