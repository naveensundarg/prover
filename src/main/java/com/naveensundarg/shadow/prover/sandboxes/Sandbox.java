package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.generators.GeneratorParams;
import com.naveensundarg.shadow.prover.generators.PropositionalProblemGenerator;
import com.naveensundarg.shadow.prover.generators.Vectorizer;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.*;

import java.util.List;
import java.util.Set;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main1(String[] args) throws Exception {

        Prover prover = new SecondOrderCognitiveCalculusProver();


        Formula kp1  = Reader.readFormulaFromString("(Knows! I now (forall [?x] (if (Agent ?x)   (or (= ?x I)   (= ?x P1)   (= ?x P2)   (= ?x P3)))))");

        Formula inf_assumption= Reader.readFormulaFromString("(Knows! I (if PA (= 0 (multiply 27 0))))");

        Formula inf_goal = Reader.readFormulaFromString("(forall [?Q] (Knows! I (or (if PA (= 0 (multiply 27 0))) ?Q)) )");
        Justification justification = prover.prove(Sets.with(inf_assumption), inf_goal).get();

        System.out.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
        System.out.println(justification);


    }

    public static void main(String[] args) throws Exception {

        generate("./train");
        generate("./test");


    }

    public static void generate(String name) throws Exception {


        GeneratorParams generatorParams = new GeneratorParams();
        generatorParams.maxAtoms = 4;
        generatorParams.clauses  = 4;
        generatorParams.maxLiteralsInClause = 4;


        PropositionalProblemGenerator propositionalProblemGenerator = new PropositionalProblemGenerator(generatorParams);

        long start = System.currentTimeMillis();
        System.out.println();

        List<Pair<List<Formula>, Boolean>> problems = propositionalProblemGenerator.generate(1000);

        long end = System.currentTimeMillis();

        long count = problems.stream().filter(Pair::second).count();

        Vectorizer vectorizer = new Vectorizer(generatorParams);

        vectorizer.vectorizePropositionalProblems(problems, name);

        System.out.println("Total Time: " + (end-start));
        System.out.println("Count: " + count);
    }

}
