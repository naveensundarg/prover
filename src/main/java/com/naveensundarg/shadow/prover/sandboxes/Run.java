package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.io.FileInputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class Run {

    public static void main(String[] args) throws Exception {

        runFromProblemFile();
        runFromStringInputs();
    }

    public static void runFromStringInputs() throws Exception {


        Formula premise1 = Reader.readFormulaFromString("(forall (x) (if (Mortal x) (Man x)))");
        Formula premise2 = Reader.readFormulaFromString("(Man socrates)");


        Set<Formula> assumptions = new HashSet<>(CollectionUtils.listOf(premise1, premise2));

        Formula goal = Reader.readFormulaFromString("(Mortal socrates)");
        System.out.println("Running from with assumptions "+  assumptions  + " and goal: " + goal);


        Prover prover = SnarkWrapper.getInstance();


        Optional<Justification> justificationOptional = prover.prove(assumptions, goal);

        if(justificationOptional.isPresent()) {
            System.out.println("Success");
        } else {
            System.out.println("Failure");

        }

    }
    public static void runFromProblemFile() throws Exception {


        String fileName = "./example.clj";

        System.out.println("Running on problem file: " + fileName);

        List<Problem> problems = ProblemReader.readFrom(new FileInputStream(fileName));

        Problem firstProblem = problems.get(0);

        Prover prover = SnarkWrapper.getInstance();


        Optional<Justification> justificationOptional = prover.prove(firstProblem.getAssumptions(), firstProblem.getGoal());

        if(justificationOptional.isPresent()) {
            System.out.println("Success");
        } else {
            System.out.println("Failure");

        }

    }
}