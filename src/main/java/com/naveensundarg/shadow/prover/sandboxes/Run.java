package com.naveensundarg.shadow.prover.sandboxes;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
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

     //   runFromStringInputs();
        runFromProblemFile();
    }

    public static void runFromStringInputs() throws Exception {


        Formula premise1 = Reader.readFormulaFromString("(forall (x) (if (Man x) (Mortal x)))");
        Formula premise2 = Reader.readFormulaFromString("(Man socrates)");


        Set<Formula> assumptions = new HashSet<>(CollectionUtils.listOf(premise1, premise2));

        Formula goal = Reader.readFormulaFromString("(Mortal socrates)");


        Prover prover = new CognitiveCalculusProver();

        System.out.println("Running from with assumptions " + assumptions + " and goal: " + goal);


        Optional<Justification> justificationOptional = prover.prove(assumptions, goal);

        if (justificationOptional.isPresent()) {
            System.out.println("Success");
        } else {
            System.out.println("Failure");

        }

    }

    static ColoredPrinter coloredPrinter = new ColoredPrinter.Builder(1, false)
            .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.BLACK)   //setting format
            .build();
    public static void runFromProblemFile() throws Exception {


        String fileName = "./example.clj";

       //% System.out.println("Running on problem file: " + fileName);

        List<Problem> problems = ProblemReader.readFrom(new FileInputStream(fileName));

        Problem firstProblem = problems.get(0);

        Prover prover = new CognitiveCalculusProver();


        Optional<Justification> justificationOptional = prover.prove(firstProblem.getAssumptions(), firstProblem.getGoal());

        if (justificationOptional.isPresent()) {
            coloredPrinter.print( "SUCCESS", Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.GREEN);
        } else {
            System.out.println("Failure");

        }

    }
}