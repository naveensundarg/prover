package com.naveensundarg.shadow.prover.examples;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.ccprovers.AxiologyProver;
import com.naveensundarg.shadow.prover.core.ccprovers.InductiveCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Optional;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Example {


    private static ColoredPrinter coloredPrinter = new ColoredPrinter.Builder(1, false)
            .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.BLACK)   //setting format
            .build();



    public static void run(Prover prover, String path) throws Exception {




        Problem problem = ProblemReader.readFrom(VirtueLearning.class.getResourceAsStream(path)).get(0);

        System.out.println();
        coloredPrinter.println("===================================" , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);


        coloredPrinter.println("    \t"+ "STARTING STATE", Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.NONE);

        problem.getAssumptions().forEach(f-> {

            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            coloredPrinter.println("    \t"+ f, Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.NONE);
        });

        coloredPrinter.println("                                  " , Ansi.Attribute.UNDERLINE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
        Thread.sleep(1000);
        coloredPrinter.println(" \t Goal", Ansi.Attribute.BOLD, Ansi.FColor.MAGENTA, Ansi.BColor.NONE);
        Thread.sleep(1000);

        coloredPrinter.println("    \t"+ problem.getGoal(), Ansi.Attribute.BOLD, Ansi.FColor.GREEN, Ansi.BColor.NONE);
        coloredPrinter.println("===================================" , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);

        Thread.sleep(1000);

        Optional<Justification> proofOpt = (prover.prove(problem.getAssumptions(), problem.getGoal()));


        if(proofOpt.isPresent()){

            coloredPrinter.println("                                  " , Ansi.Attribute.UNDERLINE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
            Thread.sleep(500);
            coloredPrinter.println(" \t INFERRED", Ansi.Attribute.BOLD, Ansi.FColor.MAGENTA, Ansi.BColor.NONE);

            coloredPrinter.println("    \t"+ problem.getGoal(), Ansi.Attribute.BOLD, Ansi.FColor.BLUE, Ansi.BColor.NONE);
            coloredPrinter.println("===================================" , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);

        }

    }

}
