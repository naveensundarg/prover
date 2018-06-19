package com.naveensundarg.shadow.prover.examples;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.ccprovers.AxiologyProver;
import com.naveensundarg.shadow.prover.core.ccprovers.InductiveCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderProver;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class VirtueLearning {


    private static ColoredPrinter coloredPrinter = new ColoredPrinter.Builder(1, false)
            .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.BLACK)   //setting format
            .build();
    public static void main(String[] args) throws Exception {






        Prover prover = new InductiveCognitiveCalculusProver();

        Problem problem = ProblemReader.readFrom(VirtueLearning.class.getResourceAsStream("../experiments/virtuelearning.clj")).get(0);
        System.out.println(prover.prove(problem.getAssumptions(), problem.getGoal()));


    }

}
