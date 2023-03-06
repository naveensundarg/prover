package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.InductiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.proof.AtomicJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.MutableGraph;

import java.io.File;
import java.io.FileInputStream;
import java.util.List;
import java.util.Optional;

import static guru.nidi.graphviz.model.Factory.mutGraph;

public class OnlyLogicCanSaveUs {

    public static void main(String[] args) throws Exception{


        Prover prover =  new CognitiveCalculusProver();

        List<Problem> problems = ProblemReader.readFrom(new FileInputStream("./examples/onlylogiccansaveus/mudd1.clj"));

        for (Problem problem: problems){

            System.out.println("=============================");

            System.out.println(problem.getName());

            Optional<Justification> justificationOptional = (prover.prove((problem.getAssumptions()), problem.getGoal()));

            if(justificationOptional.isPresent()) {

                Justification justification = justificationOptional.get();
                System.out.println(justification);

          /*      if(justification instanceof AtomicJustification){
                    MutableGraph g = mutGraph("example1").
                            setDirected(true);

                    Formula current = ((AtomicJustification) justification).getInputs()[0];
                    Graphviz.fromGraph(g).width(5000).render(Format.PNG).toFile(new File("example/ex1m.png"));

                }*/
            }
        }


    }

}
