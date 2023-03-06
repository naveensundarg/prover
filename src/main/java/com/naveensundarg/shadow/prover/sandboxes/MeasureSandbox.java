package com.naveensundarg.shadow.prover.sandboxes;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.InductiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.proof.AtomicJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.Exemplar;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.attribute.Shape;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.MutableGraph;
import guru.nidi.graphviz.model.MutableNode;
import org.json.JSONObject;

import java.io.*;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static guru.nidi.graphviz.model.Factory.mutGraph;
import static guru.nidi.graphviz.model.Factory.mutNode;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class MeasureSandbox {


    static InductiveCalculusProver inductiveCalculusProver = new InductiveCalculusProver();
    static CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver();

    public static void main(String[] args) throws Exception {
        run();
        run();
        run();
    }

    private static void run() throws Reader.ParsingException, FileNotFoundException {
        List<Problem> tests = ProblemReader.readFrom(new FileInputStream("/Users/naveensundar/projects/prover/src/main/resources/com/naveensundarg/shadow/prover/core/ccprovers/inductivecalculus-completness-tests.clj"));

        PrintWriter out = new PrintWriter("./results.csv");
        Set<Formula> allExamples = tests.stream().map(test->test.getAssumptions().stream().filter(x->x instanceof Exemplar).collect(Collectors.toSet())).reduce(Sets.newSet(), Sets::union);
        out.println("name,method,time");
        int i= 0;
        for(Problem problem: tests){
            long start = System.currentTimeMillis();
            inductiveCalculusProver.prove(Sets.union(problem.getAssumptions(),allExamples), problem.getGoal()).get();
            long end = System.currentTimeMillis();

            out.println(problem.getName() + "," + "inductive calculus prover," +  (end-start));
            i++;
            System.out.println(i);

        }
        i=0;
        for(Problem problem: tests){

            Set<Formula> assumptions = problem.getAssumptions().stream().filter(x-> !(x instanceof Exemplar)).collect(Collectors.toSet());
            long start = System.currentTimeMillis();
            System.out.println(problem.getName());


            Optional<Justification> just = cognitiveCalculusProver.prove(assumptions, problem.getGoal());
            long end = System.currentTimeMillis();
            if(just.isPresent()){
                out.println(problem.getName() + "," + "shadow prover," +  (end-start));

            } else {
                out.println(problem.getName() + "," + "shadow prover," +  "");

            }
            i++;
            System.out.println(i);


        }

        out.flush();
        out.close();
    }


}
