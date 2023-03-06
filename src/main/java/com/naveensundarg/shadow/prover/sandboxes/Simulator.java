package com.naveensundarg.shadow.prover.sandboxes;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.InductiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.proof.AtomicJustification;
import com.naveensundarg.shadow.prover.core.proof.HigherOrderUnification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.attribute.For;
import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.attribute.Shape;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Factory;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.MutableGraph;
import guru.nidi.graphviz.model.MutableNode;

import java.io.File;
import java.io.FileInputStream;
import java.util.List;
import java.util.Optional;

import static guru.nidi.graphviz.model.Factory.*;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Simulator {


    public static void main1(String[] args) throws Exception {


       /* Prover prover =  new CognitiveCalculusProver();

        Problem problem = ProblemReader.readFrom(new FileInputStream("./example.clj")).get(0);

        System.out.println(prover.prove((problem.getAssumptions()), problem.getGoal()));*/

/*
       Formula exemplar = Reader.readFormulaFromString("(e=> (or P (not P))\n" +
               "                        (Common! now (if (Knows! a P) (Believes! a P))))");
*/
      /*  Formula f1 = Reader.readFormulaFromString(" (Knows! a now Q)");

        InductiveCalculusProver inductiveCalculusProver = new InductiveCalculusProver();


        System.out.println(inductiveCalculusProver.prove(Sets.from(exemplar, f1),
                Reader.readFormulaFromString("(Believes! a now Q)")));


*/

        //System.out.println(Reader.readLogicValueFromString("(= \"hyperlog\" (clojure.string/join \"\" (reverse (reverse  \"hyperlog\"))))"));

        System.out.println(Reader.readFormulaFromString("(and A )"));
    }

    public static void main2(String[] args) throws Reader.ParsingException {
        Formula f1 = Reader.readFormulaFromString("(e=> (Perceives! a (and (R? c1) (R? c2) ))\n" +
                "                           (Perceives! a (exists [X] (and (X c1) (X c2)))))");

        Formula f2 = Reader.readFormulaFromString("(e=> (Perceives! a (and (Q c1) (Q c2) ))\n" +
                "                           (Perceives! a (exists [X] (and (X c1) (X c2)))))");

        System.out.println(Unifier.unifyFormula(f1, f2));

    }

    private static MutableNode addToGraph(Formula formula, MutableGraph g) {

        String nodeText = formula.toString();
        MutableNode node = mutNode(nodeText);
        g.add(node);
        String justificationLabel = formula.getJustificationLabel();
        MutableNode inferenceNode = mutNode(justificationLabel != null ? justificationLabel : "assume");
        node.addLink(inferenceNode);
        inferenceNode.add(Color.DARKGREEN);
        inferenceNode.add(Shape.BOX);
        List<Formula> ancestors = formula.getAncestors();
        if (ancestors != null) {
            for (Formula ancestor : ancestors) {
                if (ancestor != null && !ancestor.equals(formula)) {
                    MutableNode ancestorNode = addToGraph(ancestor, g);

                    inferenceNode.addLink(ancestorNode);
                    // g.linkTo(addToGraph(ancestor, g)).with(Label.of(justificationLabel));

                }
            }
        }


        return node;

    }

    public static void main3(String[] args) throws Exception {

/*        Optional<Justification> justificationOptional = main(args);

        if (justificationOptional.isPresent()) {

            Justification justification = justificationOptional.get();

            if (justification instanceof AtomicJustification) {
                MutableGraph g = mutGraph("example1").
                        setDirected(true);

                Formula current = ((AtomicJustification) justification).getInputs()[0];
                addToGraph(current, g);
                Graphviz.fromGraph(g).width(5000).render(Format.PNG).toFile(new File("example/ex1m.png"));

            }
        }*/

    }

    public static Optional<Justification> mainTD(String[] args) throws Exception {


        Prover prover = new SecondOrderCognitiveCalculusProver();

        Problem problem = ProblemReader.readFrom(new FileInputStream("src/main/resources/com/naveensundarg/shadow/prover/examples/peri_second_order_alml.clj")).get(0);

        Optional<Justification> opt = (prover.prove((problem.getAssumptions()), problem.getGoal()));

        System.out.println("");

//setting a format for all messages

        if (opt.isPresent()) {
            System.out.println("=========================");

            System.out.println(opt.get());
            System.out.println("=========================");

            ColoredPrinter cp = new ColoredPrinter.Builder(0, false)
                    .foreground(Ansi.FColor.NONE).background(Ansi.BColor.GREEN)
                    .build();
            cp.print("GOAL REACHED", Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.GREEN);
            System.out.println();
            System.out.println();
            System.out.println();
            System.out.println();

        } else {
            ColoredPrinter cp = new ColoredPrinter.Builder(0, false)
                    .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.RED)   //setting format
                    .build();
            cp.println("GOAL FAILED.");
            cp.clear();

            System.out.println();
            System.out.println();
            System.out.println();
            System.out.println();

        }

        return opt;

    }

    public static void main(String[] args) throws Exception {


        Prover prover = new SecondOrderCognitiveCalculusProver();


        Problem problem = ProblemReader.readFrom(new FileInputStream("src/main/resources/com/naveensundarg/shadow/prover/examples/alml_to_perception.clj")).get(0);

        Optional<Justification> opt = (prover.prove((problem.getAssumptions()), problem.getGoal()));

        System.out.println("");

//setting a format for all messages

        if (opt.isPresent()) {
            System.out.println("=========================");

            System.out.println(opt.get());
            System.out.println("=========================");

            ColoredPrinter cp = new ColoredPrinter.Builder(0, false)
                    .foreground(Ansi.FColor.NONE).background(Ansi.BColor.GREEN)
                    .build();
            cp.print("GOAL REACHED", Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.GREEN);
            System.out.println();
            System.out.println();
            System.out.println();
            System.out.println();

        } else {
            ColoredPrinter cp = new ColoredPrinter.Builder(0, false)
                    .foreground(Ansi.FColor.RED).background(Ansi.BColor.BLUE)   //setting format
                    .build();
            cp.println("GOAL FAILED.");
            cp.clear();

            System.out.println();
            System.out.println();
            System.out.println();
            System.out.println();

        }

       // return opt;
    }


    public static Formula read(String f) throws Reader.ParsingException {

        return Reader.readFormulaFromString(f);
    }

    public static void mainold(String[] args) throws Exception {

        Formula f1 = Reader.readFormulaFromString("(F a)");
        Formula f2 = Reader.readFormulaFromString("(Q a)");
    System.out.println(HigherOrderUnification.unify(f1, f2, Sets.from(new Variable("F"))));

    }

}
