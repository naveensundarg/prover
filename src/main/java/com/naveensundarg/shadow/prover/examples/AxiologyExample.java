package com.naveensundarg.shadow.prover.examples;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.ccprovers.AxiologyProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderProver;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class AxiologyExample {


    private static ColoredPrinter coloredPrinter = new ColoredPrinter.Builder(1, false)
            .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.BLACK)   //setting format
            .build();
    public static void main(String[] args) throws Exception {





        Formula f1 = Reader.readFormulaFromString("(Pref! (forall [?a ?t] (happens (action ?a (reading Ibsen)) ?t)) (happens (brush teeth) now))");
        Formula f2 = Reader.readFormulaFromString("(Indiff!  (happens (brush teeth) now))");
        Formula f3 = Reader.readFormulaFromString("(Pref!  (happens (brush teeth) now) (forall [?a ?t] (happens (action ?a (pay (tax ?a))) ?t)))");

        Formula t1 = Reader.readFormulaFromString("(Good! (forall [?a ?t] (happens (action ?a (reading Ibsen)) ?t))  )");
        Formula t2 = Reader.readFormulaFromString("(Bad! (forall [?a ?t] (happens (action ?a (pay (tax ?a))) ?t)) )");
        Formula t3 = Reader.readFormulaFromString("(O! (forall [?a ?t] (happens (action ?a (reading Ibsen)) ?t)) )");

        Prover prover = new AxiologyProver();

        System.out.println();
        coloredPrinter.println("===================================" , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);

        coloredPrinter.println(" \t Given Premises", Ansi.Attribute.BOLD, Ansi.FColor.BLUE, Ansi.BColor.NONE);

        coloredPrinter.println("    \t"+ "AXIOLOGY AXIOMS", Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.NONE);

        Sets.from(f1, f2, f3).forEach(f-> {


            coloredPrinter.println("    \t"+ f, Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.NONE);
        });

        coloredPrinter.println("                                  " , Ansi.Attribute.UNDERLINE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
         coloredPrinter.println(" \t Goal", Ansi.Attribute.BOLD, Ansi.FColor.MAGENTA, Ansi.BColor.NONE);

        coloredPrinter.println("    \t"+ t1, Ansi.Attribute.BOLD, Ansi.FColor.GREEN, Ansi.BColor.NONE);
        coloredPrinter.println("===================================" , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);

        long start = System.currentTimeMillis();
        prover.prove(Sets.from(f1, f2, f3), t1).ifPresent(proof->{

            coloredPrinter.print("Time taken: " + (System.currentTimeMillis() - start) + " (ms)", Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
            System.out.println();

            coloredPrinter.print("", Ansi.Attribute.REVERSE, Ansi.FColor.BLACK, Ansi.BColor.WHITE);
            coloredPrinter.print("-------- PROOF FOUND -------" , Ansi.Attribute.UNDERLINE, Ansi.FColor.BLACK, Ansi.BColor.WHITE);
            coloredPrinter.clear();
            System.out.println();

            System.out.println(proof);
        });

    }

}
