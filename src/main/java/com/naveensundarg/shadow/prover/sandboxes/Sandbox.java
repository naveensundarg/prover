package com.naveensundarg.shadow.prover.sandboxes;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.ccprovers.*;
import com.naveensundarg.shadow.prover.core.propositionalmodalprovers.LP;
import com.naveensundarg.shadow.prover.core.propositionalmodalprovers.LP1;
import com.naveensundarg.shadow.prover.core.propositionalmodalprovers.LP2;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.*;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {



    public static void main(String[] args) throws Exception {




        Formula f1 = Reader.readFormulaFromString("(or q (and p (not p))))");

        Formula goal = Reader.readFormulaFromString("(pos (and p (not p)))");


        System.out.println(ModalConverter.convertToCNF(f1, new Problem("","", Sets.from(f1), goal)));
        Prover prover = new LP2();
        System.out.println(prover.prove(Sets.from(), goal).get());
    }




    private static ColoredPrinter coloredPrinter = new ColoredPrinter.Builder(1, false)
            .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.BLACK)   //setting format
            .build();
    public static void main2(String[] args) throws Exception {




        Formula f1 = Reader.readFormulaFromString("(forall (x y) (iff (= x y) (forall X (iff (X x) (X y)))))");
        Formula f2 = Reader.readFormulaFromString("(TwentyFive joan)");
        Formula f3 = Reader.readFormulaFromString("(Thirty John)");
        Formula f4 = Reader.readFormulaFromString("(forall x (if (TwentyFive x) (not (Thirty x))))");

        Formula goal = Reader.readFormulaFromString("(not (= joan john))");


        Prover prover = new SecondOrderProver();

        System.out.println(prover.prove(Sets.from(f1, f2, f3, f4), goal).get());

    }

}
