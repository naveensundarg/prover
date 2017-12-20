package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.dpl.Interpreter;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.deduction.MethodApplication;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.method.Claim;
import com.naveensundarg.shadow.prover.representations.method.ModusPonens;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.lang.reflect.Method;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public class DPLSandbox {

    public static void main(String[] args) throws Reader.ParsingException {

        Formula P = Reader.readFormulaFromString("P");
        Formula Q = Reader.readFormulaFromString("Q");
        Formula implication = (Formula) Reader.readPhraseFromString("(if P Q)");



        Phrase p =   Reader.readPhraseFromString("(assume* P Q :in (!both P Q))");

        Set<Formula> assumptionBase = Sets.newSet();


        System.out.println(assumptionBase +  "==>" + Interpreter.interpret(assumptionBase, p));

    }


}
