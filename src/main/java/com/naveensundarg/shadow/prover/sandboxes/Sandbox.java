package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderProver;
import com.naveensundarg.shadow.prover.core.proof.HigherOrderUnification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;

import java.io.FileInputStream;
import java.net.URL;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {



    public static void main1(String[] args) throws Exception {


        List<Problem> tests = ProblemReader.readFrom(new FileInputStream("./src/main/resources/com/naveensundarg/shadow/prover/core/ccprovers/sandbox.clj"));


        Problem p = (tests.get(0));


        CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver();


        Optional<Justification> justificationOptional = (cognitiveCalculusProver.prove(p.getAssumptions(), p.getGoal()));



        if(justificationOptional.isPresent()){

            System.out.println("Proved!");
        } else {

            System.out.println("No!");

        }
    }


    public static void main(String[] args) throws Exception {


        SecondOrderProver secondOrderProver = new SecondOrderProver();
        Formula formula1 = Reader.readFormulaFromString("(forall x (= (+ x 0) x))");
        Formula formula2 = Reader.readFormulaFromString("(forall P (if (and (P 0) (forall x (if (P x) (P (s x))))) (forall x (P x))))");
        Formula formula3univ = Reader.readFormulaFromString("(forall Y (exists Z (forall x (iff (Z x) (Y  x)))))");

        Formula goal = Reader.readFormulaFromString("(exists Z (forall x (Z x)))");


        Formula temp = (Logic.getFalseFormula());


        Logic.transformSecondOrderToFirstOrderDeep(formula1);


        Sets.fromArray(new Formula[]{formula1, formula2, formula3univ, goal}).stream().map(Logic::transformSecondOrderToFirstOrderDeep).forEach(System.out::println);

        long start = System.currentTimeMillis();

        System.out.println(secondOrderProver.prove(Sets.fromArray(new Formula[]{formula1, formula2, formula3univ}), goal));


        long end = System.currentTimeMillis();

        System.out.println(end-start);

    }

    public static void mainl(String[] args) throws Exception {



         SnarkWrapper.getInstance();
    }

}
