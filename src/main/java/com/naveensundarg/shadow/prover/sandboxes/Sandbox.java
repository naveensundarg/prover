package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.*;

import java.io.FileInputStream;
import java.util.List;
import java.util.Optional;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {



    public static void main(String[] args) throws Exception {


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
}
