package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.ccprovers.ColorShadowProver;
import com.naveensundarg.shadow.prover.core.ccprovers.SecondOrderCognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception {

        Prover prover = new SecondOrderCognitiveCalculusProver();


        Formula kp1  = Reader.readFormulaFromString("(exists [?P] (not (pos (exists [?x] (Knows! ?x (and ?P (not (exists [?y] (Knows! ?y ?P)))))))))");

        Formula inf_assumption= Reader.readFormulaFromString("(Knows! I (if PA (= 0 (multiply 27 0))))");

        Formula inf_goal = Reader.readFormulaFromString("(forall [?Q] (Knows! I (or (if PA (= 0 (multiply 27 0))) ?Q)) )");
        Justification justification = prover.prove(Sets.with(inf_assumption), inf_goal).get();

        System.out.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
        System.out.println(justification);


    }
}
