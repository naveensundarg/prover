package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.axiomsets.SimpleEventCalculus;
import com.naveensundarg.shadow.prover.axiomsets.Telephone;
import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
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

    public static void main1(String[] args) throws Exception {

        Prover prover = new SecondOrderCognitiveCalculusProver();


        Formula kp1  = Reader.readFormulaFromString("(exists [?P] (not (pos (exists [?x] (Knows! ?x (and ?P (not (exists [?y] (Knows! ?y ?P)))))))))");

        Formula inf_assumption= Reader.readFormulaFromString("(Knows! I (if PA (= 0 (multiply 27 0))))");

        Formula inf_goal = Reader.readFormulaFromString("(forall [?Q] (Knows! I (or (if PA (= 0 (multiply 27 0))) ?Q)) )");
        Justification justification = prover.prove(Sets.with(inf_assumption), inf_goal).get();

        System.out.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
        System.out.println(justification);


    }

    public static void main(String[] args) throws Exception {

        Prover prover = new CognitiveCalculusProver();


        Formula P1  = Reader.readFormulaFromString("(not P)");

        Formula P2  = Reader.readFormulaFromString("(if P Q)");

        Formula G1  = Reader.readFormulaFromString("(=> P Q)");
        Formula G2  = Reader.readFormulaFromString("(=> P R)");

        Formula I1  = Reader.readFormulaFromString("(if P Q)");
        Formula I2  = Reader.readFormulaFromString("(if P R)");

        Set<Formula> formulas = CollectionUtils.newEmptySet();
        formulas.add(P1);
        formulas.add(P2);

        Set<Formula> SCAxioms = SimpleEventCalculus.INSTANCE.get();

        Set<Formula> TelephoneAxioms = Telephone.INSTANCE.get();

        System.out.println(SCAxioms);

        System.out.println(prover.prove(formulas, G1));
        System.out.println(prover.prove(formulas, G2));

        System.out.println(prover.prove(formulas, I1));
        System.out.println(prover.prove(formulas, I2));



    }
}
