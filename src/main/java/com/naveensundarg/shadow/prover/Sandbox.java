package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.internals.AgentSnapShot;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.*;

import java.util.List;
import java.util.Set;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception{



      Set<Formula> formulas = CollectionUtils.newEmptySet();



        Value agent = new Constant("a");
        Value time0 = new Constant("t0");;

        Value time1 = new Constant("t1");;
        Value time2  = new Constant("t2");;
        Value time3  = new Constant("t3");;

        Predicate p = new Predicate("P");
        Predicate q = new Predicate("Q");


        Knowledge k1 = new Knowledge(agent, time1, p);
        Knowledge k2 = new Knowledge(agent, time2, q);

        formulas.add(k1);
        formulas.add(k2);

        AgentSnapShot agentSnapShot = AgentSnapShot.from(formulas);


        System.out.println(agentSnapShot.allKnownByAgentTillTime(agent, time0));

    }

    private static String getAssumptionsMap(Set<Formula> formulas){


        String ans = "    {";
        int count = 1;

        for(Formula formula : formulas) {

            ans = ans + count + " " + formula + "\n     ";
            count++;
        }
        ans  = ans + "}";

        return ans;


    }
}
