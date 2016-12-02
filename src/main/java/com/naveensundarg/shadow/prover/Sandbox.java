package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.internals.AgentSnapShot;
import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.formula.Universal;
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


        Formula P1 = CommonUtils.readFromString("(forall (?x) (P (f ?x)))");
        Formula P2 = CommonUtils.readFromString("(P (f a))");

        System.out.println(UniversalInstantiation.smartHints((Universal)P1, Sets.with(P2)));
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
