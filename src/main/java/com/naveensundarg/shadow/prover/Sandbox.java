package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Holder;
import com.naveensundarg.shadow.prover.utils.Pair;

import java.util.List;
import java.util.Set;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception{

        List<Pair<Set<Formula>, Formula>> cases = CommonUtils.readCases(Sandbox.class.getResourceAsStream("cognitivecalculus-completness-tests.clj"));

        Holder<Integer> i = Holder.holderWith(1);

        System.out.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n");

        String namePrefix = "*cognitive-calculus-completeness-test-";
        String nameSuffix = "*";
        cases.stream().forEach(c-> {



            System.out.println("{:name \"" +namePrefix + i.get() + nameSuffix + "\"" );
            System.out.println(" :description \"\"");

            System.out.println(" :assumptions ");

            System.out.println(getAssumptionsMap(c.first()));

            System.out.println(" :goal "+ c.second() +"}");
            System.out.println("");

            System.out.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n");
            i.mutateWith(x -> x +1);
        });

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
