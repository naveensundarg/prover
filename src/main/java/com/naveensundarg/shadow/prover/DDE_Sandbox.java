package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Or;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;
import com.sun.org.apache.regexp.internal.RE;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 2/20/17.
 */
public class DDE_Sandbox {

    public static void main(String[] args) throws Reader.ParsingException {

        List<Problem> tests = ProblemReader.readFrom(Sandbox.class.getResourceAsStream("dde_1_no_action.clj"));

        Problem test = tests.get(0);

        System.out.println(test);


        Prover prover = new CognitiveCalculusProver();

        Formula P1_Dead = Reader.readFormulaFromString("(exists [?t] (HoldsAt (dead P1) ?t))");
        System.out.println(prover.prove(test.getAssumptions(), P1_Dead));


        Formula P2_Dead = Reader.readFormulaFromString("(exists [?t] (HoldsAt (dead P2) ?t))");
        System.out.println(prover.prove(test.getAssumptions(), P2_Dead));


        Formula P3_Alive = Reader.readFormulaFromString("(forall [?t] (implies (Prior ?t 6) (not (HoldsAt (dead P3) ?t))))");
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), P3_Alive));


    }

    static Set<Formula> arithmetic() throws Reader.ParsingException {

        Set<Formula> formulas = CollectionUtils.newEmptySet();

        int max = 10;
        for(int i = 0; i<10; i++){
            for(int j = 0; j<10;j++){

                int ans = i + j;
                formulas.add(Reader.readFormulaFromString("(= " + ans + "( + " + i + " " + j + "))"));
                formulas.add(Reader.readFormulaFromString("(= " + ans + "( + " + j + " " + i + "))"));

                if(i<j){
                    formulas.add(Reader.readFormulaFromString("(Prior " + i + " " + j +")"));
                }
                if(j < i){
                    formulas.add(Reader.readFormulaFromString("(Prior " + j + " " + i +")"));


                }
            }
        }


         formulas.add(Reader.readFormulaFromString("(forall [?P] (if (Prior ?P 6) (or (= 0 ?P) (= 1 ?P) (= 2 ?P) (= 3 ?P) (= 4 ?P) (= 5 ?P))))"));

        return formulas;
    }
}
