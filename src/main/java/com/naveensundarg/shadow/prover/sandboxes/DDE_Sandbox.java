package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

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
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), P1_Dead));


        Formula P2_Dead = Reader.readFormulaFromString("(exists [?t] (HoldsAt (dead P2) ?t))");
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), P2_Dead));


        Formula P3_Alive = Reader.readFormulaFromString("(forall [?t] (implies (Prior ?t 6) (not (HoldsAt (dead P3) ?t))))");
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), P3_Alive));


        Formula F1_S1 = Reader.readFormulaFromString("(not (Ought! I 2 situation (not (happens (action I (switch track1 track2)) 2))))");
        Formula F1_S2 = Reader.readFormulaFromString("(not (Ought! I 2 situation (not (Happens (action I (drop P3 track1 3   )) 1 ))))");

        long start, end;

        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), F1_S1));
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), F1_S1));
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), F1_S1));

        start = System.currentTimeMillis();
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), F1_S1));
        end = System.currentTimeMillis();

        long F1_S1_time = end-start;


        start = System.currentTimeMillis();
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), F1_S2));
        end = System.currentTimeMillis();

        long F1_S2_time = end-start;

        Formula F3a_1 = Reader.readFormulaFromString("(Intends! I now (not (HoldsAt (dead P1) 5)))");
        start = System.currentTimeMillis();
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), F3a_1));
        end = System.currentTimeMillis();

        long F3a_1_time = end-start;

        Formula F3a_2 = Reader.readFormulaFromString("(Intends! I now (not (HoldsAt (dead P2) 6)))");
        start = System.currentTimeMillis();
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), F3a_2));
        end = System.currentTimeMillis();

        long F3a_2_time = end-start;

        Formula F3b = Reader.readFormulaFromString("(Intends! I now (exists [?t] (HoldsAt (dead P3) ?t)))");
        start = System.currentTimeMillis();
        System.out.println(prover.prove(Sets.union(arithmetic(), test.getAssumptions()), F3b));
        end = System.currentTimeMillis();

        long F3b_time = end-start;

        System.out.println("=======================================");

        System.out.println("Time for F1_S1: " + F1_S1_time + "ms");

        System.out.println("Time for F1_S2: " + F1_S2_time + "ms");
        System.out.println("Time for F3a part 1: " + F3a_1_time + "ms");
        System.out.println("Time for F3a part 2: " + F3a_2_time + "ms");
        System.out.println("Time for F3b: " + F3b_time + "ms");

        ///// F3a

    }

    private static Set<Formula> arithmetic() throws Reader.ParsingException {

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
