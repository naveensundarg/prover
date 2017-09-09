package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;
import com.naveensundarg.shadow.prover.representations.formula.Or;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public final class ConstructiveDilemma extends PrimitiveMethod {

    private static final ConstructiveDilemma INSTANCE;

    static {
        INSTANCE = new ConstructiveDilemma();
    }

    private ConstructiveDilemma() {

    }

    public static ConstructiveDilemma getInstance() {

        return INSTANCE;
    }

    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if (args.size() == 3) {


            Phrase P1 = args.get(0);
            Phrase P2 = args.get(1);
            Phrase P3 = args.get(2);

            if(P1 instanceof Or){

                if(P2 instanceof Implication){

                    if(P3 instanceof Implication){

                        if(assumptionBase.contains(P1)){

                            if(assumptionBase.contains(P2)){

                                if(assumptionBase.contains(P3)){


                                    Or or = (Or) P1;
                                    Implication implication1 = (Implication) P2;
                                    Implication implication2 = (Implication) P3;

                                    if(or.getArguments().length==2 &&
                                       or.getArguments()[0].equals(implication1.getAntecedent()) &&
                                       or.getArguments()[1].equals(implication2.getAntecedent()) &&
                                       implication1.getConsequent().equals(implication2.getConsequent())){

                                        return implication1.getConsequent();
                                    }

                                    else{

                                           return new ErrorPhrase("constructive-dilemma: pattern matching failed");
                                    }

                                } else {

                                    return new ErrorPhrase("constructive-dilemma: assumption base does not the third formula: " + P2);

                                }



                            } else{

                                return new ErrorPhrase("constructive-dilemma: assumption base does not the second formula: " + P2);
                            }


                        } else{

                            return new ErrorPhrase("constructive-dilemma: assumption base does not contain the first formula: " + P1);
                        }

                    }

                    else {
                        return new ErrorPhrase("constructive-dilemma expects an implication as the third input, but got as the third argument " + P3 + " of class " + P3.getClass());

                    }

                } else{

                    return new ErrorPhrase("constructive-dilemma expects an implication as the second input, but got as the second argument " + P2 + " of class " + P2.getClass());

                }

            } else {

                return new ErrorPhrase("constructive-dilemma expects an Or as the first input, but got as the first argument " + P1 + " of class " + P1.getClass());
            }

        }
        {

            return new ErrorPhrase("constructive-dilemma takes exactly three inputs but got: " + args);
        }

    }

     @Override
    public String toString() {
        return "constructive-dilemma";
    }
}
