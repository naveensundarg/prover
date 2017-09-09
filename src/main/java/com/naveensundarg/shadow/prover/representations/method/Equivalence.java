package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.BiConditional;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;
import com.naveensundarg.shadow.prover.representations.formula.Or;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public final class Equivalence extends PrimitiveMethod {

    private static final Equivalence INSTANCE;

    static {
        INSTANCE = new Equivalence();
    }

    private Equivalence() {

    }

    public static Equivalence getInstance() {

        return INSTANCE;
    }

    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if (args.size() == 2) {


            Phrase P1 = args.get(0);
            Phrase P2 = args.get(1);

            if (P1 instanceof Implication) {

                if (P2 instanceof Implication) {

                    if (assumptionBase.contains(P1)) {

                        if (assumptionBase.contains(P2)) {

                            Implication implication1 = (Implication) P1;
                            Implication implication2 = (Implication) P2;
                            //implication1 === A -> B
                            //implication2 === B --> A

                            if (implication1.getConsequent().equals(implication2.getAntecedent()) &&
                                implication1.getAntecedent().equals(implication2.getConsequent())) {

                                return new BiConditional(implication1.getAntecedent(), implication1.getConsequent());

                            }
                            {

                                return new ErrorPhrase("equivalence: assumption base not the third formula: " + P2);

                            }


                        } else {

                            return new ErrorPhrase("equivalence: assumption base does not the second formula: " + P2);
                        }


                    } else {

                        return new ErrorPhrase("equivalence: assumption base does not contain the first formula: " + P1);
                    }

                }

                else{

                    return new ErrorPhrase("equivalence expects an implication as the second input, but got as the third argument " + P2 + " of class " + P2.getClass());

                }


            } else {

                return new ErrorPhrase("equivalence expects an implication as the first input, but got as the second argument " + P1 + " of class " + P1.getClass());

            }


        }
        {

            return new ErrorPhrase("equivalence takes exactly two inputs but got: " + args);
        }

    }

    @Override
    public String toString() {
        return "equivalence";
    }
}
