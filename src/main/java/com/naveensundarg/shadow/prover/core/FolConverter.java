package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.*;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.utils.Logic;

import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 4/12/16.
 */
public class FolConverter {

    public static Formula Step1_EliminateImplications(Formula formula, Problem problem){
        return Step1_EliminateImplicationsInt(formula);
    }
    private static Formula Step1_EliminateImplicationsInt(Formula formula){

        if(formula instanceof Predicate){
            return formula;
        }

        if(formula instanceof Not){
            return new Not(Step1_EliminateImplicationsInt(((Not) formula).getArgument()));
        }

        if(formula instanceof Or){
            Or or = (Or) formula;
            return new Or(Arrays.stream(or.getArguments()).
                            map(FolConverter::Step1_EliminateImplicationsInt).collect(Collectors.toList()));
        }

        if(formula instanceof And){
            And and = (And) formula;
            return new And(Arrays.stream(and.getArguments()).
                    map(FolConverter::Step1_EliminateImplicationsInt).collect(Collectors.toList()));
        }

        if(formula instanceof Universal){
            Universal universal = (Universal) formula;

            return  new Universal(universal.vars(), Step1_EliminateImplicationsInt(universal.getArgument()));
        }

        if(formula instanceof Existential){
            Existential existential = (Existential) formula;

            return  new Existential(existential.vars(), Step1_EliminateImplicationsInt(existential.getArgument()));
        }

        if(formula instanceof Implication){
            Implication implication = (Implication) formula;

            return  new Or(Logic.negated(Step1_EliminateImplicationsInt(implication.getAntecedent())), Step1_EliminateImplicationsInt(implication.getConsequent()));
        }

        if(formula instanceof BiConditional){
            BiConditional biConditional = (BiConditional) formula;


            return new And(new Or(Step1_EliminateImplicationsInt(Logic.negated(biConditional.getLeft())), Step1_EliminateImplicationsInt( biConditional.getRight())),
                    new Or(Step1_EliminateImplicationsInt(Logic.negated(biConditional.getRight())), Step1_EliminateImplicationsInt(biConditional.getLeft())));
        }
        else{
            throw new AssertionError("Unknown formula type");
        }

    }

    public static Formula Step2_MoveNegationsInWard(Formula formula, Problem problem){
        return Step2_MoveNegationsInWardInt(formula);
    }

    private static Formula Step2_MoveNegationsInWardInt(Formula formula) {



        if(formula instanceof Predicate){
            return formula;
        }

        if(formula instanceof Not){

            Not not = (Not) formula;
            Formula notArg =  not.getArgument();

            if(notArg instanceof  Predicate){
                return formula;
            }
            if(notArg instanceof Not){

                return Step2_MoveNegationsInWardInt(((Not) notArg).getArgument());
            }

            if(notArg instanceof Or){

                Or or = (Or) notArg;

                And and = new And(Arrays.stream(or.getArguments()).map(Logic::negated).
                        map(FolConverter::Step2_MoveNegationsInWardInt).collect(Collectors.toList()));
                return and;
            }

            if(notArg instanceof And){

                And and = (And) notArg;

                Or or = new Or(Arrays.stream(and.getArguments()).
                        map(Logic::negated).map(FolConverter::Step2_MoveNegationsInWardInt).collect(Collectors.toList()));
                return or;
            }

            if(notArg instanceof Existential){

                Existential existential = (Existential) notArg;


                Universal universal = new Universal(existential.vars(), Step2_MoveNegationsInWardInt(Logic.negated(existential.getArgument())));

                return universal;

            }

            if(notArg instanceof Universal){

                Universal universal = (Universal) notArg;

                Existential existential = new Existential(universal.vars(), Step2_MoveNegationsInWardInt(Logic.negated(universal.getArgument())));

                return existential;

            }


        }

        if(formula instanceof Or){
            Or or = (Or) formula;
            return new Or(Arrays.stream(or.getArguments()).
                    map(FolConverter::Step2_MoveNegationsInWardInt).collect(Collectors.toList()));
        }

        if(formula instanceof And){
            And and = (And) formula;
            return new And(Arrays.stream(and.getArguments()).
                    map(FolConverter::Step2_MoveNegationsInWardInt).collect(Collectors.toList()));
        }

        if(formula instanceof Universal){
            Universal universal = (Universal) formula;

            return  new Universal(universal.vars(), Step2_MoveNegationsInWardInt(universal.getArgument()));
        }

        if(formula instanceof Existential){
            Existential existential = (Existential) formula;

            return  new Existential(existential.vars(), Step2_MoveNegationsInWardInt(existential.getArgument()));
        }


        else{
            throw new AssertionError("Unknown formula type: " +formula);
        }

    }

    public static Formula Step3_StandardizeApart(Formula formula, Problem problem){
       return Converter.standardizeApart(formula, problem);
    }


    public static Formula preProcess(Formula formula, Problem problem){


        Formula f;
        f= Step1_EliminateImplications(formula, problem);
        f= Step2_MoveNegationsInWard(f, problem);
        f= Step3_StandardizeApart(f, problem);


        return f;
    }
    public static Formula Step3_Skolemize(Formula formula, Problem problem) {

        return  Converter.skolemize(formula, problem);
    }

}
