package com.naveensundarg.shadow.prover.dpl;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.deduction.*;
import com.naveensundarg.shadow.prover.representations.formula.And;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;
import com.naveensundarg.shadow.prover.representations.formula.Not;
import com.naveensundarg.shadow.prover.representations.method.DerivedMethod;
import com.naveensundarg.shadow.prover.representations.method.PrimitiveMethod;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 7/23/17.
 */
public final class Interpreter {


   public  static Phrase interpret(Set<Formula> assumptionBase, Phrase input){


       if(input instanceof MethodApplication){

           MethodApplication methodApplication = (MethodApplication) input;

           Phrase E = methodApplication.getE();
           List<Phrase> args = methodApplication.getArgs();

           Phrase V = interpret(assumptionBase, E);

           if(isAMethod(V)){

               List<Phrase> Vi = args.stream().map(arg-> interpret(assumptionBase, arg)).collect(Collectors.toList());

               Set<Formula> bp = CollectionUtils.newEmptySet();

               for(int i = 0; i < args.size(); i++){

                   if(args.get(i) instanceof Deduction){

                       if(!(Vi.get(i) instanceof ErrorPhrase)){
                           bp.add((Formula) (Vi.get(i)));
                       } else {

                           return Vi.get(i);
                       }

                   }
               }

               Set<Formula> assumptionBaseExtended = Sets.union(assumptionBase, bp);

               if(V instanceof PrimitiveMethod){

                   PrimitiveMethod primitiveMethod = (PrimitiveMethod) V;

                   return primitiveMethod.apply(assumptionBaseExtended, Vi);
               }

               if(V instanceof DerivedMethod){


               }


           }
           else {

               throw new AssertionError(E + " does not denote a method in " + methodApplication);
           }

       }

       if (input instanceof Assume){
           Assume assume = (Assume) input;
           Phrase E = assume.getAssumption();
           Deduction deduction = assume.getDeduction();

           Phrase Ein = interpret(assumptionBase, E);
           if(Ein instanceof Formula){
               Formula assumption = (Formula) Ein;
               Phrase Din = interpret(Sets.add(assumptionBase, assumption ), deduction);

               if(Din instanceof Formula){

                   return new Implication(assumption, (Formula) Din);

               } else {

                   if(Din instanceof ErrorPhrase){
                       return Din;
                   }
                   return new ErrorPhrase("Evaluation of assumption " + assume + " did not result in a formula: "+ Din);

               }
           }

       }
        if (input instanceof AssumeStar){
           AssumeStar assumeStar = (AssumeStar) input;
           List<Phrase> Es = assumeStar.getAssumption();
           Deduction deduction = assumeStar.getDeduction();

           List<Phrase> Eins = Es.stream().map(E->interpret(assumptionBase, E)).collect(Collectors.toList());
           Set<Formula> augmentedAssumptionBase = CollectionUtils.setFrom(assumptionBase);
            for (Phrase Ein : Eins) {
                if (Ein instanceof Formula) {
                    Formula assumption = (Formula) Ein;
                    augmentedAssumptionBase.add(assumption);
                }
                else{

                    return new ErrorPhrase("Evaluation of assume* assumption " + Ein + " did not result in a formula: ");
                }
            }
               Phrase Din = interpret(augmentedAssumptionBase, deduction);

               if(Din instanceof Formula){

                   return new Implication(new And(Eins.stream().map(x->(Formula) x).collect(Collectors.toList())), (Formula) Din);

               } else {

                   if(Din instanceof ErrorPhrase){
                       return Din;
                   }
                   return new ErrorPhrase("Evaluation of assumption " + assumeStar + " did not result in a formula: "+ Din);

               }


       }
        if (input instanceof SupposeAbsurd){

           SupposeAbsurd supposeAbsurd = (SupposeAbsurd) input;
           Phrase E = supposeAbsurd.getAssumption();
           Deduction deduction = supposeAbsurd.getDeduction();

           Phrase Ein = interpret(assumptionBase, E);

           if(Ein instanceof Formula){

               Formula assumption = (Formula) Ein;

               Phrase Din = interpret(Sets.add(assumptionBase, assumption ), deduction);

               if(Din.equals(Logic.getFalseFormula())){

                   return  new Not(assumption);

               } else {

                   if(Din instanceof ErrorPhrase){
                       return Din;
                   }
                   return new ErrorPhrase("Evaluation of suppose-absurd " + supposeAbsurd + " did not result in false: "+ Din);

               }
           }

       }

       if(input instanceof Formula || input instanceof Value || input instanceof PrimitiveMethod || input instanceof DerivedMethod ){
           return input;
       }



       throw new AssertionError("Could not interpret: " + input);

   }


   private static boolean isAMethod(Phrase phrase){

       return (phrase instanceof PrimitiveMethod) || (phrase instanceof DerivedMethod);
   }

}
