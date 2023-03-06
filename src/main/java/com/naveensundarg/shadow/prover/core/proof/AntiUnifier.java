package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;


public class AntiUnifier {

    static AtomicInteger FORMULA_VARIABLE_COUNTER = new AtomicInteger(0);
    static AtomicInteger OBJECT_VARIABLE_COUNTER = new AtomicInteger(0);

    public static Formula antiUnify(Formula f1, Formula f2){

        return antiUnify(f1, f2, CollectionUtils.newMap());
    }
    private static Formula antiUnify(Formula f1, Formula f2, Map<String, String> mapping) {

        if(f1.equals(f2)){

            return f1;
        } else {

            if(f1 instanceof Predicate && f2 instanceof Predicate) {
                Predicate p1 = (Predicate) f1;
                Predicate p2 = (Predicate) f2;

                String name1 = p1.getName();
                String name2 = p2.getName();

                Value[] args1 = p1.getArguments();
                Value[] args2 = p2.getArguments();

                if(args1.length== args2.length){

                    String newName = name1.equals(name2)? name1 : getNewFormulaVariable(mapping, name1, name2).toString();
                    if(!name1.equals(newName)){
                        mapping.put(name1, newName);

                    }
                    if(!name2.equals(newName)){
                        mapping.put(name2, newName);

                    }
                    Value[] antiUnifiedArgs = new Value[args1.length];
                    for(int i=0; i<args1.length; i++){
                        antiUnifiedArgs[i] = antiUnify(args1[i], args2[i], mapping);
                        mapping.put(args1[i].toString(), antiUnifiedArgs[i].toString());
                        mapping.put(args2[i].toString(), antiUnifiedArgs[i].toString());

                    }

                    return new Predicate(newName, antiUnifiedArgs);

                }



            }

            if(f1 instanceof And && f2 instanceof And){
                And and1 = (And) f1;
                And and2 = (And) f2;

                Formula[] args1 = and1.getArguments();
                Formula[] args2 = and2.getArguments();
                if(args1.length == args2.length){

                    int arity = and1.getArguments().length;
                    Formula[] antiUnified = new Formula[arity];

                    for(int i = 0; i<arity; i++){
                        antiUnified[i] = antiUnify(args1[i], args2[i], mapping);
                        mapping.put(args1[i].toString(), antiUnified[i].toString());
                        mapping.put(args2[i].toString(), antiUnified[i].toString());

                    }
                    return new And(antiUnified);
                }

            }
            if(f1 instanceof Or && f2 instanceof Or){
                Or or1 = (Or) f1;
                Or or2 = (Or) f2;

                Formula[] args1 = or1.getArguments();
                Formula[] args2 = or2.getArguments();
                if(args1.length == args2.length){

                    int arity = or1.getArguments().length;
                    Formula[] antiUnified = new Formula[arity];

                    for(int i = 0; i<arity; i++){
                        antiUnified[i] = antiUnify(args1[i], args2[i], mapping);
                        mapping.put(args1[i].toString(), antiUnified[i].toString());
                        mapping.put(args2[i].toString(), antiUnified[i].toString());

                    }
                    return new Or(antiUnified);
                }

            }
            if(f1 instanceof Exemplar && f2 instanceof Exemplar) {

                Exemplar e1 = (Exemplar) f1;
                Exemplar e2 = (Exemplar) f2;
                Formula au1 = antiUnify(e1.getInput(), e2.getInput(), mapping);
                Formula au2 = antiUnify(e1.getInput(), e2.getInput(), mapping);
                return new Exemplar(
                        au1, au2
                );
            }

            if (f1 instanceof Perception && f2 instanceof Perception){

                Perception p1 = (Perception) f1;
                Perception p2 = (Perception) f2;

                return new Perception(
                        antiUnify(p1.getAgent(), p2.getAgent(), mapping),
                        antiUnify(p1.getTime(), p2.getTime(), mapping),
                        antiUnify(p1.getFormula(), p2.getFormula(), mapping));
            }


            if (f1 instanceof Knowledge && f2 instanceof Knowledge){

                Knowledge k1 = (Knowledge) f1;
                Knowledge k2 = (Knowledge) f2;

                return new Knowledge(
                        antiUnify(k1.getAgent(), k2.getAgent(), mapping),
                        antiUnify(k1.getTime(), k2.getTime(), mapping),
                        antiUnify(k1.getFormula(), k2.getFormula(), mapping));
            }


        }
        return getNewFormulaVariable(mapping, null, null);

    }

    public static Value antiUnify(Value v1, Value v2, Map<String, String> mapping) {

        if(v1.equals(v2)){
            return v1;
        }


        if(v1 instanceof Compound && v2 instanceof Compound) {

            Compound c1 = (Compound) v1;
            Compound c2 = (Compound) v2;

            if(c1.getArguments().length==c2.getArguments().length){

                Value[] args1 = c1.getArguments();
                Value[] args2 = c2.getArguments();
                String name1 = c1.getName();
                String name2 = c2.getName();
                String newName = name1.equals(name2)? name1 : getNewVariable(mapping, name1, name2).getName();

                Value[] antiUnifiedArgs = new Value[c1.getArguments().length];
                for(int i=0; i<c1.getArguments().length; i++){
                    antiUnifiedArgs[i] = antiUnify(args1[i], args2[i], mapping);
                    mapping.put(args1[i].toString(), antiUnifiedArgs[i].toString());
                    mapping.put(args2[i].toString(), antiUnifiedArgs[i].toString());


                }
                return new Compound(newName, antiUnifiedArgs);
            }

        }

        return getNewVariable(mapping, v1.toString(), v2.toString());
    }
    private static  FormulaVariable getNewFormulaVariable(Map<String, String> mapping, String left, String right) {

        if(mapping.containsKey(left)){
            return  new FormulaVariable(mapping.get(left));
        }
        if(mapping.containsKey(right)){
            return  new FormulaVariable(mapping.get(right));
        }
        return new FormulaVariable("@!f" + FORMULA_VARIABLE_COUNTER.incrementAndGet() + "?");
    }
    private static Variable getNewVariable(Map<String, String> mapping, String left, String right) {
        if(mapping.containsKey(left)){
            return  new Variable(mapping.get(left));
        }
        if(mapping.containsKey(right)){
            return  new Variable(mapping.get(right));
        }
        return new Variable("!x" + OBJECT_VARIABLE_COUNTER.incrementAndGet() + "?");
    }

    public static void main(String[] args) throws Reader.ParsingException {
        Value v1 = Reader.readLogicValueFromString("a");
        Value v2 = Reader.readLogicValueFromString("b");

        Value c1 = Reader.readLogicValueFromString("(f a c)");
        Value c2 = Reader.readLogicValueFromString("(f b c)");
        Value c3 = Reader.readLogicValueFromString("(g b c)");

        Formula f1 = Reader.readFormulaFromString("(P b c)");
        Formula f2 = Reader.readFormulaFromString("(Q b c)");
        Formula f3 = Reader.readFormulaFromString("(Q a c)");


       /* System.out.println(antiUnify(v1, v1));
        System.out.println(antiUnify(v1, v2));
        System.out.println(antiUnify(c1, c2));
        System.out.println(antiUnify(c1, c3));

       */
       System.out.println(antiUnify(f1, f2));
        System.out.println(antiUnify(f1, f3));


        Formula example1 = Reader.readFormulaFromString("(e=> (Perceives! a (and (Red c1) (Red c2) ))\n" +
                "                                 (Perceives! a (exists [X] (and (X c1) (X c2)))))");

        Formula example2 = Reader.readFormulaFromString("(e=> (Perceives! a (and (Green c1) (Green c2) ))\n" +
                "                                 (Perceives! a (exists [X] (and (X c1) (X c2)))))");

        System.out.println(antiUnify(example1, example2));

    }
}
