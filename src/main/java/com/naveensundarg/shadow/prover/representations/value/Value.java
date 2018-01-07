package com.naveensundarg.shadow.prover.representations.value;

import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.Expression;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Pair;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 4/11/16.
 */
public abstract class Value extends Expression implements  Comparable{

    protected String name;


    public  String getName(){
        return name;
    }
    public abstract int arity();
    public abstract Value[] getArguments();

    public abstract boolean isVariable();
    public abstract boolean isConstant();
    public abstract boolean isCompound();

    public boolean occurs(Variable x){
        return false;
    }

    public abstract Set<Variable> variablesPresent();

    public abstract Value apply(Map<Variable, Value> substitution);
    public abstract Value replace(Value value1, Value value2);
    public abstract Value generalize(Map<Value, Variable> substitution);

    public abstract Set<Value> subValues();

    public abstract int getWeight();

    public abstract Optional<Pair<Variable, Value>> subsumes(Value other);


    public static Optional<Map<Variable,Value>> subsumes(Value[] values1, Value[] values2){

        if(values1.length!=values2.length){

            return Optional.empty();

        } else {


            Map<Variable, Value> possibleAnswer = CollectionUtils.newMap();

            for(int i  = 0; i < values1.length; i++){

                Value v1 = values1[i];
                Value v2 = values2[i];

                Optional<Pair<Variable, Value>> thisSubsumed = v1.subsumes(v2);

                if(!thisSubsumed.isPresent()){

                    return Optional.empty();

                } else {


                    Variable variable = thisSubsumed.get().first();
                    Value value = thisSubsumed.get().second();


                    Optional<Map<Variable, Value>> augmentedOpt = Unifier.addTo(possibleAnswer, variable, value);

                    if(!augmentedOpt.isPresent()){

                        return augmentedOpt;

                    } else {

                        possibleAnswer.putAll(augmentedOpt.get());
                    }


                }

            }


            return Optional.of(possibleAnswer);



        }


    }

 }
