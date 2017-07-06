package com.naveensundarg.shadow.prover.core.x;

import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 6/25/17.
 */
public class NF extends Value{


    private final Set<Value> contents;

    public NF(Set<Value> contents) {
        this.contents = contents;
    }

    public void add(Value value){

        this.contents.add(value);

    }

    public static void main(String[] args) throws Reader.ParsingException {

        Value box = (Value) Reader.readLogicValueFromString("box");
        Set<Value> things  = new HashSet<>();
        things.add(box);

        NF nf = new NF(things);

        nf.add(nf);


        System.out.println(nf);



    }

    @Override
    public int arity() {
        return 0;
    }

    @Override
    public Value[] getArguments() {
        return new Value[0];
    }

    @Override
    public boolean isVariable() {
        return false;
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public boolean isCompound() {
        return false;
    }

    @Override
    public Set<Variable> variablesPresent() {
        return null;
    }

    @Override
    public Value apply(Map<Variable, Value> substitution) {
        return null;
    }

    @Override
    public Value replace(Value value1, Value value2) {
        return null;
    }

    @Override
    public Set<Value> subValues() {
        return null;
    }

    @Override
    public int getWeight() {
        return 0;
    }

    @Override
    public Optional<Pair<Variable, Value>> subsumes(Value other) {
        return null;
    }

    @Override
    public int compareTo(Object o) {
        return 0;
    }
}
