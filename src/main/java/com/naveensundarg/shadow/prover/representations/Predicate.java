package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Set;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Predicate extends Formula {

    private final String name;
    private final Value[] arguments;
    private final Set<Formula> subFormulae;

    public Predicate(String name, Value[] arguments){
        this.name = name;
        this.arguments = arguments;
        this.subFormulae = Sets.with(this);
    }


    @Override
    public Set<Formula> subFormulae() {
        return subFormulae;
    }

    @Override
    public String toString() {
        return name + "(" + Arrays.stream(arguments).map(Value::toString).
                reduce("", (x,y)-> x.isEmpty()? y: x + ", " +y) +")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Predicate predicate = (Predicate) o;

        if (name != null ? !name.equals(predicate.name) : predicate.name != null) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(arguments, predicate.arguments)) return false;
        return subFormulae != null ? subFormulae.equals(predicate.subFormulae) : predicate.subFormulae == null;

    }

    @Override
    public int hashCode() {
        int result = name != null ? name.hashCode() : 0;
        result = 31 * result + Arrays.hashCode(arguments);
        result = 31 * result + (subFormulae != null ? subFormulae.hashCode() : 0);
        return result;
    }
}
