package com.naveensundarg.shadow.prover.utils;

/**
 * Created by naveensundarg on 4/9/16.
 */
public class ImmutablePair<U,V> implements Pair{

    private final U first;
    private final V second;

    private ImmutablePair(U first, V second){
        this.first = first;
        this.second = second;
    }


    public static <U,V> ImmutablePair<U,V> from(U first, V second){
        return new ImmutablePair<>(first, second);
    }

    @Override
    public Object first() {
        return first;
    }

    @Override
    public Object second() {
        return second;
    }

    @Override
    public String toString() {
        return "Pair {" +
                "first=" + first +
                ", second=" + second +
                '}';
    }
}
