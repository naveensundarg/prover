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


    public static <U,V> Pair<U,V> from(U first, V second){
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ImmutablePair<?, ?> that = (ImmutablePair<?, ?>) o;

        if (!first.equals(that.first)) return false;
        return second.equals(that.second);

    }

    @Override
    public int hashCode() {
        int result = first.hashCode();
        result = 31 * result + second.hashCode();
        return result;
    }
}
