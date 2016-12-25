package com.naveensundarg.shadow.prover.representations.value;

/**
 * Created by naveensundarg on 11/24/16.
 */
public class Time extends Constant implements Comparable {

    int value;
    public Time(String name) {
        super(name);

        this.value = Integer.parseInt(name);
    }

    @Override
    public int compareTo(Object o) {
        if(o instanceof Time) {
            Time otherTime = (Time) o;
            return value - otherTime.value;

        } else {
            return 0;
        }
    }

    @Override
    public int getWeight() {
        return 1;
    }
}
