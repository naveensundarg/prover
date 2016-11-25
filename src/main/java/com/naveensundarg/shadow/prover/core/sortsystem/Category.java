package com.naveensundarg.shadow.prover.core.sortsystem;

/**
 * Created by naveensundarg on 8/27/16.
 */
final class Category {

    private final String name;

    protected Category(String name){
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Category category = (Category) o;

        return name.equals(category.name);

    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
