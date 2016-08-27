package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.representations.formula.Atom;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.Map;
import java.util.Set;

/**
 * Created by naveensundarg on 8/26/16.
 */
 class FastPropositionalClausalProver {

    public boolean prove(Set<Clause> clauses) {

        Map<Predicate, Integer> atomMap = countAtoms(clauses);

        int[][] matrix = new int[clauses.size()][atomMap.size()];

        //find all resolvable pairs that were not used before

        return false;
    }

    Map<Predicate, Integer> countAtoms(Set<Clause> clauses){

        Map<Predicate, Integer> map = CollectionUtils.newMap();
        int count = 0;
        for(Clause clause: clauses) {

            Set<Literal> literals = clause.getLiterals();

            for(Literal literal: literals) {

                if(!map.containsKey(literal.getPredicate())){

                    count = count + 1;
                    map.put(literal.getPredicate(), count);
                }
            }

        }

        return map;
    }
}
