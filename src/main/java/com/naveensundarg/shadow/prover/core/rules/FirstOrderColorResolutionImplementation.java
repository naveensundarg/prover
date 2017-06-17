package com.naveensundarg.shadow.prover.core.rules;

import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.ImmutablePair;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newEmptyList;
import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

/**
 * Created by naveensundarg on 4/15/16.
 */
public enum FirstOrderColorResolutionImplementation implements ForwardClauseRule {

    INSTANCE;


    private Map<Variable, Value> matches(Literal literal1, Literal literal2) {


        boolean differ = (literal1.isNegated() ^ literal2.isNegated());

        if (differ) {

            Predicate p1 = literal1.getPredicate();
            Predicate p2 = literal2.getPredicate();



            return Unifier.unify(p1, p2);
        } else {
            return null;
        }

    }

    @Override
    public Set<Clause> apply(Clause clause1, Clause clause2) {
        Set<Literal> literals1 = clause1.getLiterals();
        Set<Literal> literals2 = clause2.getLiterals();

        List<Set<Literal>> pairs = newEmptyList();
        pairs.add(literals2);
        pairs.add(literals1);

        Set<List<Literal>> pairsSet = cartesianProduct(pairs);

        Set<Pair<List<Literal>, Map<Variable, Value>>> matches =
                pairsSet.stream().map(pair -> {
                    Literal left = pair.get(0);
                    Literal right = pair.get(1);

                    return ImmutablePair.from(pair, matches(left, right));
                }).filter(x -> x.second() != null).collect(Collectors.toSet());

        return matches.stream().map(match -> {
            Set<Literal> l1 = Sets.remove(literals1, match.first().get(0));
            Set<Literal> l2 = Sets.remove(literals2, match.first().get(1));

            Set<Literal> literals = Sets.union(l1, l2);
            return (new Clause(literals, CollectionUtils.listOf(clause1, clause2))).apply(match.second());

        }).collect(Collectors.toSet());
    }


}
