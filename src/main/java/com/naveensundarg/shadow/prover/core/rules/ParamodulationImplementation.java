package com.naveensundarg.shadow.prover.core.rules;

import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.ImmutablePair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/16/16.
 */
public enum  ParamodulationImplementation implements ForwardClauseRule {

    INSTANCE;

    private Set<Literal> getIdentityLiterals(Clause clause){

        return clause.getLiterals().stream().
                filter(literal->literal.getPredicate().getName().equals("=") && !literal.isNegated()).
                collect(Collectors.toSet());

    }

    private Value getLeft(Predicate predicate){

        return predicate.getArguments()[0];
    }

    private Value getRight(Predicate predicate){

        return predicate.getArguments()[1];
    }

    @Override
    public Set<Clause> apply(Clause clause1, Clause clause2) {

        return Sets.union(applyInner(clause1, clause2), applyInner(clause2, clause1));

    }


    private Set<Clause> applyInner(Clause clause1, Clause clause2) {

        Set<Literal> identityLiterals = getIdentityLiterals(clause1);

        Set<Clause> clauses = identityLiterals.stream().map(identityLiteral->{

            Value x = getLeft(identityLiteral.getPredicate());
            Value y = getRight(identityLiteral.getPredicate());


            Set<Map<Variable, Value>> subUnifications =

                    clause2.getLiterals().stream().
                            map(Literal::getPredicate).
                            map(Predicate::getArguments).
                            flatMap(Arrays::stream).
                            map(Z-> Unifier.subUnify(x, Z)).
                            filter(Objects::nonNull).
                            reduce(Sets.with(CollectionUtils.newMap()), Sets::union);

            Clause clause = new Clause(Sets.union(Sets.remove(clause1.getLiterals(),identityLiteral), clause2.getLiterals()), CollectionUtils.listOf(clause1, clause2));

            return subUnifications.stream().map(theta-> ImmutablePair.from(clause.apply(theta),theta)).
                    map(pair-> {
                        Clause cL = pair.first();
                        Map<Variable, Value> theta = pair.second();
                        return cL.replace(x.apply(theta), y.apply(theta));
                    }).collect(Collectors.toSet());

        }).reduce(newSet(), Sets::union);

        return  clauses;

    }
}

