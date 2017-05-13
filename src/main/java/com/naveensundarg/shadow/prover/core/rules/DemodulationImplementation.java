package com.naveensundarg.shadow.prover.core.rules;

import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/15/16.
 */
public enum DemodulationImplementation implements ForwardClauseRule {

    INSTANCE;


    private boolean isEquals(Clause clause){

        return clause.getLiterals().size()==1 &&
                clause.getLiterals().stream().allMatch(x->x.getPredicate().getName().equals("="));

    }

    private Value getLeft(Clause clause){

        assert isEquals(clause);
        return clause.getLiterals().stream().map(Literal::getPredicate).map(x->x.getArguments()[0]).findAny().get();
    }

    private Value getRight(Clause clause){

        assert isEquals(clause);
        return clause.getLiterals().stream().map(Literal::getPredicate).map(x->x.getArguments()[1]).findAny().get();
    }

    @Override
    public Set<Clause> apply(Clause clause1, Clause clause2) {

        return Sets.union(applyInner(clause1, clause2), applyInner(clause2, clause1));

    }
    public Set<Clause> applyInner(Clause clause1, Clause clause2) {


        if(isEquals(clause1)){
            Value x = getLeft(clause1);
            Value y = getRight(clause1);

            Set<Map<Variable, Value>> subUnifications =

                    clause2.getLiterals().stream().
                            map(Literal::getPredicate).
                            map(Predicate::getArguments).
                            flatMap(Arrays::stream).
                            map(Z-> Unifier.subUnify(x, Z)).
                            filter(Objects::nonNull).
                            reduce(newSet(), Sets::union);


            return subUnifications.stream().map(theta-> clause2.replace(x.apply(theta), y.apply(theta))).collect(Collectors.toSet());

        } else {

            return newSet();
        }



    }


}
