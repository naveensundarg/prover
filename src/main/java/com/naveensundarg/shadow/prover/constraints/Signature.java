package com.naveensundarg.shadow.prover.constraints;

import com.naveensundarg.shadow.prover.axiomsets.SimpleEventCalculus;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import static us.bpsm.edn.parser.Parsers.defaultConfiguration;

/**
 * Created by naveensundarg on 9/9/17.
 */
public class Signature implements Constraint {

    private final Map<String, Set<Integer>> arities;

    public Signature(Map<String, Set<Integer>> arities) {

        this.arities = arities;
    }

    public Signature(InputStream is) {

        try {

            Parseable pbr = Parsers.newParseable(new InputStreamReader(is));
            Parser p = Parsers.newParser(defaultConfiguration());

            Map<?, ?> map = (Map<?, ?>) p.nextValue(pbr);

            Map<String, Set<Integer>> arities = CollectionUtils.newMap();
            AtomicBoolean errorOccured = new AtomicBoolean(false);
            map.entrySet().forEach(entry -> {

                String name = entry.getKey().toString();

                List<?> vals = (List<?>) entry.getValue();


                Set<Integer> numbers = Sets.newSet();

                vals.forEach(x-> numbers.add(Integer.parseInt(x.toString())));

                arities.put(name, numbers);

            });


            if (errorOccured.get()) {

                throw new AssertionError("Could not read axiom set from file.");
            } else {

                this.arities = arities;
            }


        } catch (Exception e) {
            e.printStackTrace();
            throw new AssertionError("Could not read axiom set from file.");
        }

    }


    private Set<String> predicatesThatAreInCorrect(Formula formula) {

        return formula.subFormulae().stream().filter(sub -> sub instanceof Predicate).map(x -> (Predicate) x).filter(x -> {

            String name = x.getName();
            int arity = x.getArguments().length;

            return !(arities.containsKey(name) && arities.get(name).contains(arity));

        }).map(Predicate::toString).collect(Collectors.toSet());
    }

    private Set<String> functionsThatAreInCorrect(Formula formula) {

        return formula.valuesPresent().stream().filter(v -> !(v instanceof Variable)).filter(x -> {


            String name = x.getName();
            int arity = x.getArguments().length;

            return !(arities.containsKey(name) && arities.get(name).contains(arity));

        }).map(Object::toString).collect(Collectors.toSet());
    }

    @Override
    public boolean satisfies(Set<Formula> formulae) {


        Set<String> violations = formulae.stream().
                map(x->Sets.union(predicatesThatAreInCorrect(x),functionsThatAreInCorrect(x))).reduce(Sets.newSet(), Sets::union);

        if (!violations.isEmpty()) {

            throw new AssertionError(this.getClass() + " violated by: " + violations);
        }

        return true;

    }

    public static  void main(String[] args){

    }
}
