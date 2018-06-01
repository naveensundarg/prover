package com.naveensundarg.shadow.prover.axiomsets;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import static us.bpsm.edn.parser.Parsers.defaultConfiguration;

/**
 * Created by naveensundarg on 9/9/17.
 */
public interface AxiomSet {


    AxiomSet getInstance();

    Set<Formula> get();


    static Set<Formula> getAxiomSetNamed(String name){

        if(name.equals("discrete-event-calculus")){

            return DiscreteEventCalculus.INSTANCE.get();
        }

        if(name.equals("simple-event-calculus")){

            return SimpleEventCalculus.INSTANCE.get();
        }
        if(name.equals("telephone")){

            return Telephone.INSTANCE.get();
        }

        if(name.equals("cognitive-telephone")){

            return CognitiveTelephone.INSTANCE.get();
        }

        else {

            throw new AssertionError("Unknown axiom set: "+ name);
        }

    }

     static Set<Formula> readFromFile(InputStream is){

        try {

            Parseable pbr = Parsers.newParseable(new InputStreamReader(is));
            Parser p = Parsers.newParser(defaultConfiguration());

            Map<?,?> map = (Map<?,?>)p.nextValue(pbr);

            Set<Formula> axioms = CollectionUtils.newEmptySet();
            AtomicBoolean errorOccured = new AtomicBoolean(false);
            map.values().forEach(value->{

                try {
                    axioms.add(Reader.readFormula(value));
                } catch (Exception e) {
                    e.printStackTrace();
                    errorOccured.set(true);
                }
            });


            if(errorOccured.get()){

                 throw new AssertionError("Could not read axiom set from file.");
            } else {

                return axioms;
            }


        } catch (Exception e) {
            e.printStackTrace();
            throw new AssertionError("Could not read axiom set from file.");
        }
    }
}
