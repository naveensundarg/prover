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
                } catch (Reader.ParsingException e) {
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
