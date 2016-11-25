package com.naveensundarg.shadow.prover.core.sortsystem;

import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.Reader;
import org.testng.annotations.Test;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import java.util.Map;

import static us.bpsm.edn.parser.Parsers.defaultConfiguration;

/**
 * Created by naveensundarg on 8/28/16.
 */
public class SortSystemTest {

    @Test
    public void test1(){

        String declarationString = " {\n" +
                "               :sorts {\n" +
                "                       Agent       #{Object}\n" +
                "                       Time        #{Object}\n" +
                "                       Proposition #{}\n" +
                "                       Person #{Agent}\n" +
                "                       }\n" +
                "\n" +
                "               :declarations\n" +
                "                      [\n" +
                "                       (mother (Person) -> Person)\n" +
                "                       (married (Person, Person) -> Person)\n" +
                "                       (a1 () -> Agent)\n" +
                "                       (jack () -> Person)\n" +

                "                       (t1 () -> Time)\n" +
                "                       (P () -> Proposition)\n" +
                "                       ]\n" +
                "               }";



        Parseable pbr = Parsers.newParseable(declarationString);
        Parser p = Parsers.newParser(defaultConfiguration());


        SortSystem sortSystem = SortSystem.buildFrom((Map<?,?>)p.nextValue(pbr));

        sortSystem.checkTypeOfArguments("a1", new Value[0], new Category("Agent"));

        Value[] values = new Value[1];
        values[0] = new Constant("jack");

        sortSystem.checkTypeOfArguments("mother", values, new Category("Agent"));

        Value[] values1 = new Value[1];

        values1[0] = new Compound("mother", values);


        sortSystem.checkTypeOfArguments("mother", values1, new Category("Person"));

    }
}
