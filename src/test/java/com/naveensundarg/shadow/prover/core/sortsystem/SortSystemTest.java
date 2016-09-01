package com.naveensundarg.shadow.prover.core.sortsystem;

import com.naveensundarg.shadow.prover.representations.value.Value;
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

        String declarationString = "{\n" +
                "               :sorts {\n" +
                "                       Agent       #{Object}\n" +
                "                       Time        #{Object}\n" +
                "                       Proposition #{}\n" +
                "                       }\n" +
                "\n" +
                "               :declarations\n" +
                "                      [\n" +
                "                       (a1 () -> Agent)\n" +
                "                       (t1 () -> Time)\n" +
                "                       (P () -> Proposition)\n" +
                "                       ]\n" +
                "               }";



        Parseable pbr = Parsers.newParseable(declarationString);
        Parser p = Parsers.newParser(defaultConfiguration());


        SortSystem sortSystem = SortSystem.buildFrom((Map<?,?>)p.nextValue(pbr));

        sortSystem.checkTypeOfArguments("a1", new Value[0], new Category("Agent"));

        sortSystem.checkTypeOfArguments("a1", new Value[0], new Category("Object"));

    }
}
