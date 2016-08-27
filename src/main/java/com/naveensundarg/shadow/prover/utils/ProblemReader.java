package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.Sandbox;
import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import us.bpsm.edn.Keyword;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static us.bpsm.edn.parser.Parsers.defaultConfiguration;

/**
 * Created by naveensundarg on 7/23/16.
 */
public class ProblemReader {

    private static final Keyword ASSUMPTIONS_KEY = Keyword.newKeyword("assumptions");
    private static final Keyword GOAL_KEY = Keyword.newKeyword("goal");

    public static List<Problem> readFrom(InputStream inputStream) throws Reader.ParsingException {

        Parseable pbr = Parsers.newParseable(new InputStreamReader(inputStream));
        Parser p = Parsers.newParser(defaultConfiguration());


        List<Problem> problems = CollectionUtils.newEmptyList();

        Object problemDesc = (Map<?, ?> ) p.nextValue(pbr);

        while (problemDesc != Parser.END_OF_INPUT) {

            problems.add(buildProblem( (Map<?,?>) problemDesc));
            problemDesc = p.nextValue(pbr);
        }

        return problems;

    }


    private static Problem buildProblem(Map<?, ?> map) throws Reader.ParsingException {

        Set<Formula> assumptions = readAssumptions((Map<?, ?>) map.get(ASSUMPTIONS_KEY));
        Formula goal = Reader.readFormula(map.get(GOAL_KEY));

        return new Problem(assumptions, goal);
    }

    private static Set<Formula> readAssumptions(Map<?, ?> map) {



        return map.entrySet().stream().map(entry -> {
            try {

                return Reader.readFormula(entry.getValue());

            } catch (Reader.ParsingException e) {

                throw new AssertionError("Parsing Exception:" + e.getMessage());

            }

        }).collect(Collectors.toSet());

    }

    public static void main (String[] args) throws Reader.ParsingException {
        System.out.println(readFrom(Sandbox.class.getResourceAsStream("firstorder-completness-tests.clj")));
    }
}
