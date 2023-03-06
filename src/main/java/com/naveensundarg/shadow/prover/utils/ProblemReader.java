package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.axiomsets.AxiomSet;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.sandboxes.Simulator;
import com.naveensundarg.shadow.prover.core.sortsystem.SortSystem;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import org.apache.commons.lang3.NotImplementedException;
import us.bpsm.edn.Keyword;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static us.bpsm.edn.parser.Parsers.defaultConfiguration;

/**
 * Created by naveensundarg on 7/23/16.
 */
public class ProblemReader {

    private static final Keyword ASSUMPTIONS_KEY = Keyword.newKeyword("assumptions");
    private static final Keyword GOAL_KEY = Keyword.newKeyword("goal");
    private static final Keyword SORTSYSTEM_KEY = Keyword.newKeyword("sortsystem");
    private static final Keyword NAME_KEY = Keyword.newKeyword("name");
    private static final Keyword DESCRIPTION_KEY = Keyword.newKeyword("description");
    private static final Keyword ANSWER_VARIABLES = Keyword.newKeyword("answer-variables");
    private static final Keyword ANSWERS_EXPECTED = Keyword.newKeyword("answers-expected");
    private static final Keyword INPUT = Keyword.newKeyword("input");
    private static final Keyword OUTPUT = Keyword.newKeyword("output");
    private static final Keyword SKIP_KEY = Keyword.newKeyword("skip");

    public static List<Problem> readFrom(InputStream inputStream) throws Reader.ParsingException {

        Parseable pbr = Parsers.newParseable(new InputStreamReader(inputStream));
        Parser p = Parsers.newParser(defaultConfiguration());


        List<Problem> problems = CollectionUtils.newEmptyList();

        Object problemDesc = p.nextValue(pbr);

        while (problemDesc != Parser.END_OF_INPUT) {

            Problem problem = buildProblem((Map<?, ?>) problemDesc);
            if(!problem.shouldSkip()){
                problems.add(problem);
            }
            problemDesc = p.nextValue(pbr);
        }

        return problems;

    }

    public static List<Problem> readFrom(String path) throws Reader.ParsingException, FileNotFoundException {

        return readFrom( new FileInputStream(path));
    }

    public static List<DPLChunk> readDPLChunkFrom(InputStream inputStream) throws Reader.ParsingException {

        Parseable pbr = Parsers.newParseable(new InputStreamReader(inputStream));
        Parser p = Parsers.newParser(defaultConfiguration());


        List<DPLChunk> problems = CollectionUtils.newEmptyList();

        Object problemDesc = p.nextValue(pbr);

        while (problemDesc != Parser.END_OF_INPUT) {

            problems.add(buildChunk((Map<?, ?>) problemDesc));
            problemDesc = p.nextValue(pbr);
        }

        return problems;

    }


    private static List<Variable> readVariableList(List<?> lst) throws Reader.ParsingException {

        List<Variable> vars = lst.stream().map(x -> {
            try {
                return (Variable) Reader.readLogicValue(x);
            } catch (Reader.ParsingException e) {
                return null;
            }
        }).collect(Collectors.toList());

        if (vars.stream().anyMatch(Objects::isNull)) {

            throw new Reader.ParsingException("List has invalid variables: " + lst);
        }


        return vars;


    }

    private static List<Value> readValueList(List<?> lst) throws Reader.ParsingException {

        List<Value> vars = lst.stream().map(x -> {
            try {
                return Reader.readLogicValue(x);
            } catch (Reader.ParsingException e) {
                return null;
            }
        }).collect(Collectors.toList());

        if (vars.stream().anyMatch(Objects::isNull)) {

            throw new Reader.ParsingException("List has invalid values: " + lst);
        }


        return vars;


    }

     private static DPLChunk buildChunk(Map<?, ?> map) throws Reader.ParsingException {

        Set<Formula> assumptions = readAssumptions(map.get(ASSUMPTIONS_KEY));
        Formula goal = Reader.readFormula(map.get(OUTPUT));
        Phrase phrase = Reader.readPhrase(map.get(INPUT));
        if (map.containsKey(SORTSYSTEM_KEY)) {
            //TODO: Create a sorted problem
            //TODO: Define the class

            SortSystem sortSystem = SortSystem.buildFrom((Map<?, ?>) map.get(SORTSYSTEM_KEY));
            throw new NotImplementedException("buildChunk");

        } else {

            if (map.containsKey(ANSWERS_EXPECTED) && map.containsKey(ANSWER_VARIABLES)) {

                Set<List<Value>> expectedAnswers = ((List<?>)map.get(ANSWERS_EXPECTED))
                        .stream().
                        map(x -> {
                            try {
                                return readValueList((List<?>) x);
                            } catch (Reader.ParsingException e) {
                                return null;
                            }
                        }).collect(Collectors.toSet());


                return new DPLChunk(((Map) map).getOrDefault(NAME_KEY, "").toString(),
                        ((Map) map).getOrDefault(DESCRIPTION_KEY, "").toString(),
                        assumptions, phrase, goal);


            } else {

                return new DPLChunk(((Map) map).getOrDefault(NAME_KEY, "").toString(), ((Map) map).getOrDefault(DESCRIPTION_KEY, "").toString(), assumptions, phrase, goal);

            }

        }


    }
    private static Problem buildProblem(Map<?, ?> map) throws Reader.ParsingException {

        Set<Formula> assumptions = readAssumptions(map.get(ASSUMPTIONS_KEY));
        Formula goal = Reader.readFormula(map.get(GOAL_KEY));
        boolean skip =  (boolean) ((Map) map).getOrDefault(SKIP_KEY, false);

        if (map.containsKey(SORTSYSTEM_KEY)) {
            //TODO: Create a sorted problem
            //TODO: Define the class

            SortSystem sortSystem = SortSystem.buildFrom((Map<?, ?>) map.get(SORTSYSTEM_KEY));
            throw new NotImplementedException("buildProblem");

        } else {

            if (map.containsKey(ANSWERS_EXPECTED) && map.containsKey(ANSWER_VARIABLES)) {

                Set<List<Value>> expectedAnswers = ((List<?>)map.get(ANSWERS_EXPECTED))
                        .stream().
                        map(x -> {
                            try {
                                return readValueList((List<?>) x);
                            } catch (Reader.ParsingException e) {
                                return null;
                            }
                        }).collect(Collectors.toSet());


                return new Problem(((Map) map).getOrDefault(NAME_KEY, "").toString(),
                        ((Map) map).getOrDefault(DESCRIPTION_KEY, "").toString(),
                        assumptions, goal, readVariableList((List<?>) map.get(ANSWER_VARIABLES)),
                        expectedAnswers
                );


            } else {

                return new Problem(((Map) map).getOrDefault(NAME_KEY, "").toString(),
                    ((Map) map).getOrDefault(DESCRIPTION_KEY, "").toString(), assumptions, goal, skip);

            }

        }


    }

    private static Set<Formula> readAssumptions(Object thing) {

        if(thing instanceof Map<?, ?>){
            Map<?, ?> map = (Map<?, ?>) thing;
            return map.entrySet().stream().map(entry -> {
            try {

                return Reader.readFormula(entry.getValue());

            } catch (Exception e) {

                throw new AssertionError("Parsing Exception:" + e.getMessage());

            }

        }).collect(Collectors.toSet());
        }

        else {

            return (AxiomSet.getAxiomSetNamed(thing.toString()));

        }


    }

    public static void main(String[] args) throws Reader.ParsingException {
        System.out.println(readFrom(Simulator.class.getResourceAsStream("firstorder-completness-tests.clj")));
    }
}
