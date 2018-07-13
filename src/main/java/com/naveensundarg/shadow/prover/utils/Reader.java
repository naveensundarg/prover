package com.naveensundarg.shadow.prover.utils;

import clojure.lang.IFn;
import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.deduction.*;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.method.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import us.bpsm.edn.Symbol;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import java.io.StringReader;
import java.text.ParseException;
import java.util.*;
import java.util.stream.Collectors;

import static us.bpsm.edn.parser.Parsers.defaultConfiguration;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Reader {

    private enum QuantifierType {
        Universal, Existential, Schema
    }

    private static final String ASSUME = "assume";
    private static final String ASSUME_STAR = "assume*";

    private static final Symbol NOT = Symbol.newSymbol("not");
    private static final Symbol AND = Symbol.newSymbol("and");
    private static final Symbol OR = Symbol.newSymbol("or");
    private static final Symbol IF = Symbol.newSymbol("if");
    private static final Symbol IMPLIES = Symbol.newSymbol("implies");
    private static final Symbol COUNTERFACTUAL = Symbol.newSymbol("=>");

    private static final Symbol IFF = Symbol.newSymbol("iff");

    private static final Symbol EXISTS = Symbol.newSymbol("exists");
    private static final Symbol FORALL = Symbol.newSymbol("forall");
    private static final Symbol SCHEMA = Symbol.newSymbol("schema");

    private static final Symbol BELIEVES = Symbol.newSymbol("Believes!");
    private static final Symbol INTENDS = Symbol.newSymbol("Intends!");

    private static final Symbol KNOWS = Symbol.newSymbol("Knows!");
    private static final Symbol PERCEIVES = Symbol.newSymbol("Perceives!");
    private static final Symbol DESIRES = Symbol.newSymbol("Desires!");

    private static final Symbol COMMON = Symbol.newSymbol("Common!");
    private static final Symbol SAYS = Symbol.newSymbol("Says!");
    private static final Symbol COMMUNICATES = Symbol.newSymbol("Communicates!");

    private static final Symbol OUGHT = Symbol.newSymbol("Ought!");
    private static final Symbol TRAIT = Symbol.newSymbol("Trait!");


    private static final Symbol NEC = Symbol.newSymbol("nec");
    private static final Symbol POS = Symbol.newSymbol("pos");

    private static final Symbol CAN_PROVE = Symbol.newSymbol("CAN_PROVE!");

    public static final Value NOW, I;

    private static final Map<String,String> SNARK_BUILTIN_RELATIONS = CollectionUtils.newMap();
    private static final Map<String,String> SNARK_BUILTIN_FUNCTIONS = CollectionUtils.newMap();


    static {
        try {


            NOW = readLogicValue("NOW");
            I = readLogicValueFromString("I");

            SNARK_BUILTIN_RELATIONS.put("<", "$$$less");
            SNARK_BUILTIN_RELATIONS.put("<=", "$$$lesseq");
            SNARK_BUILTIN_RELATIONS.put(">", "$$$greater");
            SNARK_BUILTIN_RELATIONS.put(">=", "$$$greatereq");

            SNARK_BUILTIN_FUNCTIONS.put("+", "$$sum");
            SNARK_BUILTIN_FUNCTIONS.put("-", "$$difference");
            SNARK_BUILTIN_FUNCTIONS.put("*", "$$produce");
            SNARK_BUILTIN_FUNCTIONS.put("/", "$$quotient_r");


        } catch (Exception e) {
            throw new AssertionError("Could not instantiate basic constant: now");
        }
    }

    public static class ParsingException extends Exception {

        private String message;

        public ParsingException(String message) {
            this.message = message;
        }

        @Override
        public String getMessage() {
            return message;
        }
    }


    public static Value readLogicValueFromString(String input) throws ParsingException {

        Parseable parseable = Parsers.newParseable(input);
        Parser parser = Parsers.newParser(defaultConfiguration());


        return readLogicValue(parser.nextValue(parseable), Sets.newSet());
    }

    public static Value readLogicValue(Object input) throws ParsingException {

        return readLogicValue(input, Sets.newSet());
    }

    public static Value readLogicValue(Object input, Set<String> variableNames) throws ParsingException {


        if (input instanceof List) {


            List list = (List) input;

            if (list.size() == 1) {
                String name = list.get(0).toString();

                return name.startsWith("?") || variableNames.contains(name) ? new Variable(name) : new Constant(name);

            }

            Object nameObject = list.get(0);

            if (nameObject instanceof Symbol) {
                String name = ((Symbol) nameObject).getName();
                Value[] arguments = new Value[list.size() - 1];

                for (int i = 1; i < list.size(); i++) {

                    arguments[i - 1] = readLogicValue(list.get(i), variableNames);
                }

                return new Compound(SNARK_BUILTIN_FUNCTIONS.getOrDefault(name, name), arguments);
            }
            throw new ParsingException("name should be a string" + nameObject);

        } else {
            String name = input.toString();
            return name.startsWith("?") || variableNames.contains(name) ? new Variable(name) : new Constant(name);
        }

    }

    public static Formula readFormula(Object input) {

        try {
            return readFormula(input, Sets.newSet());
        } catch (ParsingException e) {

            throw new AssertionError(e.getMessage());
        }

    }

    public static Formula readFormulaFromString(String input) throws ParsingException {

        Parseable parseable = Parsers.newParseable(preprocess(input));
        Parser p = Parsers.newParser(defaultConfiguration());


        return readFormula(p.nextValue(parseable));

    }

    public static Phrase readPhraseFromString(String input) throws ParsingException {

        Parseable parseable = Parsers.newParseable(preprocess(input));
        Parser p = Parsers.newParser(defaultConfiguration());

        return readPhrase(p.nextValue(parseable));

    }

    public static Phrase readPhrase(Object input) throws ParsingException {

        return readPhrase(input, Sets.newSet());

    }

    private static Phrase readPhrase(Object input, Set<String> variableNames) throws ParsingException {

        if (input instanceof Symbol) {

            Symbol symbol = (Symbol) input;
            String name = symbol.getName();
            if (name.startsWith("!")) {

                name = name.substring(1);

                if (name.equals("claim")) {

                    return Claim.getInstance();

                } else if (name.equals("modus-ponens")) {

                    return ModusPonens.getInstance();

                } else if (name.equals("double-negation")) {

                    return DoubleNegation.getInstance();

                } else if (name.equals("modus-tollens")) {

                    return ModusTollens.getInstance();

                } else if (name.equals("both")) {

                    return Both.getInstance();

                } else if (name.equals("left-and")) {

                    return LeftAnd.getInstance();

                } else if (name.equals("right-and")) {

                    return RightAnd.getInstance();

                } else if (name.equals("left-either")) {

                    return LeftEither.getInstance();

                } else if (name.equals("right-either")) {

                    return RightEither.getInstance();

                } else if (name.equals("left-iff")) {

                    return LeftIff.getInstance();

                } else if (name.equals("right-iff")) {

                    return RightIff.getInstance();

                } else if (name.equals("constructive-dilemma")) {

                    return ConstructiveDilemma.getInstance();

                } else if (name.equals("equivalence")) {

                    return Equivalence.getInstance();

                } else if (name.equals("true-intro")) {

                    return TrueIntro.getInstance();

                } else if (name.equals("false-elim")) {

                    return FalseElim.getInstance();

                } else if (name.equals("absurd")) {

                    return Absurd.getInstance();

                }

                throw new ParsingException("Could not resolve the method " + name);
            } else {


                String atomName = ((Symbol) input).getName();
                return new Atom(SNARK_BUILTIN_RELATIONS.getOrDefault(atomName, atomName));
            }

        }
        if (input instanceof List) {
            List list = (List) input;

            if (list.isEmpty()) {
                throw new ParsingException("Can't build a formula");
            }
            Object first = list.get(0);

            if (isMethod(first)) {

                List<Phrase> args = CollectionUtils.newEmptyList();

                for (int i = 1; i < list.size(); i++) {

                    args.add(readPhrase(list.get(i), variableNames));
                }
                return new MethodApplication(readPhrase(first, variableNames), args);
            } else if (first.toString().equals(ASSUME)) {
                if (list.size() == 4) {

                    Phrase E = readPhrase(list.get(1), variableNames);
                    Phrase D = readPhrase(list.get(3), variableNames);

                    if (D instanceof Deduction) {
                        return new Assume(E, (Deduction) D);
                    } else {

                        throw new ParsingException("The second argument of an assumption should be a deduction");
                    }


                } else {

                    throw new ParsingException("Assume needs exactly two arguments but got " + (list.size() - 1));
                }

            } else if (first.toString().equals(ASSUME_STAR)) {
                if (list.size() >= 4) {


                    List<Phrase> assumptions = CollectionUtils.newEmptyList();
                    for (int i = 1; i < list.size() - 2; i++) {
                        assumptions.add(readPhrase(list.get(i), variableNames));
                    }
                    Phrase D = readPhrase(list.get(list.size() - 1), variableNames);

                    if (D instanceof Deduction) {
                        return new AssumeStar(assumptions, (Deduction) D);
                    } else {

                        throw new ParsingException("The second argument of an assumption should be a deduction");
                    }


                } else {

                    throw new ParsingException("Assume needs exactly two arguments but got " + (list.size() - 1));
                }

            } else if (first.toString().equals("suppose-absurd")) {
                if (list.size() == 4) {

                    Phrase E = readPhrase(list.get(1), variableNames);
                    Phrase D = readPhrase(list.get(3), variableNames);

                    if (D instanceof Deduction) {
                        return new SupposeAbsurd(E, (Deduction) D);
                    } else {

                        throw new ParsingException("The second argument of an suppose-absurd should be a deduction");
                    }


                } else {

                    throw new ParsingException("Suppose-absurd needs exactly two arguments but got " + (list.size() - 1));
                }

            } else {

                return readFormula(input, variableNames);
            }
        }

        throw new ParsingException("Could not read: " + input);


    }

    private static Formula readFormula(Object input, Set<String> variableNames) throws ParsingException {

        if (input instanceof Symbol) {
            String name = ((Symbol) input).getName();
            return new Atom(SNARK_BUILTIN_RELATIONS.getOrDefault(name, name));
        }

        if (input instanceof List) {
            List list = (List) input;

            if (list.isEmpty()) {
                throw new ParsingException("Can't build a formula");
            }
            Object first = list.get(0);

            if (first instanceof Symbol) {
                Symbol name = (Symbol) first;

                if (name.equals(NOT)) {
                    if (list.size() == 2) {
                        return new Not(readFormula(list.get(1), variableNames));

                    } else {
                        throw new ParsingException("Not should have one argument");
                    }
                }

                if (name.equals(IF) || name.equals(IMPLIES)) {
                    if (list.size() == 3) {
                        return new Implication(readFormula(list.get(1), variableNames), readFormula(list.get(2), variableNames));

                    } else {
                        throw new ParsingException("If should have two arguments");
                    }
                }

                if (name.equals(COUNTERFACTUAL)) {
                    if (list.size() == 3) {
                        return new CounterFactual(readFormula(list.get(1), variableNames), readFormula(list.get(2), variableNames));

                    } else {
                        throw new ParsingException("Counterfactual should have two arguments");
                    }
                }

                if (name.equals(IFF)) {
                    if (list.size() == 3) {
                        return new BiConditional(readFormula(list.get(1), variableNames), readFormula(list.get(2), variableNames));

                    } else {
                        throw new ParsingException("Iff should have two arguments");
                    }
                }

                if (name.equals(OR)) {
                    if (list.size() >= 3) {

                        Formula[] subs = new Formula[list.size() - 1];

                        for (int i = 1; i < list.size(); i++) {
                            subs[i - 1] = readFormula(list.get(i), variableNames);
                        }

                        return new Or(subs);
                    } else {
                        throw new ParsingException("Or should have at least two arguments");
                    }
                }

                if (name.equals(AND)) {
                    if (list.size() >= 3) {

                        Formula[] subs = new Formula[list.size() - 1];

                        for (int i = 1; i < list.size(); i++) {
                            subs[i - 1] = readFormula(list.get(i), variableNames);
                        }

                        return new And(subs);


                    } else {
                        throw new ParsingException("And should have at least two arguments");
                    }
                }
                //FORALL
                if (name.equals(FORALL)) {

                    return constructQuantifier(list, QuantifierType.Universal, variableNames);

                }

                //EXISTS
                if (name.equals(EXISTS)) {

                    return constructQuantifier(list, QuantifierType.Existential, variableNames);

                }

                if (name.equals(SCHEMA)) {

                    return constructQuantifier(list, QuantifierType.Schema, variableNames);


                }

                //Believes
                if (name.equals(BELIEVES)) {

                    return constructBelief(list, variableNames);
                }

                //Intends
                if (name.equals(INTENDS)) {

                    return constructIntends(list, variableNames);
                }

                //Knows
                if (name.equals(KNOWS)) {

                    return constructKnowledge(list, variableNames);
                }

                //Perceives
                if (name.equals(PERCEIVES)) {

                    return constructPerceives(list, variableNames);
                }

                if (name.equals(DESIRES)) {

                    return constructDesires(list, variableNames);
                }
                if (name.equals(COMMON)) {

                    return constructCommon(list, variableNames);
                }
                if (name.equals(SAYS)) {

                    return constructSays(list, variableNames);
                }
                if (name.equals(COMMUNICATES)) {

                    return constructCommunicates(list, variableNames);
                }


                if (name.equals(OUGHT)) {

                    return constructOught(list, variableNames);
                }


                if (name.equals(CAN_PROVE)) {

                    return constructCanProve(list, variableNames);
                }


                if (name.equals(NEC)) {

                    return constructNecessity(list, variableNames);

                }

                if (name.equals(POS)) {

                    return constructPossibility(list, variableNames);

                }

                if (name.equals(TRAIT)) {

                    return constructTrait(list, variableNames);

                }

                if (name.toString().endsWith("!")) {


                    return constructGeneralModal(list, variableNames);
                }

                return constructPredicate(list, variableNames);


            }

        }

        if (input.toString().equals("true")) {
            return new Atom("true");
        }

        if (input.toString().equals("false")) {
            return new Atom("false");
        }

        throw new AssertionError("Could not understand formula: " + input);

    }


    private static Formula constructIntends(List list, Set<String> variableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("Intends expresion cannot be empty!");
        } else if (list.size() != 4 && list.size() != 3) {
            throw new ParsingException("Intends expresion has wrong number of arguments! " + list);

        } else {

            if (list.size() == 4) {
                Object agent = list.get(1);
                Object time = list.get(2);
                Object formula = list.get(3);

                return new Intends(readLogicValue(agent), readLogicValue(time), readFormula(formula, variableNames));

            } else {

                Object agent = list.get(1);
                Object formula = list.get(2);

                return new Intends(readLogicValue(agent), NOW, readFormula(formula, variableNames));

            }
        }
    }

    private static Formula constructNecessity(List list, Set<String> variableNames) throws ParsingException {

        if (list.isEmpty()) {
            throw new ParsingException("Necessity expresion cannot be empty!");
        } else if (list.size() != 2) {
            throw new ParsingException("Necessity expresion has wrong number of arguments! " + list);

        } else {
            Object formula = list.get(1);

            return new Necessity(readFormula(formula, variableNames));

        }


    }

    private static Formula constructPossibility(List list, Set<String> variableNames) throws ParsingException {

        if (list.isEmpty()) {
            throw new ParsingException("Possibility expresion cannot be empty!");
        } else if (list.size() != 2) {
            throw new ParsingException("Possibility expresion has wrong number of arguments! " + list);

        } else {
            Object formula = list.get(1);

            return new Possibility(readFormula(formula, variableNames));

        }


    }

    private static Formula constructGeneralModal(List list, Set<String> variableNames) throws ParsingException {

        if (list.isEmpty()) {
            throw new ParsingException("Possibility expresion cannot be empty!");
        } else {
            Object formula = list.get(1);

            return new GeneralModal(Reader.readLogicValue(list.get(0)), (List<Formula>) list.subList(1, list.size()).stream().map(Reader::readFormula).collect(Collectors.toList()));

        }


    }

    private static Formula constructCanProve(List list, Set<String> variableNames) throws ParsingException {

        if (list.isEmpty()) {
            throw new ParsingException("CanProve expresion cannot be empty!");
        } else if (list.size() != 2) {
            throw new ParsingException("CanProve expresion has wrong number of arguments! " + list);

        } else {
            Object formula = list.get(1);

            return new CanProve(readFormula(formula, variableNames));

        }


    }

    // (K! agent time P)
    private static Formula constructKnowledge(List list, Set<String> variableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("Knowledge expresion cannot be empty!");
        } else if (list.size() != 4 && list.size() != 3) {
            throw new ParsingException("Knowledge expresion has wrong number of arguments! " + list);

        } else {

            if (list.size() == 4) {
                Object agent = list.get(1);
                Object time = list.get(2);
                Object formula = list.get(3);

                return new Knowledge(readLogicValue(agent), readLogicValue(time), readFormula(formula, variableNames));

            } else {

                Object agent = list.get(1);
                Object formula = list.get(2);

                return new Knowledge(readLogicValue(agent), NOW, readFormula(formula, variableNames));

            }
        }
    }

    // (K! agent time P)
    private static Formula constructPerceives(List list, Set<String> variableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("Perceieves expresion cannot be empty!");
        } else if (list.size() != 4 && list.size() != 3) {
            throw new ParsingException("Perceieves expresion has wrong number of arguments! " + list);

        } else {

            if (list.size() == 4) {
                Object agent = list.get(1);
                Object time = list.get(2);
                Object formula = list.get(3);

                return new Perception(readLogicValue(agent), readLogicValue(time), readFormula(formula, variableNames));

            } else {

                Object agent = list.get(1);
                Object formula = list.get(2);

                return new Perception(readLogicValue(agent), NOW, readFormula(formula, variableNames));

            }

        }
    }

    private static Formula constructDesires(List list, Set<String> variableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("Desires expresion cannot be empty!");
        } else if (list.size() != 4 && list.size() != 3) {
            throw new ParsingException("Desires expresion has wrong number of arguments! " + list);

        } else {

            if (list.size() == 4) {
                Object agent = list.get(1);
                Object time = list.get(2);
                Object formula = list.get(3);

                return new Desire(readLogicValue(agent), readLogicValue(time), readFormula(formula, variableNames));

            } else {

                Object agent = list.get(1);
                Object formula = list.get(2);

                return new Desire(readLogicValue(agent), NOW, readFormula(formula, variableNames));

            }

        }
    }

    // (B! agent time strength P)
    private static Formula constructBelief(List list, Set<String> variableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("Belief expresion cannot be empty!");
        } else if (list.size() != 4 && list.size() != 3) {
            throw new ParsingException("Belief expresion has wrong number of arguments! " + list);

        } else {

            if (list.size() == 4) {

                Object agent = list.get(1);
                Object time = list.get(2);
                Object formula = list.get(3);

                return new Belief(readLogicValue(agent), readLogicValue(time), readFormula(formula, variableNames));

            } else {

                Object agent = list.get(1);
                Object formula = list.get(2);

                return new Belief(readLogicValue(agent), NOW, readFormula(formula, variableNames));

            }
        }
    }

    // (Ought! agent time strength P)
    private static Formula constructOught(List list, Set<String> variableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("Ought expresion cannot be empty!");
        } else if (list.size() != 5 && list.size() != 4) {
            throw new ParsingException("Ought expresion has wrong number of arguments! " + list);

        } else {

            if (list.size() == 5) {
                Object agent = list.get(1);
                Object time = list.get(2);
                Object formula = list.get(3);

                Object ought = list.get(4);

                return new Ought(readLogicValue(agent), readLogicValue(time), readFormula(formula, variableNames), readFormula(ought, variableNames));

            } else {

                Object agent = list.get(1);
                Object formula = list.get(2);

                Object ought = list.get(3);

                return new Ought(readLogicValue(agent), NOW, readFormula(formula, variableNames), readFormula(ought, variableNames));

            }
        }
    }

    // (Trait! [vars] agent time Formula value)
    private static Formula constructTrait(List list, Set<String> outerVariableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("Trait expresion cannot be empty!");
        } else if (list.size() != 6 && list.size() != 5) {
            throw new ParsingException("Trait expresion has wrong number of arguments! " + list);

        } else {

            Set<String> variableNames;

            List<Variable> traitVars;
            Object varListObject = list.get(1);
            if (varListObject instanceof List) {
                List listOfVars = (List) varListObject;
                Variable[] variables = new Variable[((List) varListObject).size()];

                variableNames = Sets.newSet();
                for (int i = 0; i < variables.length; i++) {
                    Object varObject = listOfVars.get(i);
                    if (varObject instanceof Symbol) {

                        variables[i] = new Variable(((Symbol) varObject).getName());

                        variableNames.add(((Symbol) varObject).getName());
                    } else {

                        throw new ParsingException("Variable should be a string: " + varObject);

                    }

                }
                Set<String> conflicts = Sets.intersection(outerVariableNames, variableNames);
                if (!conflicts.isEmpty()) {
                    throw new ParsingException("This quantifier's variables appear in the outer scope: " + list + ". Conflicting vars: " + conflicts);
                }

                traitVars = Arrays.stream(variables).collect(Collectors.toList());
            } else {


                variableNames = Sets.with(((Symbol) varListObject).getName());

                Set<String> conflicts = Sets.intersection(outerVariableNames, variableNames);
                if (!conflicts.isEmpty()) {
                    throw new ParsingException("This quantifier's variables appear in the outer scope: " + list + ". Conflicting vars: " + conflicts);
                }

                traitVars = CollectionUtils.newEmptyList();
                traitVars.add(new Variable(((Symbol) varListObject).getName()));
            }
            Set<String> allVars = Sets.union(outerVariableNames, variableNames);
            if (list.size() == 6) {


                Object agent = list.get(2);
                Object time = list.get(3);
                Object formula = list.get(4);

                Object actionType = list.get(5);

                return new Trait(traitVars, readLogicValue(agent), readLogicValue(time),
                        readFormula(formula, allVars),
                        readLogicValue(actionType, allVars));

            } else {

                Object agent = list.get(2);
                Object formula = list.get(3);
                Object actionType = list.get(4);

                return new Trait(traitVars, readLogicValue(agent), NOW,
                        readFormula(formula, allVars),
                        readLogicValue(actionType, allVars));

            }
        }
    }

    // (Says! agent time strength P)
    private static Formula constructSays(List list, Set<String> variableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("Says expresion cannot be empty!");
        } else if (list.size() != 4 && list.size() != 3) {
            throw new ParsingException("Says expresion has wrong number of arguments! " + list);

        } else {

            if (list.size() == 4) {
                Object agent = list.get(1);
                Object time = list.get(2);
                Object formula = list.get(3);

                return new Says(readLogicValue(agent), readLogicValue(time), readFormula(formula, variableNames));

            } else {

                Object agent = list.get(1);
                Object formula = list.get(2);

                return new Says(readLogicValue(agent), NOW, readFormula(formula, variableNames));

            }

        }
    }

    private static Formula constructCommunicates(List list, Set<String> variableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("Coomunicates expresion cannot be empty!");
        } else if (list.size() != 5 && list.size() != 4) {
            throw new ParsingException("Says expresion has wrong number of arguments! " + list);

        } else {

            if (list.size() == 5) {
                Object agent1 = list.get(1);
                Object agent2 = list.get(2);

                Object time = list.get(3);
                Object formula = list.get(4);

                return new Communicates(readLogicValue(agent1), readLogicValue(agent2), readLogicValue(time), readFormula(formula, variableNames));

            } else {

                Object agent1 = list.get(1);
                Object agent2 = list.get(2);

                Object formula = list.get(3);

                return new Communicates(readLogicValue(agent1), readLogicValue(agent2), NOW, readFormula(formula, variableNames));

            }

        }
    }

    // (CommonUtils!  time  P)
    private static Formula constructCommon(List list, Set<String> variableNames) throws ParsingException {
        if (list.isEmpty()) {
            throw new ParsingException("CommonUtils expresion cannot be empty!");
        } else if (list.size() != 3 && list.size() != 2) {
            throw new ParsingException("CommonUtils expresion has wrong number of arguments! " + list);

        } else {

            if (list.size() == 3) {
                Object time = list.get(1);
                Object formula = list.get(2);
                return new Common(readLogicValue(time), readFormula(formula, variableNames));
            } else {

                Object formula = list.get(1);
                return new Common(NOW, readFormula(formula, variableNames));

            }
        }
    }


    private static Formula constructPredicate(List list, Set<String> variableNames) throws ParsingException {

        if (list.isEmpty()) {
            throw new ParsingException("Predicate expresion cannot be empty!");
        } else {

            Object nameObject = list.get(0);

            if (nameObject instanceof Symbol) {

                String name = ((Symbol) nameObject).getName();



                Value[] values = new Value[list.size() - 1];

                for (int i = 1; i < list.size(); i++) {

                    values[i - 1] = readLogicValue(list.get(i), variableNames);
                }


                return new Predicate(SNARK_BUILTIN_RELATIONS.getOrDefault(name, name), values);

            } else {
                throw new ParsingException("Name of predicate should be a string! " + nameObject);

            }

        }
    }


    private static Formula constructQuantifier(List list, QuantifierType quantifierType, Set<String> outerVariableNames) throws ParsingException {
        if (list.size() == 3) {

            Object varListObject = list.get(1);
            if (varListObject instanceof List) {
                List listOfVars = (List) varListObject;
                Variable[] variables = new Variable[((List) varListObject).size()];

                Set<String> variableNames = Sets.newSet();
                for (int i = 0; i < variables.length; i++) {
                    Object varObject = listOfVars.get(i);
                    if (varObject instanceof Symbol) {

                        variables[i] = new Variable(((Symbol) varObject).getName());

                        variableNames.add(((Symbol) varObject).getName());
                    } else {

                        throw new ParsingException("Variable should be a string: " + varObject);

                    }

                }
                Set<String> conflicts = Sets.intersection(outerVariableNames, variableNames);
                if (!conflicts.isEmpty()) {
                    throw new ParsingException("This quantifier's variables appear in the outer scope: " + list + ". Conflicting vars: " + conflicts);
                }
                Formula argument = readFormula(list.get(2), Sets.union(outerVariableNames, variableNames));
                if (quantifierType.equals(QuantifierType.Universal)) {
                    return new Universal(variables, argument);
                }
                if (quantifierType.equals(QuantifierType.Existential)) {
                    return new Existential(variables, argument);
                } else {
                    return new Schema(variables, argument);
                }

            } else if (varListObject instanceof Symbol) {

                Variable[] variables = new Variable[1];

                variables[0] = new Variable(((Symbol) varListObject).getName());

                Set<String> variableNames = Sets.with(((Symbol) varListObject).getName());

                Set<String> conflicts = Sets.intersection(outerVariableNames, variableNames);
                if (!conflicts.isEmpty()) {
                    throw new ParsingException("This quantifier's variables appear in the outer scope: " + list + ". Conflicting vars: " + conflicts);
                }
                Formula argument = readFormula(list.get(2), Sets.union(outerVariableNames, variableNames));
                if (quantifierType.equals(QuantifierType.Universal)) {
                    return new Universal(variables, argument);
                }
                if (quantifierType.equals(QuantifierType.Existential)) {
                    return new Existential(variables, argument);
                } else {
                    return new Schema(variables, argument);
                }


            } else {

                throw new ParsingException("The variable list for this quantifier formula is not valid: " + list);

            }


        } else {

            throw new ParsingException("quantifiers should have exactly 3 arguments: " + list);

        }
    }


    public static List<String> extractForms(String input) throws ParsingException {
        List<String> forms = new ArrayList<>();
        Stack<String> parens = new Stack<>();

        String[] tokens = input.
                replaceAll("\\(", " ( ").
                replaceAll("\\)", " ) ").
                split(" ");

        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < tokens.length; i++) {

            if (tokens[i].isEmpty()) {
                continue;
            }
            stringBuilder.append(" " + tokens[i].replace(" ", "") + " ");


            if (tokens[i].equals("(")) {
                parens.push("");
            }
            if (tokens[i].equals(")") && parens.empty()) {
                throw new ParsingException("Extra )");
            }


            if (tokens[i].equals(")") && !parens.empty()) {
                parens.pop();
            }


            if (parens.isEmpty()) {
                forms.add(stringBuilder.toString());
                stringBuilder = new StringBuilder();

            }


        }

        return forms;
    }


    public static Object readFromString(String s) {

        Parseable parseable = Parsers.newParseable(preprocess(s));
        Parser parser = Parsers.newParser(defaultConfiguration());

        return parser.nextValue(parseable);
    }


    private static final Set<String> PRIMITIVE_METHOD_NAMES = Arrays.stream(new String[]{
            "claim",
            "modus-ponens",
            "modus-tollens",
            "double-negation",
            "both",
            "left-and",
            "right-and",
            "true-intro",
            "false-elim",
            "left-either",
            "right-either",
            "left-iff",
            "right-iff",
            "constructive-dilemma",
            "equivalence",
            "absurd"


    }).
            collect(Collectors.toSet());

    private static boolean isMethod(Object input) {

        if (input instanceof Symbol) {

            String name = ((Symbol) input).getName();


            return PRIMITIVE_METHOD_NAMES.contains(name.substring(1));

        } else {

            return false;
        }

    }


    public static final String preprocess(String x) {
        return x.replace("'", "%!%");
    }

}
