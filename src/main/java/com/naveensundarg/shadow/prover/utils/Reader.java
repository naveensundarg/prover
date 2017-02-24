package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import us.bpsm.edn.Symbol;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;

import static us.bpsm.edn.parser.Parsers.defaultConfiguration;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Reader {

    private static final Symbol NOT = Symbol.newSymbol("not");
    private static final Symbol AND = Symbol.newSymbol("and");
    private static final Symbol OR = Symbol.newSymbol("or");
    private static final Symbol IF = Symbol.newSymbol("if");
    private static final Symbol IMPLIES = Symbol.newSymbol("implies");

    private static final Symbol IFF = Symbol.newSymbol("iff");

    private static final Symbol EXISTS = Symbol.newSymbol("exists");
    private static final Symbol FORALL = Symbol.newSymbol("forall");

    private static final Symbol BELIEVES = Symbol.newSymbol("Believes!");
    private static final Symbol INTENDS = Symbol.newSymbol("Intends!");

    private static final Symbol KNOWS = Symbol.newSymbol("Knows!");
    private static final Symbol PERCEIVES = Symbol.newSymbol("Perceives!");
    private static final Symbol DESIRES = Symbol.newSymbol("Desires!");

    private static final Symbol COMMON = Symbol.newSymbol("Common!");
    private static final Symbol SAYS = Symbol.newSymbol("Says!");
    private static final Symbol COMMUNICATES = Symbol.newSymbol("Communicates!");

    private static final Symbol OUGHT = Symbol.newSymbol("Ought!");


    private static final Symbol NEC = Symbol.newSymbol("nec");
    private static final Symbol POS = Symbol.newSymbol("pos");

    private static final Symbol CAN_PROVE = Symbol.newSymbol("CAN_PROVE!");

    private static final Value NOW ;

    static {
        try {

            Parseable pbr = Parsers.newParseable(new StringReader("now"));
            Parser p = Parsers.newParser(defaultConfiguration());

            NOW = readLogicValue(p.nextValue(pbr));
        } catch (Exception e) {
            throw new AssertionError("Could not instantiate basic constant: now");
        }
    }
    public static class ParsingException extends Exception{

        private String message;

        public ParsingException(String message){
            this.message = message;
        }

        @Override
        public String getMessage(){
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
    public static Value readLogicValue(Object input, Set<String> variableNames) throws ParsingException{




        if(input instanceof List){


            List list = (List) input;

            if(list.size() == 1){
                String name = list.get(0).toString();

                return name.startsWith("?") || variableNames.contains(name) ? new Variable(name): new Constant(name);

            }

            Object nameObject = list.get(0);

            if(nameObject instanceof Symbol){
                String name =  ((Symbol) nameObject).getName();
                Value[] arguments = new Value[list.size()-1];

                for(int i = 1; i< list.size(); i++){

                    arguments[i-1] = readLogicValue(list.get(i), variableNames);
                }
                return new Compound(name, arguments);
            }
            throw new ParsingException("name should be a string" + nameObject);

        }
        else {
            String name =  input.toString();
            return name.startsWith("?") || variableNames.contains(name) ? new Variable(name): new Constant(name);
        }

    }

    public static Formula readFormula(Object input) throws ParsingException {

        return readFormula(input, Sets.newSet());

    }

    public static Formula readFormulaFromString(String input) throws ParsingException {

        Parseable parseable = Parsers.newParseable(input);
        Parser p = Parsers.newParser(defaultConfiguration());


        return readFormula(p.nextValue(parseable));

    }


    private static Formula readFormula(Object input, Set<String> variableNames) throws ParsingException {

        if(input instanceof Symbol){
            return new Atom(((Symbol) input).getName());
        }

        if(input instanceof List){
            List list = (List) input;

            if(list.isEmpty()){
                throw new ParsingException("Can't build a formula");
            }
            Object first = list.get(0);

            if(first instanceof Symbol){
                Symbol name = (Symbol) first;

                if(name.equals(NOT) ){
                    if(list.size()==2){
                        return new Not(readFormula(list.get(1), variableNames));

                    } else {
                        throw new ParsingException("Not should have one argument");
                    }
                }

                if(name.equals(IF) || name.equals(IMPLIES)){
                    if(list.size()==3){
                        return new Implication(readFormula(list.get(1), variableNames),readFormula(list.get(2), variableNames));

                    } else {
                        throw new ParsingException("If should have two arguments");
                    }
                }

                if(name.equals(IFF)){
                    if(list.size()==3){
                        return new BiConditional(readFormula(list.get(1), variableNames),readFormula(list.get(2), variableNames));

                    } else {
                        throw new ParsingException("Iff should have two arguments");
                    }
                }

                if(name.equals(OR)){
                    if(list.size()>=3){

                        Formula[] subs = new Formula[list.size()-1];

                        for(int i = 1 ; i < list.size(); i++){
                            subs[i-1] = readFormula(list.get(i), variableNames);
                        }

                        return new Or(subs);
                    } else {
                        throw new ParsingException("Or should have at least two arguments");
                    }
                }

                if(name.equals(AND)){
                    if(list.size()>=3){

                        Formula[] subs = new Formula[list.size()-1];

                        for(int i = 1 ; i < list.size(); i++){
                            subs[i-1] = readFormula(list.get(i), variableNames);
                        }

                        return new And(subs);


                    } else {
                        throw new ParsingException("And should have at least two arguments");
                    }
                }
                //FORALL
                if(name.equals(FORALL)){

                    return constructQuantifier(list, true, variableNames);

                }

                //FORALL
                if(name.equals(EXISTS)){

                    return constructQuantifier(list, false, variableNames);

                }

                //Believes
                if(name.equals(BELIEVES)){

                    return constructBelief(list, variableNames);
                }

                //Intends
                if(name.equals(INTENDS)){

                    return constructIntends(list, variableNames);
                }

                //Knows
                if(name.equals(KNOWS)){

                    return constructKnowledge(list, variableNames);
                }

                //Perceives
                if(name.equals(PERCEIVES)){

                    return constructPerceives(list, variableNames);
                }

                if(name.equals(DESIRES)){

                    return constructDesires(list, variableNames);
                }
                if(name.equals(COMMON)){

                    return constructCommon(list, variableNames);
                }
                if(name.equals(SAYS)){

                    return constructSays(list, variableNames );
                }
                if(name.equals(COMMUNICATES)){

                    return constructCommunicates(list, variableNames );
                }


                if(name.equals(OUGHT)){

                    return constructOught(list, variableNames);
                }


                if(name.equals(CAN_PROVE)){

                    return constructCanProve(list, variableNames);
                }


                if(name.equals(NEC)){

                    return constructNecessity(list, variableNames);

                }

                if(name.equals(POS)){

                    return constructPossibility(list, variableNames);

                }

                return constructPredicate(list, variableNames);


            }

        }

         throw new AssertionError("Could not understand formula: " + input);

    }

    private static Formula constructIntends(List list, Set<String> variableNames) throws ParsingException {
         if(list.isEmpty()){
            throw new ParsingException("Intends expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size() != 3){
            throw new ParsingException("Intends expresion has wrong number of arguments! "+list);

        } else {

            if(list.size() == 4) {
                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Intends(readLogicValue(agent), readLogicValue(time),readFormula(formula, variableNames));

            } else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Intends(readLogicValue(agent), NOW,readFormula(formula, variableNames));

            }
        }
    }

    private static Formula constructNecessity(List list, Set<String> variableNames) throws ParsingException {

        if(list.isEmpty()){
            throw new ParsingException("Necessity expresion cannot be empty!");
        }  else if(list.size() != 2 ){
            throw new ParsingException("Necessity expresion has wrong number of arguments! "+list);

        } else {
            Object formula =  list.get(1);

            return new Necessity(readFormula(formula, variableNames));

        }


    }
    private static Formula constructPossibility(List list, Set<String> variableNames) throws ParsingException {

        if(list.isEmpty()){
            throw new ParsingException("Possibility expresion cannot be empty!");
        }  else if(list.size() != 2 ){
            throw new ParsingException("Possibility expresion has wrong number of arguments! "+list);

        } else {
            Object formula =  list.get(1);

            return new Possibility(readFormula(formula, variableNames));

        }


    }
    private static Formula constructCanProve(List list, Set<String> variableNames) throws ParsingException {

        if(list.isEmpty()){
            throw new ParsingException("CanProve expresion cannot be empty!");
        }  else if(list.size() != 2 ){
            throw new ParsingException("CanProve expresion has wrong number of arguments! "+list);

        } else {
            Object formula =  list.get(1);

            return new CanProve(readFormula(formula, variableNames));

        }


    }

    // (K! agent time P)
    private static Formula constructKnowledge(List list, Set<String> variableNames) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Knowledge expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size() != 3){
            throw new ParsingException("Knowledge expresion has wrong number of arguments! "+list);

        } else {

            if(list.size() == 4) {
                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Knowledge(readLogicValue(agent), readLogicValue(time),readFormula(formula, variableNames));

            } else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Knowledge(readLogicValue(agent), NOW,readFormula(formula, variableNames));

            }
        }
    }

    // (K! agent time P)
    private static Formula constructPerceives(List list, Set<String> variableNames) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Perceieves expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size() != 3){
            throw new ParsingException("Perceieves expresion has wrong number of arguments! "+list);

        } else {

            if (list.size()==4){
                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Perception(readLogicValue(agent), readLogicValue(time),readFormula(formula, variableNames));

            } else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Perception(readLogicValue(agent), NOW, readFormula(formula, variableNames));

            }

        }
    }
    private static Formula constructDesires(List list, Set<String> variableNames) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Desires expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size() != 3){
            throw new ParsingException("Desires expresion has wrong number of arguments! "+list);

        } else {

            if (list.size()==4){
                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Desire(readLogicValue(agent), readLogicValue(time),readFormula(formula, variableNames));

            } else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Desire(readLogicValue(agent), NOW, readFormula(formula, variableNames));

            }

        }
    }

    // (B! agent time strength P)
    private static Formula constructBelief(List list, Set<String> variableNames) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Belief expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size() != 3){
            throw new ParsingException("Belief expresion has wrong number of arguments! "+list);

        } else {

            if(list.size()==4) {

                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Belief(readLogicValue(agent), readLogicValue(time),readFormula(formula, variableNames));

            }
            else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Belief(readLogicValue(agent), NOW,readFormula(formula, variableNames));

            }
        }
    }

    // (Ought! agent time strength P)
    private static Formula constructOught(List list, Set<String> variableNames) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Ought expresion cannot be empty!");
        }  else if(list.size() != 5 && list.size()!=4){
            throw new ParsingException("Ought expresion has wrong number of arguments! "+list);

        } else {

            if(list.size()==5) {
                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                Object ought =  list.get(4);

                return new Ought(readLogicValue(agent), readLogicValue(time),readFormula(formula, variableNames), readFormula(ought, variableNames));

            } else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                Object ought =  list.get(3);

                return new Ought(readLogicValue(agent), NOW,readFormula(formula, variableNames), readFormula(ought, variableNames));

            }
        }
    }
    // (Says! agent time strength P)
    private static Formula constructSays(List list, Set<String> variableNames) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Says expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size()!=3){
            throw new ParsingException("Says expresion has wrong number of arguments! "+list);

        } else {

            if(list.size()==4) {
                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Says(readLogicValue(agent), readLogicValue(time),readFormula(formula, variableNames));

            } else{

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Says(readLogicValue(agent), NOW,readFormula(formula, variableNames));

            }

        }
    }

    private static Formula constructCommunicates(List list, Set<String> variableNames) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Coomunicates expresion cannot be empty!");
        }  else if(list.size() != 5 && list.size()!=4){
            throw new ParsingException("Says expresion has wrong number of arguments! "+list);

        } else {

            if(list.size()==5) {
                Object agent1 =  list.get(1);
                Object agent2 =  list.get(2);

                Object time =  list.get(3);
                Object formula =  list.get(4);

                return new Communicates(readLogicValue(agent1), readLogicValue(agent2), readLogicValue(time),readFormula(formula, variableNames));

            } else{

                Object agent1 =  list.get(1);
                Object agent2 =  list.get(2);

                Object formula =  list.get(3);

                return new Communicates(readLogicValue(agent1), readLogicValue(agent2), NOW,readFormula(formula, variableNames));

            }

        }
    }

    // (CommonUtils!  time  P)
    private static Formula constructCommon(List list, Set<String> variableNames) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("CommonUtils expresion cannot be empty!");
        }  else if(list.size() != 3 && list.size()!=2){
            throw new ParsingException("CommonUtils expresion has wrong number of arguments! "+list);

        } else {

            if(list.size() == 3) {
                Object time =  list.get(1);
                Object formula =  list.get(2);
                return new Common(readLogicValue(time),readFormula(formula, variableNames));
            } else {

                Object formula =  list.get(1);
                return new Common(NOW,readFormula(formula, variableNames));

            }
        }
    }


    private static Formula constructPredicate(List list, Set<String> variableNames) throws ParsingException {

        if(list.isEmpty()){
            throw new ParsingException("Predicate expresion cannot be empty!");
        }
        else{

            Object nameObject = list.get(0);

            if(nameObject instanceof Symbol){

                String name =  ((Symbol) nameObject).getName();

                if(name.startsWith("$")){
                    throw new AssertionError("Atom and predicate names cannot start with a $: "+ name);
                }

                Value[] values = new Value[list.size()-1];

                for(int i = 1; i< list.size() ; i++){

                    values[i-1] =  readLogicValue(list.get(i), variableNames);
                }

                return new Predicate(name, values);

            } else {
                throw new ParsingException("Name of predicate should be a string! " + nameObject);

            }

        }
    }

    private static Formula constructQuantifier(List list, boolean universal, Set<String> outerVariableNames) throws ParsingException {
        if(list.size()==3){

            Object varListObject = list.get(1);
            if(varListObject instanceof List){
                List listOfVars = (List) varListObject;
                Variable[] variables = new Variable[((List) varListObject).size()];

                Set<String> variableNames = Sets.newSet();
                for(int i = 0; i<variables.length; i++){
                    Object varObject = listOfVars.get(i);
                    if(varObject instanceof Symbol){

                        variables[i] = new Variable(((Symbol) varObject).getName());

                        variableNames.add(((Symbol) varObject).getName());
                    } else{

                        throw new ParsingException("Variable should be a string: " + varObject);

                    }

                }
                Set<String> conflicts = Sets.intersection(outerVariableNames,variableNames);
                if(!conflicts.isEmpty()){
                    throw new ParsingException("This quantifier's variables appear in the outer scope: " + list+ ". Conflicting vars: " + conflicts);
                }
                Formula argument = readFormula(list.get(2), Sets.union(outerVariableNames,variableNames));
                return universal? new Universal(variables, argument): new Existential(variables, argument) ;

            } else if (varListObject instanceof Symbol){

                Variable[] variables = new Variable[1];

                variables[0] = new Variable(((Symbol) varListObject).getName());

                Set<String> variableNames = Sets.with(((Symbol) varListObject).getName());

                Set<String> conflicts = Sets.intersection(outerVariableNames,variableNames);
                if(!conflicts.isEmpty()){
                    throw new ParsingException("This quantifier's variables appear in the outer scope: " + list+ ". Conflicting vars: " + conflicts);
                }
                Formula argument = readFormula(list.get(2), Sets.union(outerVariableNames,variableNames));
                return universal? new Universal(variables, argument): new Existential(variables, argument) ;


            } else {

                throw new ParsingException("The variable list for this quantifier formula is not valid: "+ list );

            }


        } else{

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
        for(int i = 0; i < tokens.length; i++){

            if(tokens[i].isEmpty()){
                continue;
            }
            stringBuilder.append(" " + tokens[i].replace(" ", "") +" ");


            if(tokens[i].equals("(")){
                parens.push("");
            }
            if(tokens[i].equals(")") && parens.empty()){
                throw new ParsingException("Extra )");
            }


            if(tokens[i].equals(")") && !parens.empty()){
                parens.pop();
            }



            if(parens.isEmpty()){
                forms.add(stringBuilder.toString());
                stringBuilder = new StringBuilder();

            }


        }

        return forms;
    }


    public static Object readFromString(String s){

        Parseable parseable = Parsers.newParseable(s);
        Parser parser = Parsers.newParser(defaultConfiguration());

        return parser.nextValue(parseable);
    }



}
