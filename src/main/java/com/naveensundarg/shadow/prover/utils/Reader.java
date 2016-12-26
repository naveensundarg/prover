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
    private static final Symbol IFF = Symbol.newSymbol("iff");

    private static final Symbol EXISTS = Symbol.newSymbol("exists");
    private static final Symbol FORALL = Symbol.newSymbol("forall");

    private static final Symbol BELIEVES = Symbol.newSymbol("Believes!");
    private static final Symbol KNOWS = Symbol.newSymbol("Knows!");
    private static final Symbol PERCEIVES = Symbol.newSymbol("Perceives!");

    private static final Symbol COMMON = Symbol.newSymbol("Common!");
    private static final Symbol SAYS = Symbol.newSymbol("Says!");
    private static final Symbol OUGHT = Symbol.newSymbol("Ought!");

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


    public static Value readLogicValue(Object input) throws ParsingException{




        if(input instanceof List){


            List list = (List) input;

            if(list.size()< 2){
                throw new ParsingException("Compound value should have atleast one argument");

            }

            Object nameObject = list.get(0);

            if(nameObject instanceof Symbol){
                String name =  ((Symbol) nameObject).getName();
                Value[] arguments = new Value[list.size()-1];

                for(int i = 1; i< list.size(); i++){

                    arguments[i-1] = readLogicValue(list.get(i));
                }
                return new Compound(name, arguments);
            }
            throw new ParsingException("name should be a string" + nameObject);

        }
        else {
            String name =  input.toString();
            return name.startsWith("?") ? new Variable(name): new Constant(name);
        }

    }


    public static Formula readFormula(Object input) throws ParsingException {

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
                        return new Not(readFormula(list.get(1)));

                    } else {
                        throw new ParsingException("Not should have one argument");
                    }
                }

                if(name.equals(IF)){
                    if(list.size()==3){
                        return new Implication(readFormula(list.get(1)),readFormula(list.get(2)));

                    } else {
                        throw new ParsingException("If should have two arguments");
                    }
                }

                if(name.equals(IFF)){
                    if(list.size()==3){
                        return new BiConditional(readFormula(list.get(1)),readFormula(list.get(2)));

                    } else {
                        throw new ParsingException("Iff should have two arguments");
                    }
                }

                if(name.equals(OR)){
                    if(list.size()>=3){

                        Formula[] subs = new Formula[list.size()-1];

                        for(int i = 1 ; i < list.size(); i++){
                            subs[i-1] = readFormula(list.get(i));
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
                            subs[i-1] = readFormula(list.get(i));
                        }

                        return new And(subs);


                    } else {
                        throw new ParsingException("And should have at least two arguments");
                    }
                }
                //FORALL
                if(name.equals(FORALL)){

                    return constructQuantifier(list, true);

                }

                //FORALL
                if(name.equals(EXISTS)){

                    return constructQuantifier(list, false);

                }

                //Believes
                if(name.equals(BELIEVES)){

                    return constructBelief(list);
                }

                //Knows
                if(name.equals(KNOWS)){

                    return constructKnowledge(list);
                }

                //Perceives
                if(name.equals(PERCEIVES)){

                    return constructPerceives(list);
                }
                if(name.equals(COMMON)){

                    return constructCommon(list);
                }
                if(name.equals(SAYS)){

                    return constructSays(list);
                }

                if(name.equals(OUGHT)){

                    return constructOught(list);
                }

                return constructPredicate(list);


            }

        }

         throw new AssertionError("Could not understand formula: " + input);

    }

    // (K! agent time P)
    private static Formula constructKnowledge(List list) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Knowledge expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size() != 3){
            throw new ParsingException("Knowledge expresion has wrong number of arguments! "+list);

        } else {

            if(list.size() == 4) {
                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Knowledge(readLogicValue(agent), readLogicValue(time),readFormula(formula));

            } else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Knowledge(readLogicValue(agent), NOW,readFormula(formula));

            }
        }
    }

    // (K! agent time P)
    private static Formula constructPerceives(List list) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Perceieves expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size() != 3){
            throw new ParsingException("Perceieves expresion has wrong number of arguments! "+list);

        } else {

            if (list.size()==4){
                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Perception(readLogicValue(agent), readLogicValue(time),readFormula(formula));

            } else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Perception(readLogicValue(agent), NOW, readFormula(formula));

            }

        }
    }

    // (B! agent time strength P)
    private static Formula constructBelief(List list) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Belief expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size() != 3){
            throw new ParsingException("Belief expresion has wrong number of arguments! "+list);

        } else {

            if(list.size()==4) {

                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Belief(readLogicValue(agent), readLogicValue(time),readFormula(formula));

            }
            else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Belief(readLogicValue(agent), NOW,readFormula(formula));

            }
        }
    }

    // (Ought! agent time strength P)
    private static Formula constructOught(List list) throws ParsingException{
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

                return new Ought(readLogicValue(agent), readLogicValue(time),readFormula(formula), readFormula(ought));

            } else {

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                Object ought =  list.get(3);

                return new Ought(readLogicValue(agent), NOW,readFormula(formula), readFormula(ought));

            }
        }
    }
    // (Says! agent time strength P)
    private static Formula constructSays(List list) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Says expresion cannot be empty!");
        }  else if(list.size() != 4 && list.size()!=3){
            throw new ParsingException("Says expresion has wrong number of arguments! "+list);

        } else {

            if(list.size()==4) {
                Object agent =  list.get(1);
                Object time =  list.get(2);
                Object formula =  list.get(3);

                return new Says(readLogicValue(agent), readLogicValue(time),readFormula(formula));

            } else{

                Object agent =  list.get(1);
                Object formula =  list.get(2);

                return new Says(readLogicValue(agent), NOW,readFormula(formula));

            }

        }
    }

    // (CommonUtils!  time  P)
    private static Formula constructCommon(List list) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("CommonUtils expresion cannot be empty!");
        }  else if(list.size() != 3 && list.size()!=2){
            throw new ParsingException("CommonUtils expresion has wrong number of arguments! "+list);

        } else {

            if(list.size() == 3) {
                Object time =  list.get(1);
                Object formula =  list.get(2);
                return new Common(readLogicValue(time),readFormula(formula));
            } else {

                Object formula =  list.get(1);
                return new Common(NOW,readFormula(formula));

            }
        }
    }


    private static Formula constructPredicate(List list) throws ParsingException {

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

                    values[i-1] =  readLogicValue(list.get(i));
                }

                return new Predicate(name, values);

            } else {
                throw new ParsingException("Name of predicate should be a string! " + nameObject);

            }

        }
    }

    private static Formula constructQuantifier(List list, boolean universal) throws ParsingException {
        if(list.size()==3){

            Object varListObject = list.get(1);
            if(varListObject instanceof List){
                List listOfVars = (List) varListObject;
                Variable[] variables = new Variable[((List) varListObject).size()];

                for(int i = 0; i<variables.length; i++){
                    Object varObject = listOfVars.get(i);
                    if(varObject instanceof Symbol){

                        variables[i] = new Variable(((Symbol) varObject).getName());
                    } else{

                        throw new ParsingException("Variable should be a string: " + varObject);

                    }

                }
                Formula argument = readFormula(list.get(2));
                return universal? new Universal(variables, argument): new Existential(variables, argument) ;

            } else{
                throw new ParsingException("The second object for a quantifier should be a list of variables: "+ list );

            }


        } else{

            throw new ParsingException("quantifiers should have exactly 3 arguments: " + list);

        }
    }


    public static Object read(String input) throws ParsingException {

        input = input.trim();

        List<Object> tree = new ArrayList<>();


        if(input.isEmpty()){
            return tree;
        }

        if(input.startsWith("(") && input.endsWith(")")){

            input = input.substring(1, input.length()-1);

            return extractForms(input).stream().map(x -> {
                try {
                    return read(x);
                } catch (ParsingException e) {
                    e.printStackTrace();
                }
                return null;
            }).collect(Collectors.toList());

        }

        if(input.startsWith("(") &&  !input.endsWith(")")){

            throw new ParsingException("Missing a )");

        }

        if(!input.startsWith("(") && input.endsWith(")")){

            throw new ParsingException("Missing a (");
        }


        String[] tokens = input.split(" ");
        if(tokens.length>1){
            throw new ParsingException("Missing braces");

        }


        return input;

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


    public static void main(String[] args) throws ParsingException {

//        extractForms("a ( b c )").stream().forEach(System.out::println);

        System.out.println(readFormula("(and a b (if p q))"));
    }


}
