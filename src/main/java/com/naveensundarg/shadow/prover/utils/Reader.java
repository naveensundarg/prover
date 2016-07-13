package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.representations.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Reader {

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


        if(input instanceof String){
            String name = (String) input;
            return name.startsWith("?") ? new Variable(name): new Constant(name);
        }

        if(input instanceof List){


            List list = (List) input;

            if(list.size()< 2){
                throw new ParsingException("Compound value should have atleast one argument");

            }

            Object nameObject = list.get(0);

            if(nameObject instanceof String){
                String name = (String) nameObject;
                Value[] arguments = new Value[list.size()-1];

                for(int i = 1; i< list.size(); i++){

                    arguments[i-1] = readLogicValue(list.get(i));
                }
                return new Compound(name, arguments);
            }
            throw new ParsingException("name should be a string" + nameObject);

        }

        throw new ParsingException("Cannot build logic value from this object " + input + "of class "+ input.getClass());
    }


    public static Formula readFormula(Object input) throws ParsingException {

        if(input instanceof String){
            return new Atom((String) input);
        }

        if(input instanceof List){
            List list = (List) input;

            if(list.isEmpty()){
                throw new ParsingException("Can't build a formula");
            }
            Object first = list.get(0);

            if(first instanceof String){
                String name = (String) first;

                if(name.equals("not") ){
                    if(list.size()==2){
                        return new Not(readFormula(list.get(1)));

                    } else {
                        throw new ParsingException("Not should have one argument");
                    }
                }

                if(name.equals("if")){
                    if(list.size()==3){
                        return new Implication(readFormula(list.get(1)),readFormula(list.get(2)));

                    } else {
                        throw new ParsingException("If should have two arguments");
                    }
                }

                if(name.equals("iff")){
                    if(list.size()==3){
                        return new BiConditional(readFormula(list.get(1)),readFormula(list.get(2)));

                    } else {
                        throw new ParsingException("Iff should have two arguments");
                    }
                }

                if(name.equals("or")){
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

                if(name.equals("and")){
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
                if(name.equals("forall")){

                    return constructQuantifier(list, true);

                }

                //FORALL
                if(name.equals("exists")){

                    return constructQuantifier(list, false);

                }

                //Believes
                if(name.equals("Believes!")){

                    return constructBelief(list);
                }

                //Knows
                if(name.equals("Knows!")){

                    return constructKnowledge(list);
                }

                if(name.equals("Common!")){

                    return constructCommon(list);
                }

                return constructPredicate(list);


            }

        }

        return null;

    }

    // (K! agent time P)
    private static Formula constructKnowledge(List list) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Knowledge expresion cannot be empty!");
        }  else if(list.size() != 4){
            throw new ParsingException("Knowledge expresion has wrong number of arguments! "+list);

        } else {

            String operator = (String) list.get(0);
            String agent = (String) list.get(1);
            String time = (String) list.get(2);
            Object formula =  list.get(3);

            return new Knowledge(readLogicValue(agent), readLogicValue(time),readFormula(formula));
        }
    }

    // (B! agent time strength P)
    private static Formula constructBelief(List list) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("Belief expresion cannot be empty!");
        }  else if(list.size() != 4){
            throw new ParsingException("Belief expresion has wrong number of arguments! "+list);

        } else {

            String operator = (String) list.get(0);
            String agent = (String) list.get(1);
            String time = (String) list.get(2);
            Object formula =  list.get(3);

            return new Belief(readLogicValue(agent), readLogicValue(time),readFormula(formula));
        }
    }

    // (CommonUtils!  time  P)
    private static Formula constructCommon(List list) throws ParsingException{
        if(list.isEmpty()){
            throw new ParsingException("CommonUtils expresion cannot be empty!");
        }  else if(list.size() != 3){
            throw new ParsingException("CommonUtils expresion has wrong number of arguments! "+list);

        } else {

            String operator = (String) list.get(0);
            String time = (String) list.get(1);
            Object formula =  list.get(2);

            return new Common(readLogicValue(time),readFormula(formula));
        }
    }


    private static Formula constructPredicate(List list) throws ParsingException {

        if(list.isEmpty()){
            throw new ParsingException("Predicate expresion cannot be empty!");
        }
        else{

            Object nameObject = list.get(0);

            if(nameObject instanceof String){

                String name = (String) nameObject;

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
                    if(varObject instanceof String){

                        variables[i] = new Variable((String) varObject);
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
