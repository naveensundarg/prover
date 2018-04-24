package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;
import com.naveensundarg.shadow.prover.utils.Reader;
import org.apache.commons.lang3.tuple.Pair;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.LispObject;

import java.io.*;
import java.net.*;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Created by naveensundarg on 12/4/16.
 */
public class SnarkWrapper implements Prover {


    private static AtomicBoolean local = new AtomicBoolean(true);

    public static boolean isLocal() {
        return local.get();
    }

    public static void setLocal(boolean local) {
        SnarkWrapper.local.set(local);
    }

    private  static Interpreter interpreter;

    private SnarkWrapper() {

    }

    private static final SnarkWrapper INSTANCE;

    static {
        INSTANCE = new SnarkWrapper();
    }

    public static SnarkWrapper getInstance() {

        return INSTANCE;
    }

    static {


        if (local.get()) {

            interpreter = Interpreter.createInstance();
            LispObject result = interpreter.eval("(load \"snark-20120808r02/snark-system.lisp\")");


            result = interpreter.eval("(make-snark-system)");


            result = interpreter.eval("(load \"snark-20120808r02/snark-interface.lisp\")");
            result = interpreter.eval("(load \"snark-20120808r02/commons.lisp\")");


        } else {
            interpreter = null;
        }


    }


    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {


        String assumptionsListString = assumptions.stream().map(Formula::toString).reduce("'(", (x, y) -> x + " " + y) + ") ";
        String goalString = "'" + formula.toString();

        assumptionsListString = assumptionsListString.replace("\n", "").replace("\r", "");
        goalString = goalString.replace("\n", "").replace("\r", "");

        String resultString = "";
        if (local.get()) {

            synchronized (interpreter) {


                LispObject result = interpreter.eval("(prove-from-axioms-yes-no " + assumptionsListString + goalString + " :verbose nil)");

                resultString = result.toString();
            }
        } else {

            String url = null;
            try {
                url = "http://localhost:8000/prove?assumptions=" + URLEncoder.encode(assumptionsListString, "UTF-8") + "&goal=" + URLEncoder.encode(goalString, "UTF-8");
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
            URL proverURL = null;
            try {
                proverURL = new URL(url);


            } catch (MalformedURLException e) {
                e.printStackTrace();
            }
            BufferedReader in = null;
            try {
                in = new BufferedReader(
                        new InputStreamReader(proverURL.openStream()));
            } catch (IOException e) {
                e.printStackTrace();
            }

            String inputLine = null;
            try {
                while ((inputLine = in.readLine()) != null) {

                    resultString = resultString + inputLine;
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                in.close();
            } catch (IOException e) {
                e.printStackTrace();
            }


        }

        if (resultString.equals("YES")) {
            return Optional.of(new TrivialJustification(assumptions, formula, ":FOLFromSnark"));
        } else {
            return Optional.empty();
        }


    }


    @Override
    public Optional<Value> proveAndGetBinding(Set<Formula> assumptions, Formula formula, Variable variable) {


        String assumptionsListString = assumptions.stream().map(Formula::toString).reduce("'(", (x, y) -> x + " " + y) + ") ";
        String goalString = "'" + formula.toString();

        assumptionsListString = assumptionsListString.replace("\n", "").replace("\r", "");
        goalString = goalString.replace("\n", "").replace("\r", "");

        String resultString = "";
        if (local.get()) {

            synchronized (interpreter) {


                LispObject result = interpreter.eval("(prove-from-axioms-and-get-answer " + assumptionsListString + goalString + " '" + variable.toString() + " :verbose nil)");

                resultString = result.toString();
            }
        } else {

            String url = null;
            try {
                url = "http://localhost:8000/prove?assumptions=" + URLEncoder.encode(assumptionsListString, "UTF-8") + "&goal=" + URLEncoder.encode(goalString, "UTF-8");
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
            URL proverURL = null;
            try {
                proverURL = new URL(url);


            } catch (MalformedURLException e) {
                e.printStackTrace();
            }
            BufferedReader in = null;
            try {
                in = new BufferedReader(
                        new InputStreamReader(proverURL.openStream()));
            } catch (IOException e) {
                e.printStackTrace();
            }

            String inputLine = null;
            try {
                while ((inputLine = in.readLine()) != null) {

                    resultString = resultString + inputLine;
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                in.close();
            } catch (IOException e) {
                e.printStackTrace();
            }


        }

        if (resultString.isEmpty()) {
            return Optional.empty();
        } else {


            try {
                return Optional.of(Reader.readLogicValueFromString(resultString));
            } catch (Reader.ParsingException e) {
                e.printStackTrace();
                return Optional.empty();
            }
        }


    }

    @Override
    public Optional<Map<Variable, Value>> proveAndGetBindings(Set<Formula> assumptions, Formula formula, List<Variable> variables) {


        String varListString = "(" + variables.stream().map(Variable::toString).reduce(" ", (x, y) -> x + " " + y) + ")";

        String assumptionsListString = assumptions.stream().map(Formula::toString).reduce("'(", (x, y) -> x + " " + y) + ") ";
        String goalString = "'" + formula.toString();

        assumptionsListString = assumptionsListString.replace("\n", "").replace("\r", "");
        goalString = goalString.replace("\n", "").replace("\r", "");

        String resultString = "";
        if (local.get()) {

            synchronized (interpreter) {


                LispObject result = interpreter.eval("(prove-from-axioms-and-get-answers " + assumptionsListString + goalString + " '" + varListString + " :verbose nil)");

                resultString = result.toString();
            }
        } else {

            String url = null;
            try {
                url = "http://localhost:8000/prove?assumptions=" + URLEncoder.encode(assumptionsListString, "UTF-8") + "&goal=" + URLEncoder.encode(goalString, "UTF-8");
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
            URL proverURL = null;
            try {
                proverURL = new URL(url);


            } catch (MalformedURLException e) {
                e.printStackTrace();
            }
            BufferedReader in = null;
            try {
                in = new BufferedReader(
                        new InputStreamReader(proverURL.openStream()));
            } catch (IOException e) {
                e.printStackTrace();
            }

            String inputLine = null;
            try {
                while ((inputLine = in.readLine()) != null) {

                    resultString = resultString + inputLine;
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                in.close();
            } catch (IOException e) {
                e.printStackTrace();
            }


        }

        if (resultString.isEmpty()) {
            return Optional.empty();
        } else {

            if (resultString.toLowerCase().equals("nil")) {

                return Optional.of(CollectionUtils.newMap());
            }

            try {

                List<?> resultLst = (List<?>) Reader.readFromString(resultString);


                if (resultLst.size() != variables.size()) {

                    return Optional.empty();
                }

                Map<Variable, Value> variableValueMap = CollectionUtils.newMap();

                for (int i = 0; i < variables.size(); i++) {

                    variableValueMap.put(variables.get(i), Reader.readLogicValue(resultLst.get(i)));
                }

                return Optional.of(variableValueMap);

            } catch (Reader.ParsingException e) {
                e.printStackTrace();
                return Optional.empty();
            }
        }


    }


    @Override
    public Optional<Pair<Justification, Set<Map<Variable, Value>>>> proveAndGetMultipleBindings(Set<Formula> assumptions, Formula formula, List<Variable> variables) {


        String varListString = "(" + variables.stream().map(Variable::toString).reduce(" ", (x, y) -> x + " " + y) + ")";

        String assumptionsListString = assumptions.stream().map(Formula::toString).reduce("'(", (x, y) -> x + " " + y) + ") ";
        String goalString = "'" + formula.toString();

        assumptionsListString = assumptionsListString.replace("\n", "").replace("\r", "");
        goalString = goalString.replace("\n", "").replace("\r", "");

        String resultString = "";
        if (local.get()) {

            synchronized (interpreter) {


                LispObject result = interpreter.eval("(prove-from-axioms-and-get-multiple-answers " + assumptionsListString + goalString + " '" + varListString + " :verbose nil)");

                resultString = result.toString();
            }
        } else {

            String url = null;
            try {
                url = "http://localhost:8000/prove?assumptions=" + URLEncoder.encode(assumptionsListString, "UTF-8") + "&goal=" + URLEncoder.encode(goalString, "UTF-8");
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
            URL proverURL = null;
            try {
                proverURL = new URL(url);


            } catch (MalformedURLException e) {
                e.printStackTrace();
            }
            BufferedReader in = null;
            try {
                in = new BufferedReader(
                        new InputStreamReader(proverURL.openStream()));
            } catch (IOException e) {
                e.printStackTrace();
            }

            String inputLine = null;
            try {
                while ((inputLine = in.readLine()) != null) {

                    resultString = resultString + inputLine;
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                in.close();
            } catch (IOException e) {
                e.printStackTrace();
            }


        }

        if (resultString.isEmpty()) {
            return Optional.empty();
        } else {

            if (resultString.toLowerCase().equals("(nil)")) {

                return Optional.of(Pair.of(null, CollectionUtils.newEmptySet()));
            }

            try {

                List<?> answerList = (List<?>) Reader.readFromString(resultString);

                Set<Map<Variable, Value>> answers = Sets.newSet();

                for (Object ans : answerList) {

                    List<?> resultLst = (List<?>) ans;
                    if (resultLst.size() != variables.size()) {

                        return Optional.empty();
                    }

                    Map<Variable, Value> variableValueMap = CollectionUtils.newMap();

                    for (int i = 0; i < variables.size(); i++) {

                        variableValueMap.put(variables.get(i), Reader.readLogicValue(resultLst.get(i)));
                    }

                    answers.add(variableValueMap);
                }


                Justification trivialJustification = TrivialJustification.trivial(assumptions, formula);

                return Optional.of(Pair.of(trivialJustification, answers));

            } catch (Reader.ParsingException e) {
                e.printStackTrace();
                return Optional.empty();
            }
        }


    }


}
