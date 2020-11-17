package com.naveensundarg.shadow.prover.hyperlog;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.Keyword;
import clojure.lang.PersistentArrayMap;
import com.naveensundarg.shadow.prover.representations.Expression;
import com.naveensundarg.shadow.prover.representations.formula.NamedLambda;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public class Evaluator {

    private static IFn eval_script;
    private static Set<Object> ALLOWED_BASIC_OPS;
    private static Keyword RESULT;

    static {

        IFn require = Clojure.var("clojure.core", "require");
        IFn loadFile = Clojure.var("clojure.core", "load-file");
        loadFile.invoke("hyperlog/tools.clj");
        loadFile.invoke("hyperlog/core.clj");
        require.invoke(Clojure.read("interpreter.core"));
        IFn load_tools = Clojure.var("interpreter.core", "load-tools!");
        load_tools.invoke();


        String[] PRIMITIVE_SYMBOLS = {
                "letfn",
                "=",
                "+", "-", "*", "/", "quot", "rem", "mod",
                "inc", "dec", "max" , "min", "+'", "-'", "*'", "inc'", "dec'",
                "==", "<", ">", "<=", ">=", "compare",
                "zero?", "pos?", "neg?", "even?", "odd?",
                "number?", "rational?", "integer?", "ratio?", "decimal?", "float?",
                "double?", "int?", "nat-int?", "neg-int?", "pos-int?",
                "count", "get", "subs", "compare",
                "clojure.string/join", "clojure.string/escape", "clojure.string/split",
                "clojure.string/split-lines", "clojure.string/replace", "clojure.string/replace-first",
                "reverse", "index-of", "last-index-of", "str"



        };

        ALLOWED_BASIC_OPS = Arrays.stream(PRIMITIVE_SYMBOLS).map(Clojure::read).collect(Collectors.toSet());
        // load the `eval-script`
        eval_script = Clojure.var("interpreter.core", "eval-script");
        RESULT = (Keyword) Clojure.read(":result");

    }
    public static Object evaluate(Set<NamedLambda> definitions, Expression expression){

/**
 *
 * (letfn [(twice [x]
 *            (* x 2))
 *         (six-times [y]
 *            (* (twice y) 3))]
 *   (println "Twice 15 =" (twice 15))
 *   (println "Six times 15 =" (six-times 15)))
 */


        // Load extra operations we can use in our script

        // execute it

        StringBuilder program = new StringBuilder();
        program.append("(letfn [\n");
        Set<Object> allowedOps = Sets.copy(ALLOWED_BASIC_OPS);

        for(NamedLambda namedLambda: definitions) {

            String vars = "[" +  Arrays.stream(namedLambda.vars()).map(Variable::toString).collect(Collectors.joining(" "))  + "]";
            String definition = " (" + namedLambda.getName() + vars + namedLambda.getArgument() + ") ";
            program.append(definition);
            allowedOps.add(Clojure.read(namedLambda.getName()));
            for (int i = 0; i < namedLambda.vars().length; i++){
                allowedOps.add(Clojure.read(namedLambda.vars()[i].toString()));
            }
        }

        program.append("] ");
        program.append(expression.toString());
        program.append(")");

        PersistentArrayMap result =  (PersistentArrayMap) eval_script.invoke(program.toString(), allowedOps);

        return result.get(RESULT);



    }
}
