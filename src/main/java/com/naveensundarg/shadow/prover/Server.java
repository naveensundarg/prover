package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import com.google.gson.Gson;

import java.util.*;

import static spark.Spark.*;


public final class Server {
    static {
        Prover prover = SnarkWrapper.getInstance();
        try {
            List<Problem> problems = ProblemReader.readFrom(Server.class.getResourceAsStream("firstorder-completness-tests.clj"));
            problems.forEach(problem -> {
                prover.prove(problem.getAssumptions(), problem.getGoal());
            });
        }
        catch (Reader.ParsingException e) {
            e.printStackTrace();
            System.exit(-1);
        }

    }
    public static void main(String[] args) {
        port(7000);

        get("/", (req, res) -> "Hello World!");

        post("/", "application/json", (req, res) -> {
            RequestPayload obj = new Gson().fromJson(req.body(), RequestPayload.class);

            Set<Formula> assumptions = Sets.newSet();
            for (int i = 0; i < obj.assumptions.length; i++) {
                assumptions.add(Reader.readFormulaFromString(obj.assumptions[i]));
            }

            Formula goal = Reader.readFormulaFromString(obj.goal);
            SnarkWrapper prover = SnarkWrapper.getInstance();

            String return_string = "";
            if (obj.method.equals("prove")) {
                Optional<Justification> justification = prover.prove(assumptions, goal);
                return_string = "{\"proved\": " + (justification.isPresent() ? "true" : "false") + "}";
            }
            else if (obj.method.equalsIgnoreCase("proveAndGetBinding")) {
                Variable variable = new Variable(obj.variable);
                Optional<Value> justification = prover.proveAndGetBinding(assumptions, goal, variable);
                StringBuilder inner = new StringBuilder();
                inner.append("\"").append(variable.getName()).append("\": \"");
                justification.ifPresent(value -> inner.append(value.toString()));
                inner.append("\"");
                return_string = "{\"proved\": " + (justification.isPresent() ? "true" : "false") + ", " + inner.toString() + "}";
            }
            else if (obj.method.equals("proveAndGetBindings")) {
                ArrayList<Variable> variables = new ArrayList<>();
                for (String variable : obj.variables) {
                    variables.add(new Variable(variable));
                }
                Optional<Map<Variable, Value>> justification = prover.proveAndGetBindings(assumptions, goal, variables);
                StringBuilder inner = new StringBuilder();
                inner.append("\"variables\": {");
                if (justification.isPresent()) {
                    for (Variable variable : variables) {
                        inner.append("\"").append(variable.getName()).append("\": \"").append(justification.get().get(variable).toString()).append("\"");
                    }
                }
                inner.append("}");
                return_string = "{\"proved\": " + (justification.isPresent() ? "true" : "false") + ", " + inner.toString() + "}";
            }
            return return_string;
        });
    }

    public class RequestPayload {
        public String method;
        public String variable;
        public String[] variables;
        public String[] assumptions;
        public String goal;
    }
}