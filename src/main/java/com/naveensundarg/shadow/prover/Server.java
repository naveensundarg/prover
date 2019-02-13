package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.representations.formula.Formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import com.google.gson.Gson;
import java.util.*;

import static spark.Spark.*;


public final class Server {
    public static void main(String[] args) {
        port(7000);
        get("/", (req, res) -> {
            RequestPayload obj = new Gson().fromJson(req.body(), RequestPayload.class);

            Set<Formula> assumptions = Sets.newSet();
            if (args.length > 1) {
                for (int i = 0; i < obj.assumptions.length; i++) {
                    assumptions.add(Reader.readFormulaFromString(obj.assumptions[i]));
                }
            }

            Formula goal = Reader.readFormulaFromString(obj.goal);
            SnarkWrapper prover = SnarkWrapper.getInstance();
            ArrayList<Variable> variables = new ArrayList<>();
            Optional<Map<Variable, Value>> justification = prover.proveAndGetBindings(assumptions, goal, variables);
            String return_string = "{}";
            if (justification.isPresent()) {
                StringBuilder inner = new StringBuilder();
                for (Variable variable : variables) {
                    inner.append("\"").append(variable.getName()).append("\": \"").append(justification.get().get(variable)).append("\",");
                }
                return_string = "{\"variables\": {" + inner + "}}";
            }
            return return_string;
        });
    }

    public class RequestPayload {
        public String[] assumptions;
        public String goal;
    }
}