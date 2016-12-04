package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.LispObject;

import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 12/4/16.
 */
public class SnarkWrapper implements Prover {



    private final static Interpreter interpreter;
    static {


        interpreter = Interpreter.createInstance();
        LispObject result  = interpreter.eval("(load \"src/main/resources/com/naveensundarg/shadow/prover/snark-20120808r02/snark-system.lisp\")");

        System.out.println(result);

        result = interpreter.eval("(make-snark-system)");


        System.out.println(result);

        result = interpreter.eval("(load \"src/main/resources/com/naveensundarg/shadow/prover/snark-20120808r02/snark-interface.lisp\")");
        System.out.println(result);

    }


    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {
        synchronized (interpreter) {

            String assumptionsListString = assumptions.stream().map(x->x.toString()).reduce("'(", (x,y) -> x+ " " +y) +") ";
            String goalString = "'" +  formula.toString();

            LispObject result = interpreter.eval("(prove-from-axioms-yes-no " + assumptionsListString +  goalString+ " :verbose nil)");

            if(result.toString().equals("YES")) {
                return Optional.of(new TrivialJustification(formula));
            }
            else {
                return Optional.empty();
            }

        }



    }



}
