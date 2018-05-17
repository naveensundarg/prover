package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Universal;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Sets;
import org.armedbear.lisp.*;

import java.util.Set;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class LispSandbox {

    public static void main(String[] args) throws Exception {

        Interpreter interpreter = Interpreter.createInstance();


        LispObject result  = interpreter.eval("(load \"./snark-20120808r02/snark-system.lisp\")");

        System.out.println(result);

        result = interpreter.eval("(make-snark-system)");


        System.out.println(result);

        result = interpreter.eval("(load \"./snark-20120808r02/snark-interface.lisp\")");
        System.out.println(result);

        String s = "(prove-from-axioms-and-get-answer '((man socrates) (forall ?x (implies (man ?x) (mortal ?x))))" +
                " '(mortal ?x)  '?x)";
        System.out.println(s);
        result = interpreter.eval(s);

        System.out.println(result);



    }

}
