package com.naveensundarg.shadow.prover;

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


        LispObject result  = interpreter.eval("(load \"src/main/resources/com/naveensundarg/shadow/prover/snark-20120808r02/snark-system.lisp\")");

        System.out.println(result);

        result = interpreter.eval("(make-snark-system)");


        System.out.println(result);

        result = interpreter.eval("(load \"src/main/resources/com/naveensundarg/shadow/prover/snark-20120808r02/snark-interface.lisp\")");
        System.out.println(result);

        result = interpreter.eval("(prove-from-axioms-yes-no '((implies P Q) R) 'P :verbose nil)");

        System.out.println(result);



    }

}
