package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.Converter;
import com.naveensundarg.shadow.prover.core.FirstOrderResolutionProver;
import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.PropositionalResolutionProver;
import com.naveensundarg.shadow.prover.representations.Formula;
import com.naveensundarg.shadow.prover.utils.Common;
import com.naveensundarg.shadow.prover.utils.Logic;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;
import java.util.Set;

import static com.naveensundarg.shadow.prover.utils.Reader.read;
import static com.naveensundarg.shadow.prover.utils.Reader.readFormula;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception{

        Formula f = readFormula(read("(exists (?x) (and (Man ?x) (Flew-to-Space ?x)))"));
        System.out.println(Converter.convertToCNF(Logic.negated(f),new Problem()));


    }
}
