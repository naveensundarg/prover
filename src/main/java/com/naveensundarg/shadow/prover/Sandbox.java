package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.Converter;
import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.representations.Formula;

import static com.naveensundarg.shadow.prover.utils.Reader.read;
import static com.naveensundarg.shadow.prover.utils.Reader.readFormula;

/**
 * Created by naveensundarg on 4/8/16.
 */
 class Sandbox {

    public static void main(String[] args) throws Exception{
        Formula f;

        f = readFormula(read("(if (forall (?x) (and (P ?x) (exists (?y) (not (Q ?y))))) (and (forall (?x) (P ?x)) (exists (?y) (not (Q ?y)))))"));
        System.out.println(Converter.convertToCNF(f,new Problem()).renameVars(new Problem()));

        System.out.println("--------");
        f = readFormula(read("(if (and (forall (?x) (P ?x)) (exists (?y) (not (Q ?y)))) (forall (?x) (and (P ?x) (exists (?y) (not (Q ?y))))) )"));

        System.out.println(Converter.convertToCNF(f,new Problem()).renameVars(new Problem()));

        System.out.println("--------");
        f = readFormula(read("(iff (forall (?x) (and (P ?x) (exists (?y) (not (Q ?y))))) (and (forall (?x) (P ?x)) (exists (?y) (not (Q ?y)))))"));

       System.out.println(Converter.convertToCNF(f,new Problem()).renameVars(new Problem()));

        System.out.println("--------");

        // System.out.println(Unifier.subUnify(v1, v2));

    }
}
