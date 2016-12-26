package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.utils.Reader;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;

import static guru.nidi.graphviz.model.Factory.*;

import java.io.File;
import java.util.List;
import java.util.Set;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception {

       System.out.println( Reader.readFormula(Reader.read("(forall (?x ?y) (if (not (= ?x ?y)) (not (= (+ ?x 1) (+ ?y 1)))))")));

    }
}
