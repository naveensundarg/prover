package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Reader;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import static guru.nidi.graphviz.model.Factory.*;

import java.io.File;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Set;

import static us.bpsm.edn.Keyword.newKeyword;
import static us.bpsm.edn.parser.Parsers.defaultConfiguration;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {

    public static void main(String[] args) throws Exception {

        Parseable pbr = Parsers.newParseable("(implies P Q)");
        Parser p = Parsers.newParser(defaultConfiguration());
       Formula f = ( Reader.readFormula(p.nextValue(pbr)));


       System.out.println(Reader.readFormulaFromString("(forall (t') (P t))"));
    }
}
