package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.special.KnightsKnavesProver;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Quantifier;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Logic;
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

        SnarkWrapper snarkWrapper = new SnarkWrapper();

        Formula b1 = Reader.readFormulaFromString("(forall [?x] (or (Knight ?x) (Knave ?x)))");
        Formula b2 = Reader.readFormulaFromString("(forall [?x] (not (and (Knight ?x) (Knave ?x))))");
        Formula b3 = Reader.readFormulaFromString("(forall [?x ?p] (if (and (Knight ?x) (Says ?p)) (Holds ?p)))");


        Formula b4 = Reader.readFormulaFromString("");

/*

        KnightsKnavesProver knightsKnavesProver = new KnightsKnavesProver();

        Formula b1 = Reader.readFormulaFromString("(forall [?x] (or (Knight ?x) (Knave ?x)))");
        Formula b2 = Reader.readFormulaFromString("(forall [?x] (not (and (Knight ?x) (Knave ?x))))");

        Formula f1 = Reader.readFormulaFromString("(Says! A (Knave B))");
        Formula f2 = Reader.readFormulaFromString("(Says! A (Knave A))");

        Formula h1 = Reader.readFormulaFromString("(Says! A (iff (Knave A) (Knave B)))");
        Formula h2 = Reader.readFormulaFromString("(Says! B (iff (Knave A) (Knight B)))");

        Set<Formula> base = CollectionUtils.newEmptySet();

        base.add(b1);
        base.add(b2);

        base.add(h1);
        base.add(h2);

        System.out.println(knightsKnavesProver.prove(base, Reader.readFormulaFromString("(Knave A)")));

*/


    }
}
