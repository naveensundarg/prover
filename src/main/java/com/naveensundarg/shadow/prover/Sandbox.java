package com.naveensundarg.shadow.prover;

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

    public static void main(String[] args) throws Exception{

        System.loadLibrary("gv");
        Graph g = graph("example").directed();

        Node a  = Node.named("a");
        Node b = Node.named("b");
        g.link(a,b);

        a.link(b);

        g.node(a);
       // g.node(b);

        System.out.println(g.getLinks());



        Graphviz.fromGraph(g).renderToFile(new File("example.png"));    }


}
