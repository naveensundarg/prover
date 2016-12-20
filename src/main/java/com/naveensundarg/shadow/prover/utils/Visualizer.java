package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.core.NDRule;
import com.naveensundarg.shadow.prover.core.Node;

import java.io.*;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 12/15/16.
 */
public class Visualizer {



    private static String prefix = "digraph G {";
    private static String suffix = "}";

    public static void renderToDot(Node node) throws FileNotFoundException {

        Map<Integer, Node> usedNodes = CollectionUtils.newMap();

        node.id = 0;
        Set<List<String>> lines = getNodeMaps(node).entrySet().stream().map(e->getNodeDefs(e.getValue())).collect(Collectors.toSet());


        StringBuilder builder = new StringBuilder();

        builder.append(prefix + "\n");
        lines.forEach(x->x.stream().forEach(y->builder.append(y+"\n")));
        builder.append(suffix);


        PrintWriter out = new PrintWriter(new FileOutputStream("temp.gz", false), true);

        out.print(builder.toString());

        out.flush();
        out.close();

        Runtime rt = Runtime.getRuntime();
        try {
            Process pr = rt.exec("dot -Tpdf temp.gz -o graph.pdf");
            rt.exec("open graph.pdf");
        } catch (IOException e) {
            e.printStackTrace();
        }


    }



    public static Map<Integer, Node> getNodeMaps(Node node){

        Map<Integer, Node> map = CollectionUtils.newMap();

        List<Node> ancestors = node.ancestors();


        int smallestSimilarAncestorSize = Integer.MAX_VALUE;
        Node smallestSimilarAncestor = null;
        for(Node ancestor: ancestors){
            if(ancestor.formulaEquals(node.getFormula()) && Sets.subset( ancestor.getDerivedFrom(), node.getDerivedFrom()) && ancestor.ancestors().size() < smallestSimilarAncestorSize){
                smallestSimilarAncestor = ancestor;
                smallestSimilarAncestorSize  = ancestor.ancestors().size();
            }
        }

        if(smallestSimilarAncestor!=null){
            int oldID = node.id;
            node = smallestSimilarAncestor;
            smallestSimilarAncestor.id = oldID;
        }

        map.put(node.getId(), node);

        for(Node parent: node.getParents()){


            map.putAll(getNodeMaps(parent));

        }

        return map;


    }
    public static Set<Node> getAllNodesInTree(Node node){


        return Sets.add(
                node.getParents().stream().map(Visualizer::getAllNodesInTree).reduce(Sets.newSet(), Sets::union), //recurse
                node);
    }


    private static List<String> getNodeDefs(Node node){


        if(node.getNdRule().equals(NDRule.GIVEN)){
            return getGivenNodeDefs(node);
        } else if (node.getNdRule().equals(NDRule.ASSUMPTION)) {
            return getAssumptionNodeDefs(node);
        } else {
            return getNormalNodeDefs(node);
        }




    }

    private static List<String> getGivenNodeDefs(Node node){

        int id = node.getId();
        String line1 =  "N"+id+ "[label= <<FONT COLOR=\"white\">"+  node.getFormula() + "</FONT> >];";
        String line2 = "N" +id+"[shape=box; style=filled, color=\"darkgreen\"];";

        List<String> lines =  CollectionUtils.newEmptyList();
        lines.add(line1);
        lines.add(line2);

        for (Node parent: node.getParents()){

            lines.add("N"+parent.getId() + " -> " + "R"+id +";");
        }

        return lines;
    }

    private static List<String> getAssumptionNodeDefs(Node node){

        int id = node.getId();
        String line1 =  "N"+id+ "[label= <<FONT COLOR=\"white\">"+  node.getFormula() + "</FONT> >];";
        String line2 = "N" +id+"[shape=box; style=filled, color=\"orange\"];";

        List<String> lines =  CollectionUtils.newEmptyList();
        lines.add(line1);
        lines.add(line2);

        for (Node parent: node.getParents()){

            lines.add("N"+parent.getId() + " -> " + "R"+id +";");
        }

        return lines;
    }

    private static List<String> getNormalNodeDefs(Node node){


        int id = node.getId();
        String line1 =  "N"+id+ "[label=< <FONT>"+  node.getFormula() + "</FONT>  <BR/> <FONT COLOR=\"gray\"><B>" +
                node.getDerivedFrom()+
                "</B></FONT>>];";
        String line2 = "N" +id+"[shape=box];";
        String line3 = "R"+id+"[label=\"" + node.getNdRule()+ "\"];";

        String line4 = "R"+id + " -> " + "N"+id +";";
        String line5 = "R" +id+"[shape=oval,style=filled,color=\".7 .3 1.0\"];";
        String line7 = "R" +id+"[margin=.05];";

        List<String> lines =  CollectionUtils.newEmptyList();
        lines.add(line1);
        lines.add(line2);
        lines.add(line3);
        lines.add(line4);
       lines.add(line5);
      //  lines.add(line6);
        lines.add(line7);

        if (node.id == 0) {
            lines.add("N" +id+"[peripheries=2, color=\"darkgreen\"];");
        }


        for (Node parent: node.getParents()){

            lines.add("N"+parent.getId() + " -> " + "R"+id +";");
        }

        return lines;
    }
}
