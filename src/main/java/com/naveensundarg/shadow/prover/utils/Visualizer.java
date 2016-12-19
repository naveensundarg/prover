package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.core.NDRule;
import com.naveensundarg.shadow.prover.core.Node;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
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

        Set<List<String>> lines =getAllNodesInTree(node).stream().map(Visualizer::getNodeDefs).collect(Collectors.toSet());


        StringBuilder builder = new StringBuilder();

        builder.append(prefix + "\n");
        lines.forEach(x->x.stream().forEach(y->builder.append(y+"\n")));
        builder.append(suffix);


        PrintWriter out = new PrintWriter(new FileOutputStream("temp.gz", false), true);

        out.print(builder.toString());

        out.flush();
        out.close();




    }


    private static Set<Node> getAllNodesInTree(Node node){


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

        for (Node parent: node.getParents()){

            lines.add("N"+parent.getId() + " -> " + "R"+id +";");
        }

        return lines;
    }
}
