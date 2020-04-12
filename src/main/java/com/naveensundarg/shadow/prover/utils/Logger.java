package com.naveensundarg.shadow.prover.utils;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Set;

public class Logger {


    ColoredPrinter coloredPrinter;

    private boolean verbose;
    private String indent = "";
    public Logger(){

        this.coloredPrinter = new ColoredPrinter.Builder(1, false)
                .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.BLACK)   //setting format
                .build();
        this.indent = "";

    }

    public boolean isVerbose() {
        return verbose;
    }

    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }

    public void addContext(){
        indent = indent + "\t";
    }

    public void removeContext(){
        indent = indent.substring(0 , indent.length() -1);
    }


    public void expansionLog(String principle, Set<Formula> newSet) {
        if (!verbose) return;

        if (!newSet.isEmpty()) {

            coloredPrinter.print(indent + "Forward Reasoning: ", Ansi.Attribute.BOLD, Ansi.FColor.BLUE, Ansi.BColor.NONE);
            coloredPrinter.print(principle, Ansi.Attribute.BOLD, Ansi.FColor.WHITE, Ansi.BColor.BLUE);
            coloredPrinter.clear();
            coloredPrinter.print(newSet, Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
            coloredPrinter.println("");
        } else {

        }

    }

    public void tryLog(String principle, Formula goal) {
        if (!verbose) return;


        coloredPrinter.print(indent + "Backward Reasoning: ", Ansi.Attribute.BOLD, Ansi.FColor.GREEN, Ansi.BColor.NONE);
        coloredPrinter.print(principle, Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.GREEN);
        coloredPrinter.clear();
        coloredPrinter.print(goal, Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
        coloredPrinter.println("");

    }

    public void tryAgentClosure(Formula formula) {
        if (!verbose) return;

        coloredPrinter.clear();
        coloredPrinter.print(indent + "Trying", Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);

        coloredPrinter.print(" Agent Closure: ", Ansi.Attribute.BOLD, Ansi.FColor.WHITE, Ansi.BColor.BLACK);
        coloredPrinter.clear();
        coloredPrinter.print(formula, Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
        coloredPrinter.println("");
    }
}
