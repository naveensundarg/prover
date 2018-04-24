package com.naveensundarg.shadow.prover.generators;

import com.naveensundarg.shadow.prover.representations.formula.Atom;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Not;
import com.naveensundarg.shadow.prover.representations.formula.Or;
import com.naveensundarg.shadow.prover.utils.Pair;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class Vectorizer {

    private GeneratorParams generatorParams;

    public Vectorizer(GeneratorParams generatorParams){

        this.generatorParams = generatorParams;
    }
    public void  vectorizePropositionalProblems( List<Pair<List<Formula>, Boolean>> problems, String fileName) throws IOException {


        String dummy = "";
        for(int i =0 ; i < generatorParams.maxAtoms -1; i++){
            dummy = dummy + "0,";
        }

        dummy = dummy.substring(0,dummy.length()-1);

        StringBuilder stringBuilder = new StringBuilder();

        int id = 0;
        for(Pair<List<Formula>, Boolean> problem: problems){



            for(Formula formula: problem.first()){


                stringBuilder.append("P" + id + "," +  vectorizePropositionalFormula(formula));
                stringBuilder.append("\n");
            }


            stringBuilder.append("P" + id + "," + dummy + "," + (problem.second()? 0 : 1));
            stringBuilder.append("\n");


            id++;

        }




        Files.write(Paths.get(fileName), stringBuilder.toString().getBytes());



    }


    public String vectorizePropositionalFormula(Formula formula){

        Or or = (Or) formula;
        StringBuilder stringBuilder = new StringBuilder();

        Set<Formula> literals = Arrays.stream(((Or) formula).getArguments()).collect(Collectors.toSet());

        for (int i = 0; i<generatorParams.maxAtoms; i++){

            Atom atom = new Atom(Names.NAMES[i]);

            if(literals.contains(atom)){

                stringBuilder.append("1");
            } else if (literals.contains(new Not(atom))){

                stringBuilder.append("-1");


            } else {
                stringBuilder.append("0");

            }

            stringBuilder.append(",");
        }

        String ans = stringBuilder.toString();
        return ans.substring(0, ans.length() - 1);
    }
}
