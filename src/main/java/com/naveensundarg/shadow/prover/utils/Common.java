package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.representations.Formula;

import java.io.InputStream;
import java.util.*;

import static com.naveensundarg.shadow.prover.utils.Reader.extractForms;
import static com.naveensundarg.shadow.prover.utils.Reader.read;
import static com.naveensundarg.shadow.prover.utils.Reader.readFormula;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Common {

    public static String toString(Object[] objects){

        StringBuilder stringBuilder = new StringBuilder();


        for (Object object : objects) {
            stringBuilder.append(object);
            stringBuilder.append(" ");
        }

        return stringBuilder.toString();
    }


    public static List<Pair<Set<Formula>, Formula>> readCases(InputStream testFile) throws Reader.ParsingException {

        List<Pair<Set<Formula>, Formula>> cases = new ArrayList<>();
        Scanner scanner = new Scanner(testFile);

        while(scanner.hasNext()){

            String line = scanner.nextLine();

            while(line!=null){
                if(line.startsWith("\"begin")){

                    Set<Formula> assumptions = Sets.newSet();
                    Formula formula = null;
                    line = scanner.nextLine();
                    while(!line.startsWith("\"end")){

                        List<String> parts = extractForms(line);
                        if(parts.get(0).equals(" \"assumption\" ")){

                            assumptions.add(readFormula(read(parts.get(1))));
                        }

                        if(parts.get(0).equals(" \"goal\" ")){

                            formula = (readFormula(read(parts.get(1))));
                        }


                        line = scanner.nextLine();
                    }

                    cases.add(ImmutablePair.from(assumptions,formula));

                }

                if(scanner.hasNext())
                line = scanner.nextLine();
                else
                    break;;
            }


        }

        return cases;


    }

    public static int maxLevel(Formula[] formulas){

        OptionalInt optionalInt = Arrays.stream(formulas).mapToInt(Formula::getLevel).max();

        return optionalInt.orElseGet(()->0);
    }



}
