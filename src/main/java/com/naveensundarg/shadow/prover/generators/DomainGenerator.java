package com.naveensundarg.shadow.prover.generators;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

public class DomainGenerator {


    private static int MAX_DOMAIN_OBJECTS_1 = 10;
    private static int MIN_DOMAIN_OBJECTS_1 = 5;

    private static int MAX_DOMAIN_OBJECTS_2 = 5;
    private static int MIN_DOMAIN_OBJECTS_2 = 2;

    private static double CONNECTIVITY_THRESHOLD = 0.5;
    private static double COMMUTATIVITY_THRESHOLD = 0.9;
    private static double TRANSITIVITY_THRESHOLD = 0.9;

    public static void main(String[] args){


        int totalObjects1 = totalDomainObjects(1);
        int totalObjects2 = totalDomainObjects(2);

        String type1 = Names.TYPES[ThreadLocalRandom.current().nextInt(0, Names.TYPES.length)];
        String type2 = Names.TYPES[ThreadLocalRandom.current().nextInt(0, Names.TYPES.length)];

        String binaryRelation1 = Names.BINARY_RELATIONS[ThreadLocalRandom.current().nextInt(0, Names.BINARY_RELATIONS.length)];
        String binaryRelation2 = Names.BINARY_RELATIONS[ThreadLocalRandom.current().nextInt(0, Names.BINARY_RELATIONS.length)];
        String binaryRelation3 = Names.BINARY_RELATIONS[ThreadLocalRandom.current().nextInt(0, Names.BINARY_RELATIONS.length)];

        generateTypeStatements(type1, totalObjects1 ).forEach(System.out::println);
        generateTypeStatements(type2, totalObjects1 ).forEach(System.out::println);

        generateGraphForObjectType( totalObjects1, binaryRelation1, false ).forEach(System.out::println);
        generalizedFormulae(type1, binaryRelation1).forEach(System.out::println);

        generalizedFormulae(type2, binaryRelation2).forEach(System.out::println);
        generateGraphForObjectType( totalObjects2, binaryRelation2, false ).forEach(System.out::println);

        generateGraphForObjectType2(totalObjects2, totalObjects2, binaryRelation3, false ).forEach(System.out::println);

    }


    private static int totalDomainObjects(int type){
        if(type == 1){
            return ThreadLocalRandom.current().nextInt(MIN_DOMAIN_OBJECTS_1, MAX_DOMAIN_OBJECTS_1);

        }
        if(type == 2){
            return ThreadLocalRandom.current().nextInt(MIN_DOMAIN_OBJECTS_2, MAX_DOMAIN_OBJECTS_2);

        }

        throw new AssertionError("unknown type");
    }


    private static boolean booleanWith(double threshold){
        return ThreadLocalRandom.current().nextDouble(0.0, 1.0f) < threshold;
    }



    private static Set<Formula> generalizedFormulae(String type1, String binaryRelation1){

        Set<Formula> generalizedFormulae = CollectionUtils.newEmptySet();

        if(booleanWith(COMMUTATIVITY_THRESHOLD)){

            try {
                generalizedFormulae.add(Reader.readFormulaFromString(String.format("(forall [?x ?y] (if (and (%s ?x) (%s ?y) )  (if (%s ?x ?y) (%s ?y ?x))))",
                        type1, type1, type1,
                        binaryRelation1, binaryRelation1)));
            } catch (Reader.ParsingException e) {
                e.printStackTrace();
            }
        }

        if(booleanWith(TRANSITIVITY_THRESHOLD)){

            try {
                generalizedFormulae.add(Reader.readFormulaFromString(String.format("(forall [?x ?y ?z]  (if (and (%s ?x) (%s ?y) (%s ?z)) (if (and (%s ?x ?y) (%s ?y ?z)) (%s ?x ?z))))",
                        type1, type1, type1,
                        binaryRelation1, binaryRelation1, binaryRelation1)));
            } catch (Reader.ParsingException e) {
                e.printStackTrace();
            }
        }

        return generalizedFormulae;
    }
    private static Set<Formula> generateGraphForObjectType(int total, String binaryRelation, boolean canConnectToSelf){

        Set<Formula> connectivity = CollectionUtils.newEmptySet();

        IntStream.range(0, total).forEach(i -> IntStream.range(0, total).forEach(j ->{


            if((i == j && canConnectToSelf) || i!=j) {

                if(booleanWith(CONNECTIVITY_THRESHOLD)){
                    try {
                        connectivity.add(Reader.readFormulaFromString("(" + binaryRelation + " object" +i  + " object" +j +")" ));
                    } catch (Reader.ParsingException e) {
                        e.printStackTrace();
                    }
                }
            }

        }));

        return connectivity;
    }

    private static Set<Formula> generateGraphForObjectType2(int total1, int total2, String binaryRelation, boolean canConnectToSelf){

        Set<Formula> connectivity = CollectionUtils.newEmptySet();

        IntStream.range(0, total1).forEach(i -> IntStream.range(0, total2).forEach(j ->{



            if(booleanWith(CONNECTIVITY_THRESHOLD)){
                try {
                    connectivity.add(Reader.readFormulaFromString("(" + binaryRelation + " object" +i  + " object" +j +")" ));
                } catch (Reader.ParsingException e) {
                    e.printStackTrace();
                }
            }

        }));

        return connectivity;
    }
    private static Set<Formula> generateTypeStatements(String type, int total){
        Set<Formula> typeStatements = CollectionUtils.newEmptySet();

        IntStream.range(0, total).forEach(i -> {

            try {
                typeStatements.add(Reader.readFormulaFromString("(" + type   + " object" +i  +")"));
            } catch (Reader.ParsingException e) {
                e.printStackTrace();
            }

        });

        return typeStatements;

    }


}
