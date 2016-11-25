package com.naveensundarg.shadow.prover.core.sortsystem;


import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import us.bpsm.edn.Keyword;
import us.bpsm.edn.Symbol;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 8/27/16.
 */
public class SortSystem {

    private final Ontology ontology;
    private final Map<String, Category[]> declarations;
    private final Set<Functor> functors;

    public SortSystem(Ontology ontology, Map<String, Category[]> declarations) {
        this.ontology = ontology;

        this.declarations = Collections.unmodifiableMap(declarations);

        this.functors = declarations.entrySet().stream().map(stringEntry -> new Functor(stringEntry.getKey(), stringEntry.getValue())).
                collect(Collectors.toSet());


    }

    //Returns true
    public void checkTypeOfArguments(String functorName, Value[] arguments, Category expectedType) {

        if (!declarations.containsKey(functorName)) {

            throw new AssertionError("Undeclared " + functorName);

        } else {


            Category[] declaration = declarations.get(functorName);

            if (declaration.length - 1 != arguments.length) {

                throw new AssertionError(functorName + " needs " +
                        declaration.length + " arguments, but was given " +
                        arguments.length + " arguments");
            }

            //TODO: deuglify this.
            Category actualType = declaration[declaration.length-1];

            if(!expectedType.equals(actualType) && !ontology.isAncestor(expectedType, actualType )) {

                throw new AssertionError("Expected type [" + expectedType + "], but got ["+actualType+"] in "
                + functorName +"(" + Arrays.toString(arguments) + ")");

            }

            for (int i = 0; i < declaration.length-1; i++) {

                Value argument = arguments[i];
                Category declaredCategory = declaration[i];

                checkTypeOfArguments(argument.getName(), argument.getArguments(), declaredCategory);

            }

        }

    }

    public static SortSystem  buildFrom(Map<?,?> sortSystemDefinition) {

        SortSystemBuilder sortSystemBuilder = new SortSystemBuilder(sortSystemDefinition);

        return sortSystemBuilder.build();

    }

    private static final class SortSystemBuilder {


        private static final Keyword DECLARATIONS_KEY = Keyword.newKeyword("declarations");
        private static final Keyword SORTS_KEY = Keyword.newKeyword("sorts");

        private Ontology ontology;
        private Map<String, Category[]> declarations;

        SortSystemBuilder(Map<?,?> sortSystemDefinition) {

            this.ontology = Ontology.initializeOntology((Map<?,?>) sortSystemDefinition.get(SORTS_KEY));
            this.declarations = buildDeclarations((List<?>) sortSystemDefinition.get(DECLARATIONS_KEY));
        }

        SortSystem build(){

            return new SortSystem(ontology, declarations);
        }

        Map<String, Category[]> buildDeclarations(List<?> decls){

            Map<String, Category[]> declarations = CollectionUtils.newMap();
            for (int i = 0; i<decls.size(); i++){

                List<?> decl = (List<?>) decls.get(i);

                if(decl.size()< 4) {
                    throw new AssertionError("Declaration should have atleast 4 elements: "+decl);
                }

                Symbol functor = (Symbol) decl.get(0);

                List<?> inputSymbols = (List<?>) decl.get(1);
                Symbol outputSymbol = (Symbol) decl.get(3);

                Category[] cats = new Category[inputSymbols.size()+1];

                for(int j = 0; j< inputSymbols.size(); j++) {
                    Symbol catSymbol = (Symbol) inputSymbols.get(j);
                    cats[j] = new Category(catSymbol.getName());

                }

                cats[cats.length-1] =new Category(outputSymbol.getName());
                declarations.put(functor.getName(), cats);


            }

            return declarations;
        }

    }
}