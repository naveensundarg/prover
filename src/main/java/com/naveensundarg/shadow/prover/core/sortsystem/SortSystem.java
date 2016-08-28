package com.naveensundarg.shadow.prover.core.sortsystem;


import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import us.bpsm.edn.Keyword;

import java.util.Collections;
import java.util.Map;

/**
 * Created by naveensundarg on 8/27/16.
 */
public class SortSystem {

    private final Ontology ontology;
    private final Map<String, Category[]> declarations;

    public SortSystem(Ontology ontology, Map<String, Category[]> declarations) {
        this.ontology = ontology;
        this.declarations = Collections.unmodifiableMap(declarations);
    }

    //Returns true
    public void checkMatch(String functorName, Value[] arguments) {

        if (!declarations.containsKey(functorName)) {

            throw new AssertionError("Undeclared " + functorName);

        } else {


            Category[] declaration = declarations.get(functorName);

            if (declaration.length != arguments.length) {

                throw new AssertionError(functorName + " needs " +
                        declaration.length + " arguments, but was given " +
                        arguments.length + " arguments");
            }

            boolean allMatch = true;

            for (int i = 0; i < declaration.length; i++) {

                Value argument = arguments[i];
                Category declaredCategory = declaration[i];

                if (argument.isCompound()) {

                    checkMatch(argument.getName(), argument.getArguments());

                } else {

                    String name = argument.getName();

                    Category[] arg_declaration = declarations.get(name);

                    if (arg_declaration.length != 1) {

                        throw new AssertionError(name + " needs " +
                                declaration.length + " arguments, but was given " +
                                arguments.length + " arguments");
                    }

                    Category arg_category = arg_declaration[0];

                    //TODO: subcategory check;
                    if (ontology.isAncestor(declaredCategory, arg_category)) {

                        throw new AssertionError("Type mismatch at: " + name);
                    }
                }

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
            this.declarations = CollectionUtils.newMap();
        }

        SortSystem build(){

            return new SortSystem(ontology, declarations);
        }

    }
}