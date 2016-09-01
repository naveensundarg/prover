package com.naveensundarg.shadow.prover.core.sortsystem;

import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;
import us.bpsm.edn.Symbol;

import java.util.Map;
import java.util.Set;

/**
 * Created by naveensundarg on 8/27/16.
 */
public final class Ontology {

    private final Map<Category, Set<Category>> parentMap;
    private final Map<Category, Set<Category>> childMap;


    protected Ontology(final Map<Category, Set<Category>> parentMap, final Map<Category, Set<Category>> childMap){

        this.parentMap = parentMap;
        this.childMap = childMap;
    }


    public static Ontology initializeOntology(Map sortDefinitions){

        Builder builder = new Builder(sortDefinitions);
        return builder.build();
    }

    protected boolean isDescendant(Category c1, Category c2){

        return parentMap.getOrDefault(c1, CollectionUtils.newEmptySet()).stream().anyMatch(c -> c.equals(c2) || isDescendant(c, c2));
    }

    protected boolean isAncestor(Category c1, Category c2){

        return isDescendant(c2,c1);
    }


    protected static final class Builder {

        private final Map<Category, Set<Category>> parentMap;
        private final Map<Category, Set<Category>> childMap;

        Builder(Map sortDefinitions){
            parentMap = CollectionUtils.newMap();
            childMap = CollectionUtils.newMap();


            sortDefinitions.keySet().forEach(key -> {

                Category category = new Category(((Symbol)key).getName());
                Set<?> parents = (Set<?>) sortDefinitions.get(key);
                Set<Category> parentCategories = CollectionUtils.newEmptySet();

                parents.forEach(parent->parentCategories.add(new Category(((Symbol)parent).getName())));

                add(category,parentCategories);
            });



        }

        public Ontology build(){
            return new Ontology(parentMap, childMap);
        }

        void add(Category category, Set<Category> parents){

            parentMap.put(category, parents);
            parents.forEach(parent -> {
                childMap.computeIfAbsent(parent, ignore -> Sets.with(category));
                childMap.computeIfPresent(parent, (ignore,existing) -> Sets.add(existing, category));
            });

        }


    }
}
