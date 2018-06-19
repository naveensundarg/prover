package com.naveensundarg.shadow.prover.core.propositionalmodalprovers;

import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.SymbolGenerator;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import java.util.Map;

/**
 * Created by naveensundarg on 1/1/17.
 */
public class PropositionalModalConverter {


    public static Variable WORLD;
    public static Constant W;

    static {

        Parseable parseable = Parsers.newParseable("?WORLD");
        Parser p = Parsers.newParser(Parsers.defaultConfiguration());




        try {
            WORLD = (Variable) Reader.readLogicValue(p.nextValue(parseable));
            parseable = Parsers.newParseable("W");

            W =  (Constant) Reader.readLogicValue(p.nextValue(parseable));

        } catch (Reader.ParsingException e) {
            e.printStackTrace();
        }

    }


    private static Predicate makeAtomInWorldPredicate(Atom atom, Value world){

        Value[] args = new Value[1];
        args[0] = world;

        return new Predicate(atom.getName(), args);

    }

    private static Formula getAccessibilityPredicate(Variable variable){

        Value[] args = new Value[2];

        args[0] = WORLD;
        args[1] = variable;

        return new Predicate("!R!", args);
    }

    public static Formula convert(Formula formula, Problem problem) {


        Map<Variable, Value> map = CollectionUtils.newMap();

        map.put(WORLD, W);


        return convertInternal(formula, problem).apply(map);

    }
    private static Formula convertInternal(Formula formula, Problem problem){


        if(formula instanceof Atom){

            return makeAtomInWorldPredicate((Atom) formula, WORLD);

        }

        if(formula instanceof Not){

            return new Not(convertInternal(((Not) formula).getArgument(), problem));
        }


        if(formula instanceof Implication){

            Implication implication = (Implication) formula;

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            return new Implication(convertInternal(antecedent, problem), convertInternal(consequent, problem));

        }

        if(formula instanceof BiConditional){

            BiConditional biConditional = (BiConditional) formula;

            Formula left = biConditional.getLeft();
            Formula right = biConditional.getRight();

            return new BiConditional(convertInternal(left, problem), convertInternal(right, problem));

        }

        if(formula instanceof And){

            And and = (And) formula;

            Formula[] arguments = ((And) formula).getArguments();
            Formula[] converted = new Formula[arguments.length];

            for(int i = 0; i<arguments.length; i++){

                converted[i] = convertInternal(arguments[i], problem);

            }

            return new And(converted);

        }

        if(formula instanceof Or){

            Or or = (Or) formula;

            Formula[] arguments = ((Or) formula).getArguments();
            Formula[] converted = new Formula[arguments.length];

            for(int i = 0; i<arguments.length; i++){

                converted[i] = convertInternal(arguments[i], problem);

            }

            return new Or(converted);

        }

        if(formula instanceof Necessity){

            Variable newVariable = SymbolGenerator.newVariable(problem);

            Formula inner = convertInternal(((Necessity) formula).getFormula(), problem);

            Map<Variable, Value> map = CollectionUtils.newMap();

            map.put(WORLD, newVariable);

            Formula replaced = inner.apply(map);

            Variable[] vars = new Variable[1];
            vars[0] = newVariable;

            return new Universal(vars, new Implication(getAccessibilityPredicate(newVariable) , replaced));
        }


        if(formula instanceof Possibility){

            return convertInternal(new Not(new Necessity(new Not(((Possibility) formula).getFormula()))), problem);
/*
            Variable newVariable = SymbolGenerator.newVariable(problem);

            Formula inner = convertInternal(((Possibility) formula).getTriggeringCondition(), problem);

            Map<Variable, Value> map = CollectionUtils.newMap();

            map.put(WORLD, newVariable);

            Formula replaced = inner.apply(map);

            Variable[] vars = new Variable[1];
            vars[0] = newVariable;

            return new Existential(vars, new Implication(getAccessibilityPredicate(newVariable) , replaced));
*/
        }
        return formula;

    }

    public static void main(String[] args) throws Reader.ParsingException {

        System.out.println(convertInternal(Reader.readFormulaFromString("(nec P)"), new Problem("A", "B", Sets.newSet(), new Atom("A"))));

    }
}
