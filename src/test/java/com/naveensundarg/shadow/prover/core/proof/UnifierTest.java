package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Reader;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import static com.naveensundarg.shadow.prover.utils.Reader.readFormulaFromString;
import static org.testng.Assert.*;

/**
 * Created by naveensundarg on 1/2/17.
 */
public class UnifierTest {
    @Test
    public void testUnifyFormula() throws Exception {


        Variable x = (Variable) Reader.readLogicValueFromString("?x");
        Variable y = (Variable) Reader.readLogicValueFromString("?y");
        Variable z = (Variable) Reader.readLogicValueFromString("?z");

        Value a =  Reader.readLogicValueFromString("a");
        Value b =  Reader.readLogicValueFromString("b");
        Value c =  Reader.readLogicValueFromString("c");


        Assert.assertFalse(Unifier.unifyFormula(readFormulaFromString("P"), readFormulaFromString("Q")).isPresent());

        Assert.assertTrue(Unifier.unifyFormula(readFormulaFromString("P"), readFormulaFromString("P")).isPresent());

        Assert.assertTrue(Unifier.unifyFormula(readFormulaFromString("(P ?x)"), readFormulaFromString("(P a)")).isPresent());

        {
            Optional<Map<Variable, Value>> valueOpt = Unifier.unifyFormula(
                    readFormulaFromString("(and (P ?x) (Q a))"),
                    readFormulaFromString("(and (P a) (Q ?x))"));
            Assert.assertTrue(valueOpt.isPresent());
            Assert.assertEquals(valueOpt.get().get(x), a);

        }

        {
            Optional<Map<Variable, Value>> valueOpt = Unifier.unifyFormula(
                    readFormulaFromString("(if (and (P ?x) (Q a)) (R ?y))"),
                    readFormulaFromString("(if (and (P a) (Q ?x)) (R c))"));

            Assert.assertTrue(valueOpt.isPresent());
            Assert.assertEquals(valueOpt.get().get(x), a);
            Assert.assertEquals(valueOpt.get().get(y), c);

        }

        {
            Optional<Map<Variable, Value>> valueOpt = Unifier.unifyFormula(
                    readFormulaFromString("(forall (?x) (if (and (P ?x) (Q ?x)) (R ?y)))"),
                    readFormulaFromString("(forall (?x) (if (and (P ?x) (Q ?x)) (R c)))"));

            Assert.assertTrue(valueOpt.isPresent());
            Assert.assertEquals(valueOpt.get().get(y), c);

        }

    }

}