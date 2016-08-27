package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.PropositionalResolutionProver;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import junit.framework.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 4/9/16.
 */
public class PropositionalProverTests {


    Prover prover;

    PropositionalProverTests(){

        prover = new PropositionalResolutionProver();
    }

    @DataProvider(name="completenessTestsProvider")
    public Object[][] completenessTestsProvider() throws Reader.ParsingException {

        List<Problem>tests = ProblemReader.readFrom(Sandbox.class.getResourceAsStream("propositional-completness-tests.clj"));
        Object[][] cases =  new Object[tests.size()][2];

        for(int  i = 0; i < tests.size(); i++){

            Problem test = tests.get(i);

            cases[i][0] =  test.getAssumptions();
            cases[i][1] = test.getGoal();

        }


        return cases;

    }


    @Test(dataProvider = "completenessTestsProvider")
    public void testCompleteness(Set<Formula> assumptions, Formula formula){

        Assert.assertTrue(prover.prove(assumptions, formula).isPresent());

    }

    @DataProvider(name="soundnessTestsProvider")
    public Object[][] soundnessTestsProvider() throws Reader.ParsingException {

        List<Problem >tests = ProblemReader.readFrom(Sandbox.class.getResourceAsStream("propositional-soundness-tests.clj"));
        Object[][] cases =  new Object[tests.size()][2];

        for(int  i = 0; i < tests.size(); i++){

            Problem test = tests.get(i);

            cases[i][0] =  test.getAssumptions();
            cases[i][1] = test.getGoal();

        }

        return cases;

    }


    @Test(dataProvider = "soundnessTestsProvider")
    public void testSoundess(Set<Formula> assumptions, Formula formula){

        Assert.assertFalse(prover.prove(assumptions, formula).isPresent());

    }
}
