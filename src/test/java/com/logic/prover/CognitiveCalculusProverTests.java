package com.logic.prover;

import com.naveensundarg.shadow.prover.core.ccprovers.ColorShadowProverTests;
import com.naveensundarg.shadow.prover.sandboxes.Sandbox;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import junit.framework.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class CognitiveCalculusProverTests {


    Prover prover;
    Map<Problem, Pair<Clause, Clause>> used;
    CognitiveCalculusProverTests(){

        prover = new CognitiveCalculusProver();
    }


    public static void main (String[] args) {
		new TestNGEntryPoint(CognitiveCalculusProverTests.class.getName(), "testCompleteness");
	}


    @DataProvider(name="completenessTestsProvider")
    public Object[][] completenessTestsProvider() throws Reader.ParsingException {

       List<Problem >tests = ProblemReader.readFrom(CognitiveCalculusProver.class.getResourceAsStream("cognitivecalculus-completness-tests.clj"));
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

    @DataProvider(name="debugTestsProvider")
    public Object[][] debugTestsProvider() throws Reader.ParsingException {

        List<Problem >tests = ProblemReader.readFrom(Sandbox.class.getResourceAsStream("../debug.clj"));
        Object[][] cases =  new Object[tests.size()][2];

        for(int  i = 0; i < tests.size(); i++){

            Problem test = tests.get(i);

            cases[i][0] =  test.getAssumptions();
            cases[i][1] = test.getGoal();

        }


        return cases;

    }


   @Test(dataProvider = "debugTestsProvider")
    public void debugTests(Set<Formula> assumptions, Formula formula){

        prover = new CognitiveCalculusProver();

        Assert.assertTrue(prover.prove(assumptions, formula).isPresent());

    }

    @DataProvider(name="soundnessTestsProvider")
    public Object[][] soundnessTestsProvider() throws Reader.ParsingException {

        List<Problem >tests = ProblemReader.readFrom(Sandbox.class.getResourceAsStream("cognitivecalculus-soundness-tests.clj"));
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
