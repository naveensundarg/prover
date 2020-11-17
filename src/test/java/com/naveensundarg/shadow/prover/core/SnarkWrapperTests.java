package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.sandboxes.Sandbox;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import junit.framework.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.FileNotFoundException;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by naveensundarg on 4/9/16.
 */
public class SnarkWrapperTests {


    Prover prover;
    Map<Problem, Pair<Clause, Clause>> used;
    SnarkWrapperTests(){

        prover = SnarkWrapper.getInstance();
    }

    @DataProvider(name="completenessTestsProvider")
    public Object[][] completenessTestsProvider() throws Reader.ParsingException, FileNotFoundException {

        List<Problem> tests = ProblemReader.readFrom("problems/firstorder-completeness-tests.clj");
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

        Assert.assertTrue(prover.prove(assumptions, formula).isPresent());

    }

    @DataProvider(name="debugTestsProvider")
    public Object[][] debugTestsProvider() throws Reader.ParsingException, FileNotFoundException {

        List<Problem >tests = ProblemReader.readFrom("problems/firstorder-debug-tests.clj");
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
    public Object[][] soundnessTestsProvider() throws Reader.ParsingException, FileNotFoundException {

        List<Problem >tests = ProblemReader.readFrom("problems/firstorder-soundness-tests.clj");
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
