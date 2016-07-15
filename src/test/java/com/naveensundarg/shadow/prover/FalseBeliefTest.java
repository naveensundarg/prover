package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.representations.Formula;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Reader;
import junit.framework.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by naveensundarg on 4/9/16.
 */
public class FalseBeliefTest {


    Prover prover;
    Map<Problem, Pair<Clause, Clause>> used;
    FalseBeliefTest(){

        prover = new CognitiveCalculusProver();
    }

    @DataProvider(name="completenessTestsProvider")
    public Object[][] completenessTestsProvider() throws Reader.ParsingException {

       List<Pair<Set<Formula>, Formula>> tests = CommonUtils.readCases(Sandbox.class.getResourceAsStream("false-belief-test.clj"));
    //    List<Pair<Set<Formula>, Formula>> tests = CommonUtils.readCases(Sandbox.class.getResourceAsStream("temp.clj"));
        Object[][] cases =  new Object[tests.size()][2];

        for(int  i = 0; i < tests.size(); i++){

            Pair<Set<Formula>, Formula> test = tests.get(i);

            cases[i][0] =  test.first();
            cases[i][1] = test.second();

        }


        return cases;

    }


    @Test(dataProvider = "completenessTestsProvider")
    public void testCompleteness(Set<Formula> assumptions, Formula formula){

        Assert.assertTrue(prover.prove(assumptions, formula).isPresent());

    }


    @DataProvider(name="soundnessTestsProvider")
    public Object[][] soundnessTestsProvider() throws Reader.ParsingException {

        List<Pair<Set<Formula>, Formula>> tests = CommonUtils.readCases(Sandbox.class.getResourceAsStream("cognitivecalculus-soundness-tests.clj"));

        Object[][] cases =  new Object[tests.size()][2];

        for(int  i = 0; i < tests.size(); i++){

            Pair<Set<Formula>, Formula> test = tests.get(i);

            cases[i][0] =  test.first();
            cases[i][1] = test.second();

        }


        return cases;

    }


    //@Test(dataProvider = "soundnessTestsProvider")
    public void testSoundess(Set<Formula> assumptions, Formula formula){

        Assert.assertFalse(prover.prove(assumptions, formula).isPresent());

    }

}
