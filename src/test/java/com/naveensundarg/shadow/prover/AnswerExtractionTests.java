package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import junit.framework.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 4/9/16.
 */
public class AnswerExtractionTests {


    Prover prover;
    Map<Problem, Pair<Clause, Clause>> used;
    AnswerExtractionTests(){

        prover = new SnarkWrapper();
    }

    @DataProvider(name="testsProvider")
    public Object[][] completenessTestsProvider() throws Reader.ParsingException {

        List<Problem>tests = ProblemReader.readFrom(Sandbox.class.getResourceAsStream("firstorder-answer-extraction-tests.clj"));
        Object[][] cases =  new Object[tests.size()][2];

        for(int  i = 0; i < tests.size(); i++){

            Problem test = tests.get(i);

            cases[i][0] =  test;
            cases[i][1] = test.getAnswerVariable().isPresent();

        }


        return cases;

    }



    @Test(dataProvider = "testsProvider")
    public void testCompleteness(Problem problem, boolean answerVariableGiven){

        Optional<Value> valueOptional = prover.proveAndGetBinding(problem.getAssumptions(), problem.getGoal(),
                problem.getAnswerVariable().get());

        Assert.assertEquals(valueOptional.get(), problem.getAnswerExpected().get());


    }


}
