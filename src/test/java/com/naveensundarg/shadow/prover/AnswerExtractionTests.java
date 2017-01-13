package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;
import java.util.Optional;

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
            cases[i][1] = test.getAnswerVariables().isPresent();

        }


        return cases;

    }



    @Test(dataProvider = "testsProvider")
    public void testCompleteness(Problem problem, boolean answerVariableGiven){

        Map<Variable,Value> answerMap = prover.proveAndGetBindings(problem.getAssumptions(), problem.getGoal(),
                problem.getAnswerVariables().get()).get();

        List<Variable> answerVariables = problem.getAnswerVariables().get();
        List<Value> expectedAnswers = problem.getAnswersExpected().get();

        for(int i = 0; i< answerVariables.size(); i++){

            Assert.assertEquals(answerMap.get(answerVariables.get(i)), expectedAnswers.get(i));
        }

    }


}
