package com.naveensundarg.shadow.prover.core.ccprovers;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.ProblemReader;
import com.naveensundarg.shadow.prover.utils.Reader;
import junit.framework.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class InductiveCalculusProverTests {


    Prover prover;
    Map<Problem, Pair<Clause, Clause>> used;
    InductiveCalculusProverTests(){

        prover = new InductiveCalculusProver();
    }

    @DataProvider(name="completenessTestsProvider")
    public Object[][] completenessTestsProvider() throws Reader.ParsingException {

       List<Problem >tests = ProblemReader.readFrom(InductiveCalculusProver.class.getResourceAsStream("inductivecalculus-completness-tests.clj"));
       Object[][] cases =  new Object[tests.size()][3];

        for(int  i = 0; i < tests.size(); i++){

            Problem test = tests.get(i);

            cases[i][0] = test.getName();
            cases[i][1] =  test.getAssumptions();
            cases[i][2] = test.getGoal();

        }


        return cases;

    }


    @Test(dataProvider = "completenessTestsProvider")
    public void testCompleteness(String name, Set<Formula> assumptions, Formula formula){


        Assert.assertTrue(prover.prove(assumptions, formula).isPresent());

    }

}
