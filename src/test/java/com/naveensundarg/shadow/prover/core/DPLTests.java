package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.dpl.Interpreter;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.sandboxes.Sandbox;
import com.naveensundarg.shadow.prover.utils.*;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import us.bpsm.edn.parser.Parseable;
import us.bpsm.edn.parser.Parser;
import us.bpsm.edn.parser.Parsers;

import java.io.InputStreamReader;
import java.util.List;

import static us.bpsm.edn.parser.Parsers.defaultConfiguration;

public class DPLTests {


    DPLTests(){

    }

    @DataProvider(name="completenessTestsProvider")
    public Object[][] completenessTestsProvider() throws Reader.ParsingException {

        List<DPLChunk>tests = ProblemReader.readDPLChunkFrom(Sandbox.class.getResourceAsStream("../dpl/test1.clj"));
        Object[][] cases =  new Object[tests.size()][2];

        for(int  i = 0; i < tests.size(); i++){

            DPLChunk test = tests.get(i);

            cases[i][0] =  test;
            cases[i][1] = test.getGoal();

        }


        return cases;

    }


    @Test(dataProvider = "completenessTestsProvider")
    public void testCompleteness(DPLChunk dplChunk, Phrase goal){

            Assert.assertEquals(Interpreter.interpret(dplChunk.getAssumptions(), dplChunk.getInput()), dplChunk.getGoal());

    }




}
