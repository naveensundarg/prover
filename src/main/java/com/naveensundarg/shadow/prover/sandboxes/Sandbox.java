package com.naveensundarg.shadow.prover.sandboxes;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.ccprovers.*;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.propositionalmodalprovers.LP;
import com.naveensundarg.shadow.prover.core.propositionalmodalprovers.LP1;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Quantifier;
import com.naveensundarg.shadow.prover.representations.measures.QuantifierRank;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;

import java.io.FileInputStream;
import java.util.List;
import java.util.Optional;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {



    public static void main(String[] args) throws Exception {


        Prover prover =  new CognitiveCalculusProver();

        Problem problem = ProblemReader.readFrom(new FileInputStream("./example.clj")).get(0);

        System.out.println(prover.prove((problem.getAssumptions()), problem.getGoal()));
    }


    public static Formula read(String f) throws Reader.ParsingException {

        return Reader.readFormulaFromString(f);
    }

}
