package com.naveensundarg.shadow.prover.sandboxes;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.ccprovers.*;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.core.propositionalmodalprovers.LP;
import com.naveensundarg.shadow.prover.core.propositionalmodalprovers.LP1;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.measures.QuantifierRank;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;

import java.io.FileInputStream;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static us.bpsm.edn.Keyword.newKeyword;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sandbox {



    public static void main(String[] args) throws Exception {


       /* Prover prover =  new CognitiveCalculusProver();

        Problem problem = ProblemReader.readFrom(new FileInputStream("./example.clj")).get(0);

        System.out.println(prover.prove((problem.getAssumptions()), problem.getGoal()));*/

       Formula exemplar = Reader.readFormulaFromString("(e=> (Knows! a t1 P) P)");
        Formula f1 = Reader.readFormulaFromString("(Knows! c t3 (Knows! d t4 R))");

        InductiveCalculusProver inductiveCalculusProver = new InductiveCalculusProver();


        System.out.println(inductiveCalculusProver.prove(Sets.from(exemplar, f1),
                Reader.readFormulaFromString("R")));

    }


    public static Formula read(String f) throws Reader.ParsingException {

        return Reader.readFormulaFromString(f);
    }

}
