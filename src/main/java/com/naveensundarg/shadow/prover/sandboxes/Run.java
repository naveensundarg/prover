package com.naveensundarg.shadow.prover.sandboxes;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.ProblemReader;

import java.io.FileInputStream;
import java.util.List;
import java.util.Optional;

public class Run {

    public static void main(String[] args) throws Exception {

        String fileName = "./example.clj";

        List<Problem> problems = ProblemReader.readFrom(new FileInputStream(fileName));
        Problem firstProblem = problems.get(0);

        Prover prover = SnarkWrapper.getInstance();


        Optional<Justification> justificationOptional = prover.prove(firstProblem.getAssumptions(), firstProblem.getGoal());

        if(justificationOptional.isPresent()) {
            System.out.println("Success");
        } else {
            System.out.println("Failure");

        }

    }
}