package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.List;

public class ResolutionJustification extends Justification {

    private final List<Formula> rows;
    public ResolutionJustification(List<Formula> rows) {

        this.rows = rows;
    }

    @Override
    public String toString() {
        return "ResolutionJustification{" +
                "rows=" + rows +
                '}';
    }
}
