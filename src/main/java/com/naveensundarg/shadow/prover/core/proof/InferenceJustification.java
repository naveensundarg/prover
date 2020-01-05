package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class InferenceJustification extends Justification {

    private List<Formula> base;
    private String        message;

    public InferenceJustification(String message, List<Formula> base) {
        this.base = base;
        this.message = message;
    }

    public List<Formula> getBase() {
        return base;
    }

    public void setBase(List<Formula> base) {
        this.base = base;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return "InferenceJustification{" +
                "base=" + base +
                ", message='" + message + '\'' +
                '}';
    }

    public static InferenceJustification from(String message, Formula ... fs){

        return new InferenceJustification(message, Arrays.stream(fs).collect(Collectors.toList()));
    }
}
