package com.naveensundarg.shadow.prover.representations;

/**
 * Created by naveensundarg on 8/28/17.
 */
public class ErrorPhrase extends Phrase {

    private final String message;


    public ErrorPhrase(String message) {
        this.message = message;

    }

    @Override
    public String toString() {
        return "(error \"" + message  + "\")";
    }
}
