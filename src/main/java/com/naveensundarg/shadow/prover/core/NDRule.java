package com.naveensundarg.shadow.prover.core;

/**
 * Created by naveensundarg on 12/13/16.
 */
public enum NDRule {

        IF_INTRO ("\u21D2 intro"), IFF_INTRO("\u21D4 intro"), AND_INTRO("\u2227 intro"), OR_INTRO("\u2228 intro"), NOT_INTRO("\u00AC intro"),
        IF_ELIM ("\u21D2 elim"), IFF_ELIM("\u21D4 elim"), AND_ELIM("\u2227 elim"), OR_ELIM("\u2228 elim"), NOT_ELIM("\u00AC elim"),
        ASSUMPTION("assume"),
    GIVEN("given");


        private final String name;
    NDRule(String name){
        this.name = name;
    }

    @Override
    public String toString(){
        return name;
    }

    }
