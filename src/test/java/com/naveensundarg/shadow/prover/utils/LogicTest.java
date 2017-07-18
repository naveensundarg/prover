package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.representations.formula.Quantifier;
import com.naveensundarg.shadow.prover.representations.formula.Universal;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Created by naveensundarg on 1/3/17.
 */
public class LogicTest {
    @Test
    public void testIsInstantationOfUniversal() throws Exception {


        Assert.assertTrue(Logic.isInstantationOfQuantifier(
                (Universal) Reader.readFormulaFromString("(forall (?x) (P ?x))"),
                 Reader.readFormulaFromString("(P a)")).isPresent());

        Assert.assertTrue(Logic.isInstantationOfQuantifier(
                (Universal) Reader.readFormulaFromString("(forall (?x) (P ?x))"),
                Reader.readFormulaFromString("(P ?x)")).isPresent());


        Assert.assertTrue(Logic.isInstantationOfQuantifier(
                (Universal) Reader.readFormulaFromString("(forall (?x) (P ?x))"),
                Reader.readFormulaFromString("(P (f a))")).isPresent());


        Assert.assertFalse(Logic.isInstantationOfQuantifier(
                (Universal) Reader.readFormulaFromString("(forall (?x) (P ?x))"),
                Reader.readFormulaFromString("(forall (?x) (P ?x))")).isPresent());


        Assert.assertFalse(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(forall (?x) (P ?x))"),
                Reader.readFormulaFromString(" (Q a))")).isPresent());


        Assert.assertTrue(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(forall (?x) (forall (?y) (P ?x ?y)))"),
                Reader.readFormulaFromString("(forall (?y) (P ?x ?y))")).isPresent());

        Assert.assertTrue(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(forall (?x) (forall (?y) (P ?x ?y)))"),
                Reader.readFormulaFromString("(forall (?y) (P a ?y))")).isPresent());

        Assert.assertFalse(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(forall (?x) (forall (?y) (P ?x ?y)))"),
                Reader.readFormulaFromString("(forall (?y) (P ?y ?y))")).isPresent());


        Assert.assertTrue(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(exists (?x) (P ?x))"),
                Reader.readFormulaFromString("(P ?x)")).isPresent());


        Assert.assertTrue(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(exists (?x) (P ?x))"),
                Reader.readFormulaFromString("(P (f a))")).isPresent());


        Assert.assertFalse(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(exists (?x) (P ?x))"),
                Reader.readFormulaFromString("(exists (?x) (P ?x))")).isPresent());


        Assert.assertFalse(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(exists (?x) (P ?x))"),
                Reader.readFormulaFromString(" (Q a))")).isPresent());


        Assert.assertTrue(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(exists (?x) (forall (?y) (P ?x ?y)))"),
                Reader.readFormulaFromString("(forall (?y) (P ?x ?y))")).isPresent());

        Assert.assertTrue(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(exists (?x) (forall (?y) (P ?x ?y)))"),
                Reader.readFormulaFromString("(forall (?y) (P a ?y))")).isPresent());

        Assert.assertFalse(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(exists (?x) (forall (?y) (P ?x ?y)))"),
                Reader.readFormulaFromString("(forall (?y) (P ?y ?y))")).isPresent());


        Assert.assertFalse(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(exists x (exists y (Likes (f y) x)))" ),
                Reader.readFormulaFromString("(exists y (Likes (f y) y))")).isPresent());


        Assert.assertFalse(Logic.isInstantationOfQuantifier(
                (Quantifier) Reader.readFormulaFromString("(exists x (exists y (Likes x y)))" ),
                Reader.readFormulaFromString("(exists y (Likes (f y) y))")).isPresent());


    }

}