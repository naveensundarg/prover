package com.naveensundarg.shadow.prover.core.ccprovers;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.And;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.GeneralModal;
import com.naveensundarg.shadow.prover.representations.formula.Not;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
 import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class AxiologyProver implements Prover {


    private List<String> operations;

    private static boolean VERBOSE = false;
    private static String indent = "";

    private static ColoredPrinter coloredPrinter = new ColoredPrinter.Builder(1, false)
            .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.BLACK)   //setting format
            .build();

    public AxiologyProver(){

        operations = CollectionUtils.newEmptyList();
    }

    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        Set<Formula> base = Sets.copy(assumptions);

        assumptions.forEach(x->x.setJustification(Justification.atomic("GIVEN", x)));

        while (true) {

            int sizeBeforeExpansion = base.size();
            base = expand(base, formula);
            int sizeAfterExpansion = base.size();


            if(sizeAfterExpansion == sizeBeforeExpansion){

                return Optional.empty();

            }

            Optional<Formula> result = base.stream().filter(x-> x.equals(formula)).findFirst();
            if(result.isPresent()){

                Justification justification =  result.get().getJustification()  ;

                return Optional.of(justification);

            }
        }


    }


    private Set<Formula> expand(Set<Formula> base, Formula goal){

        Set<Formula> expanded  = Sets.copy(base);


        Set<GeneralModal> generalModals = base.stream().filter(x->x instanceof GeneralModal).map(x->(GeneralModal) x).collect(Collectors.toSet());

        generalModals.forEach(generalModal -> A1Forward(generalModal).ifPresent(expanded::add));
        generalModals.forEach(generalModal -> OughtFromGood(generalModal).ifPresent(expanded::add));


        Sets.cartesianProduct(CollectionUtils.listOf(generalModals, generalModals)).forEach(y->{


            A4Reverse(y.get(0), y.get(1)).ifPresent(expanded::add);
            A5Reverse(y.get(0), y.get(1)).ifPresent(expanded::add);

        });


        return expanded;




    }

    private static String SAME = "Same!";
    private static String PREF = "Pref!";
    private static String INDIFF = "Indiff!";
    private static String GOOD = "Good!";
    private static String BAD = "Bad!";
    private static String ALAG = "ALAG!";

    private static String OUGHT = "O!";

    private Optional<Formula> A1Forward(GeneralModal same) {


        if (same.getName().equals("Same!")) {


            Formula phi = same.getFormulae().get(0);
            Formula psi = same.getFormulae().get(1);

            return Optional.of(new And(new Not(GeneralModal.build("Pref!", phi, psi)), new Not(GeneralModal.build("Pref!", psi, phi))));

        } else {

            return Optional.empty();
        }

    }


    private Optional<Formula> OughtFromGood(GeneralModal good) {


        if (is(good, GOOD)) {

            tryMessage("OughtFromGood");

            Formula phi = good.getFormulae().get(0);
            successMessage(" Applied OughtFromGood " + good);

            return Optional.of(GeneralModal.build(OUGHT, phi).setJustification(Justification.compound("OughtFromGood", CollectionUtils.listOf(
                    good.getJustification()
            ))));

        } else {

            return Optional.empty();
        }

    }




    private Optional<Formula> A4Reverse(GeneralModal indiff, GeneralModal pref) {



        if (is(indiff, INDIFF) && is(pref, PREF)) {

            tryMessage("A4Reverse");
            indent = indent + "\t";

            Formula psi_1 = indiff.getFormulae().get(0);
            Formula phi = pref.getFormulae().get(0);
            Formula psi_2 = pref.getFormulae().get(1);

            if (psi_1.equals(psi_2)) {

                operations.add("A4Reverse(" + indiff + "," + pref+")");

                successMessage(" Applied A4Reverse " + indiff + ", " + pref);
                indent = indent.substring(0, indent.length()-1);

                return Optional.of(GeneralModal.build(GOOD, phi).setJustification(Justification.compound("A4 & BiCondElim",
                        CollectionUtils.listOf(
                                indiff.getJustification(), pref.getJustification()))));
            } else {


                failureMessage(" applying A4Reverse " + indiff + ", " + pref);
                indent = indent.substring(0, indent.length()-1);

                return Optional.empty();

            }


        } else {

            return Optional.empty();
        }

    }

    private Optional<Formula> A5Reverse(GeneralModal indiff, GeneralModal pref) {


        if (is(indiff, INDIFF) && is(pref, PREF)) {

            tryMessage("A5Reverse");
            indent = indent + "\t";


            Formula psi_1 = indiff.getFormulae().get(0);
            Formula phi = pref.getFormulae().get(1);
            Formula psi_2 = pref.getFormulae().get(0);

            if (psi_1.equals(psi_2)) {

                operations.add("A5Reverse(" + indiff + "," + pref+")");
                successMessage(" Applied A5Reverse " + indiff + ", " + pref);
                indent = indent.substring(0, indent.length()-1);

                return Optional.of(GeneralModal.build(BAD, phi).setJustification(Justification.compound("A5 & BiCondElim", CollectionUtils.listOf(
                        indiff.getJustification(),
                        pref.getJustification()))));


            } else {

                failureMessage(" applying A5Reverse " + indiff + ", " + pref);
                indent = indent.substring(0, indent.length()-1);

                return Optional.empty();

            }


        } else {

            return Optional.empty();
        }

    }


    private boolean is(Formula f, String type) {

        return f instanceof GeneralModal && ((GeneralModal) f).getName().equals(type);
    }


    private static void tryMessage(String s){

        if(!VERBOSE) return;

        coloredPrinter.clear();
        coloredPrinter.print(indent+ "Trying " + s , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.YELLOW);

        coloredPrinter.clear();
        coloredPrinter.println("");

        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private static void successMessage(String s){

        if(!VERBOSE) return;

        coloredPrinter.print(indent+ "", Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
        coloredPrinter.print("success" , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.GREEN);
        coloredPrinter.print(s , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);

        coloredPrinter.clear();
        coloredPrinter.println("");

        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

    }

    private static void failureMessage(String s){

        if(!VERBOSE) return;
        coloredPrinter.print(indent+ ""  , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
        coloredPrinter.print("failure" , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.RED);
        coloredPrinter.print( s , Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
        coloredPrinter.clear();
        coloredPrinter.println("");

        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}
