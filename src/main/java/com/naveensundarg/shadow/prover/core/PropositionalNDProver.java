package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.*;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.utils.Logic;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Reader.readFormula;
import static com.naveensundarg.shadow.prover.utils.Reader.read;


/**
 * Created by naveensundarg on 4/8/16.
 */
public class PropositionalNDProver implements Prover{

    private static Formula FALSE;

    public PropositionalNDProver(){
        throw new NotImplementedException();
    }
    static {
        try {
            FALSE = readFormula(read("(or A (not A))"));
        } catch (Reader.ParsingException e) {
            e.printStackTrace();
        }
    }


    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        Set<Formula> goalHistory = Sets.newSet();
        goalHistory.add(formula);
        return prove(assumptions,  formula, goalHistory);

    }

    private Optional<Justification> prove(Set<Formula> assumptions, Formula formula, Set<Formula> goalHistory) {

        if(assumptions.contains(formula)){
            return Optional.of(Justification.trivial(formula));
        }

        // And Introduction
        Optional<Justification> backwardJustifications = backward(assumptions, formula, goalHistory);
        if (backwardJustifications.isPresent()) return backwardJustifications;

        Optional<Justification> forwardJustifications = forward(assumptions, formula, goalHistory);
        if (forwardJustifications.isPresent()) return forwardJustifications;



        return Optional.empty();

    }

    private Optional<Justification> forward(Set<Formula> assumptions, Formula formula, Set<Formula> goalHistory) {
        Optional<Formula> andOpt = assumptions.stream().filter(x-> x instanceof And).findAny();

        if(andOpt.isPresent()){

            Set<Formula> newAssumptions = Sets.remove(assumptions, andOpt.get());

            And and = (And) andOpt.get();
            Arrays.stream(and.getArguments()).forEach(newAssumptions::add);

            Optional<Justification> subProof = prove(newAssumptions, formula, Sets.add(goalHistory, formula));
            if(subProof.isPresent()){
                return Optional.of(Justification.compound("AndElim "+andOpt.get(), subProof.get()));

            }



        }

        Optional<Formula> OrOpt = assumptions.stream().filter(x-> x instanceof Or).findAny();

        if(OrOpt.isPresent()){

            Set<Formula> newAssumptions = Sets.remove(assumptions, OrOpt.get());

            Or or = (Or) OrOpt.get();

            Optional<Optional<Justification>> subProofOpt = Arrays.stream(or.getArguments()).map(disjunct->
                    prove(Sets.add(newAssumptions,disjunct), formula, Sets.add(goalHistory, formula))).findAny();

            if(subProofOpt.isPresent()){
                Optional<Justification> subProof = subProofOpt.get();
                if(subProof.isPresent()){
                    return Optional.of(Justification.compound("OrElim "+ OrOpt.get(), subProof.get()));

                }
            }

        }

        Optional<Optional<Justification>> subProofOpt = assumptions.stream().
                filter(x-> (x instanceof Not) && x.subFormulae().contains(formula)).
                filter(x-> !goalHistory.contains(x)).
                map(not->{
                    Set<Formula> newAssumptions = Sets.remove(assumptions, Logic.negated(formula));

                    return prove(newAssumptions, Logic.negated(not), Sets.add(goalHistory, Logic.negated(not)));
                }).filter(Optional::isPresent).findAny();


        if(subProofOpt.isPresent()){
            return subProofOpt.get();
        }


        return Optional.empty();
    }

    private Optional<Justification> backward(Set<Formula> assumptions, Formula formula, Set<Formula> goalHistory) {
        if(formula instanceof And){

            And and = (And) formula;

            Formula[] arguments = and.getArguments();

            List<Optional<Justification>> justifications =  Arrays.stream(arguments).
                    map(f -> prove(assumptions, f, Sets.add(goalHistory, f))).collect(Collectors.toList());

            if(justifications.stream().allMatch(justification -> justification.isPresent())){

                return Optional.of(Justification.compound("AndIntroduction",
                        justifications.stream().map(Optional::get).collect(Collectors.toList())));

            }else{

                return Optional.empty();
            }

        }

        // Or Introduction
        if(formula instanceof Or){

            Or or = (Or) formula;

            Formula[] arguments = or.getArguments();

            List<Optional<Justification>> justifications =  Arrays.stream(arguments).
                    map(f -> prove(assumptions, f, Sets.add(goalHistory, f))).collect(Collectors.toList());

            if(justifications.stream().anyMatch(justification -> justification.isPresent())){

                return Optional.of(Justification.compound("OrIntroduction "+formula,
                        justifications.stream().filter(Optional::isPresent).map(Optional::get).collect(Collectors.toList())));

            }else{

                return Optional.empty();
            }
        }

        // If Introduction

        if(formula instanceof Implication){

            Implication implication = (Implication) formula;

            Formula antecent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();


            Optional<Justification> subProof = prove(Sets.add(assumptions, antecent), consequent, Sets.add(goalHistory, consequent));
            if(subProof.isPresent()){

                return Optional.of(Justification.compound("Assume " +antecent, subProof.get()));
            }
        }


        if(formula instanceof Not){

            Not not = (Not) formula;

            Formula negated = not.getArgument();

            Set<Formula> newAssumptions = Sets.add(assumptions, negated);

            Optional<Optional<Justification>> subProofOpt = assumptions.stream().
                    map(x-> Logic.negated(x)).
                    filter(x->!goalHistory.contains(x)).
                    map(notX -> prove(newAssumptions, notX, Sets.add(goalHistory, notX))).filter(Optional::isPresent).findAny();

            if(subProofOpt.isPresent()){
                Optional<Justification> subProof = subProofOpt.get();
                if(subProof.isPresent()){
                    return Optional.of(Justification.compound("Reductio " + negated, subProof.get()));

                }
            }
        }

        return Optional.empty();
    }



}
