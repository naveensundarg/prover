package com.naveensundarg.shadow.prover.core.ccprovers;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 4/12/16.
 */
public class FolConverter {

    public static Formula Step1_EliminateImplications(Formula formula, Problem problem) {
        return Step1_EliminateImplicationsInt(formula);
    }

    private static Formula Step1_EliminateImplicationsInt(Formula formula) {

        if (formula instanceof Predicate) {
            return formula;
        }

        if (formula instanceof Not) {
            return new Not(Step1_EliminateImplicationsInt(((Not) formula).getArgument()));
        }

        if (formula instanceof Or) {
            Or or = (Or) formula;
            return new Or(Arrays.stream(or.getArguments()).
                    map(FolConverter::Step1_EliminateImplicationsInt).collect(Collectors.toList()));
        }

        if (formula instanceof And) {
            And and = (And) formula;
            return new And(Arrays.stream(and.getArguments()).
                    map(FolConverter::Step1_EliminateImplicationsInt).collect(Collectors.toList()));
        }

        if (formula instanceof Universal) {
            Universal universal = (Universal) formula;

            return new Universal(universal.vars(), Step1_EliminateImplicationsInt(universal.getArgument()));
        }

        if (formula instanceof Existential) {
            Existential existential = (Existential) formula;

            return new Existential(existential.vars(), Step1_EliminateImplicationsInt(existential.getArgument()));
        }

        if (formula instanceof Implication) {
            Implication implication = (Implication) formula;

            return new Or(Logic.negated(Step1_EliminateImplicationsInt(implication.getAntecedent())), Step1_EliminateImplicationsInt(implication.getConsequent()));
        }

        if (formula instanceof BiConditional) {
            BiConditional biConditional = (BiConditional) formula;

            Formula left = biConditional.getLeft();
            Formula right = biConditional.getRight();

            return Step1_EliminateImplicationsInt(new And(Step1_EliminateImplicationsInt(new Implication(left, right)),
                    Step1_EliminateImplicationsInt(new Implication(right, left))));
        }

        if (formula instanceof Common) {

            Common belief = (Common) formula;

            Formula arg = belief.getFormula();
            Value time = belief.getTime();

            return new Common(time, Step1_EliminateImplicationsInt(arg));
        }
        if (formula instanceof Belief) {

            Belief belief = (Belief) formula;

            Formula arg = belief.getFormula();
            Value agent = belief.getAgent();
            Value time = belief.getTime();

            return new Belief(agent, time, Step1_EliminateImplicationsInt(arg));
        }

        if (formula instanceof Knowledge) {

            Knowledge knowledge = (Knowledge) formula;

            Formula arg = knowledge.getFormula();
            Value agent = knowledge.getAgent();
            Value time = knowledge.getTime();

            return new Knowledge(agent, time, Step1_EliminateImplicationsInt(arg));
        }
        if (formula instanceof Perception) {

            Perception perception = (Perception) formula;

            Formula arg = perception.getFormula();
            Value agent = perception.getAgent();
            Value time = perception.getTime();

            return new Perception(agent, time, Step1_EliminateImplicationsInt(arg));
        } else {
            throw new AssertionError("Unknown formula type");
        }

    }

    public static Formula Step2_MoveNegationsInWard(Formula formula, Problem problem) {
        return Step2_MoveNegationsInWardInt(formula, ColoredConverter.NONE);
    }


    private static Formula Step2_MoveNegationsInWardInt(Formula formula, Value color) {


        if (formula instanceof Predicate) {
            return ColoredConverter.augmentWithColor((Predicate) formula, color);
        }

        if (formula instanceof Not) {

            Not not = (Not) formula;
            Formula notArg = not.getArgument();

            if (notArg instanceof Predicate) {
                return new Not(ColoredConverter.augmentWithColor((Predicate) notArg, color));
            }
            if (notArg instanceof Not) {

                return Step2_MoveNegationsInWardInt(((Not) notArg).getArgument(), ColoredConverter.addToColor(ColoredConverter.NOT, color));
            }

            if (notArg instanceof Or) {

                Or or = (Or) notArg;

                And and = new And(Arrays.stream(or.getArguments()).map(Logic::negated).
                        map(x -> FolConverter.Step2_MoveNegationsInWardInt(x, color)).collect(Collectors.toList()));
                return and;
            }

            if (notArg instanceof And) {

                And and = (And) notArg;

                Or or = new Or(Arrays.stream(and.getArguments()).
                        map(Logic::negated).map(x -> FolConverter.Step2_MoveNegationsInWardInt(x, color)).collect(Collectors.toList()));
                return or;
            }

            if (notArg instanceof Existential) {

                Existential existential = (Existential) notArg;


                Universal universal = new Universal(existential.vars(), Step2_MoveNegationsInWardInt(Logic.negated(existential.getArgument()), color));

                return universal;

            }

            if (notArg instanceof Universal) {

                Universal universal = (Universal) notArg;

                Existential existential = new Existential(universal.vars(), Step2_MoveNegationsInWardInt(Logic.negated(universal.getArgument()), color));

                return existential;

            }

            if (notArg instanceof Common) {

                Common common = (Common) notArg;

                Formula arg = common.getFormula();
                Value time = common.getTime();

                return Step2_MoveNegationsInWardInt(new Not(arg), ColoredConverter.addToColor(ColoredConverter.NOT, ColoredConverter.addToColor(new Constant("B"), ColoredConverter.addToColor(time, color))));
            }
            if (notArg instanceof Belief) {

                Belief belief = (Belief) notArg;

                Formula arg = belief.getFormula();
                Value agent = belief.getAgent();
                Value time = belief.getTime();

                List<Value> givenColor = ColoredConverter.getValues(color);
                List<Value> newColor = CollectionUtils.newEmptyList();
                newColor.add(ColoredConverter.NONE);

                newColor.add(0, time);
                newColor.add(0, agent);
                newColor.add(0, new Constant("B"));
                givenColor.remove(0);
                givenColor.forEach(x -> {
                    newColor.add(0, x);

                });

                return Step2_MoveNegationsInWardInt(new Not(arg), ColoredConverter.makeColor(newColor));
            }

            if (notArg instanceof Knowledge) {

                Knowledge knowledge = (Knowledge) notArg;

                Formula arg = knowledge.getFormula();
                Value agent = knowledge.getAgent();
                Value time = knowledge.getTime();

                List<Value> givenColor = ColoredConverter.getValues(color);
                List<Value> newColor = CollectionUtils.newEmptyList();
                newColor.add(ColoredConverter.NONE);

                newColor.add(0, time);
                newColor.add(0, agent);
                newColor.add(0, new Constant("K"));
                givenColor.remove(0);
                givenColor.forEach(x -> {
                    newColor.add(0, x);

                });

                return Step2_MoveNegationsInWardInt(new Not(arg), ColoredConverter.makeColor(newColor));
            }

            if (notArg instanceof Perception) {

                Perception perception = (Perception) notArg;

                Formula arg = perception.getFormula();
                Value agent = perception.getAgent();
                Value time = perception.getTime();

                return Step2_MoveNegationsInWardInt(new Not(arg), ColoredConverter.addToColor(ColoredConverter.NOT, ColoredConverter.addToColor(new Constant("P"), ColoredConverter.addToColor(agent, ColoredConverter.addToColor(time, color)))));
            }

        }

        if (formula instanceof Or) {
            Or or = (Or) formula;
            return new Or(Arrays.stream(or.getArguments()).
                    map(x -> FolConverter.Step2_MoveNegationsInWardInt(x, color)).collect(Collectors.toList()));
        }

        if (formula instanceof And) {
            And and = (And) formula;
            return new And(Arrays.stream(and.getArguments()).
                    map(x -> FolConverter.Step2_MoveNegationsInWardInt(x, color)).collect(Collectors.toList()));
        }

        if (formula instanceof Universal) {
            Universal universal = (Universal) formula;

            return new Universal(universal.vars(), Step2_MoveNegationsInWardInt(universal.getArgument(), color));
        }

        if (formula instanceof Existential) {
            Existential existential = (Existential) formula;

            return new Existential(existential.vars(), Step2_MoveNegationsInWardInt(existential.getArgument(), color));
        }


        if (formula instanceof Common) {

            Common common = (Common) formula;

            Formula arg = common.getFormula();
            Value time = common.getTime();

            return new Common(time, Step2_MoveNegationsInWardInt(arg, color));
        }
        if (formula instanceof Belief) {

            Belief belief = (Belief) formula;

            Formula arg = belief.getFormula();
            Value agent = belief.getAgent();
            Value time = belief.getTime();

            List<Value> givenColor = ColoredConverter.getValues(color);

            List<Value> newColor = CollectionUtils.newEmptyList();
            newColor.add(ColoredConverter.NONE);

            newColor.add(0, time);
            newColor.add(0, agent);
            newColor.add(0, new Constant("B"));
            givenColor.remove(0);
            givenColor.forEach(x -> {
                    newColor.add(0, x);

            });

            return Step2_MoveNegationsInWardInt(arg, ColoredConverter.makeColor(newColor));
        }

        if (formula instanceof Knowledge) {

            Knowledge knowledge = (Knowledge) formula;

            Formula arg = knowledge.getFormula();
            Value agent = knowledge.getAgent();
            Value time = knowledge.getTime();

            List<Value> givenColor = ColoredConverter.getValues(color);

            List<Value> newColor = CollectionUtils.newEmptyList();
            newColor.add(ColoredConverter.NONE);

            newColor.add(0, time);
            newColor.add(0, agent);
            newColor.add(0, new Constant("K"));
            givenColor.remove(0);
            givenColor.forEach(x -> {
                    newColor.add(0, x);

            });

            return Step2_MoveNegationsInWardInt(arg, ColoredConverter.makeColor(newColor));
        }

        if (formula instanceof Perception) {

            Perception perception = (Perception) formula;

            Formula arg = perception.getFormula();
            Value agent = perception.getAgent();
            Value time = perception.getTime();

            return new Perception(agent, time, Step2_MoveNegationsInWardInt(arg, color));
        } else {
            throw new AssertionError("Unknown formula type: " + formula);
        }

    }

    public static Formula Step3_StandardizeApart(Formula formula, Problem problem) {
        return Converter.standardizeApart(formula, problem);
    }


    public static Formula preProcess(Formula formula, Problem problem) {


        Formula f;
        f = Step1_EliminateImplications(formula, problem);
        f = Step2_MoveNegationsInWard(f, problem);
        f = Step3_StandardizeApart(f, problem);


        return f;
    }

    public static Formula Step3_Skolemize(Formula formula, Problem problem) {

        return Converter.skolemize(formula, problem);
    }

}
