package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.core.ccprovers.ColoredConverter;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Value;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * Created by naveensundarg on 5/16/17.
 */
public class Color {
    public static Value colorOFLiteral(Literal literal){

        Value[] args = literal.getPredicate().getArguments();

        if(args.length==0){
            return ColoredConverter.NONE;
        }
        else {
            return args[args.length-1];
        }
    }

    public static Value firstColor(Value color){

        return color.getArguments().length>0?color.getArguments()[0]: ColoredConverter.NONE;
    }

    public static Value restColor(Value color){

        return color.getArguments().length>0?color.getArguments()[1]:ColoredConverter.NONE;
    }

    public static Literal changeColor(Literal literal, Value color){

        Predicate predicate = literal.getPredicate();
        Value[] args = Arrays.copyOf(predicate.getArguments(), predicate.getArguments().length);
        args[args.length-1]  = color;

        return new Literal(new Predicate(predicate.getName(), args), literal.isNegated());
    }

    public static Optional<Pair<Literal, Literal>> areColorSwitchedLiterals(Literal literal1, Literal literal2){

        Value color1 = colorOFLiteral(literal1);
        Value color2 = colorOFLiteral(literal2);

        if(firstColor(color1).equals(ColoredConverter.NOT)){

            if(restColor(color1).equals(color2)){

                return Optional.of(ImmutablePair.from(changeColor(literal1, restColor(color1)), literal2));

            }
        }
        if(firstColor(color2).equals(ColoredConverter.NOT)){
            if(restColor(color2).equals(color1)){

                return Optional.of(ImmutablePair.from(literal1, changeColor(literal2, restColor(color2))));

            }
        }

        return Optional.empty();

    }

    public static List<Value> getValuesFromColor(Value color){

        List<Value> answer = CollectionUtils.newEmptyList();

        if(color.equals(ColoredConverter.NONE)){
            return answer;
        }

        Value[] args = color.getArguments();

        answer.add(args[0]);
        if(args[1].equals(ColoredConverter.NONE)){
            return answer;
        }

        answer.addAll(getValuesFromColor(args[1]));

        return answer;
    }
    public static Value getBaseColorTerm(Value ... values) {

        Value answer = ColoredConverter.NONE;
        for(int i = values.length-1; i>=0; i--){

            answer = new Compound("color", new Value[]{values[i], answer});
        }

        return answer;
    }

    public static Value getBaseColorTerm(List<Value> values) {


        Value answer = ColoredConverter.NONE;
        for(int i = values.size()-1; i>=0; i--){

            answer = new Compound("color", new Value[]{values.get(i), answer});
        }

        return answer;
    }

    public static Value joinColor(Value color1, Value color2){

        List<Value> colors = getValuesFromColor(color1);
        colors.addAll(getValuesFromColor(color2));
        return getBaseColorTerm(colors);
    }
}
