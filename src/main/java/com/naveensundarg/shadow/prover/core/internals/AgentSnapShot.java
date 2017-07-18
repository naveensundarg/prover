package com.naveensundarg.shadow.prover.core.internals;

import com.naveensundarg.shadow.prover.representations.formula.Belief;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Intends;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.core.Logic;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 11/25/16.
 */
public final class AgentSnapShot {

    private final Map<Value, Map<Value, Set<Formula>>> knowledgeSnapshots;
    private final Map<Value, Map<Value, Set<Formula>>> beliefSnapshots;

    private final Map<Value, Map<Value, Set<Formula>>> intendSnapShots;

    public AgentSnapShot(Map<Value, Map<Value, Set<Formula>>> knowledgeSnapshots,
                         Map<Value, Map<Value, Set<Formula>>> beliefSnapshots,
                         Map<Value, Map<Value, Set<Formula>>> intendSnapShots) {

        this.knowledgeSnapshots = knowledgeSnapshots;
        this.beliefSnapshots = beliefSnapshots;

        this.intendSnapShots = intendSnapShots;
    }

    public static AgentSnapShot from(Set<Formula> formulae) {

        Map<Value, Map<Value, Set<Formula>>> knowledgeSnapshots = CollectionUtils.newMap();
        Map<Value, Map<Value, Set<Formula>>> beliefSnapshots = CollectionUtils.newMap();

        Map<Value, Map<Value, Set<Formula>>> intendSnapShots = CollectionUtils.newMap();

        Set<Knowledge> knowledges = formulae.stream().
                filter(f -> f instanceof Knowledge).map(f -> (Knowledge) f).
                collect(Collectors.toSet());

        Set<Belief> beliefs = formulae.stream().
                filter(f -> f instanceof Belief).map(f -> (Belief) f).
                collect(Collectors.toSet());

        Set<Intends> intends = formulae.stream().
                filter(f -> f instanceof Intends).map(f -> (Intends) f).
                collect(Collectors.toSet());

        Set<Value> agents = Logic.allAgents(formulae);
        Set<Value> times = Logic.allTimes(formulae);


        agents.stream().forEach(agent -> {

                    Map<Value, Set<Formula>> timeMap = CollectionUtils.newMap();
                    times.forEach(time->{
                        timeMap.put(time, CollectionUtils.newEmptySet());
                    });

                    knowledgeSnapshots.put(agent, timeMap);
                }

        );

        agents.stream().forEach(agent -> {

                    Map<Value, Set<Formula>> timeMap = CollectionUtils.newMap();
                    times.forEach(time->{
                        timeMap.put(time, CollectionUtils.newEmptySet());
                    });

                    beliefSnapshots.put(agent, timeMap);
                }

        );

          agents.stream().forEach(agent -> {

                    Map<Value, Set<Formula>> timeMap = CollectionUtils.newMap();
                    times.forEach(time->{
                        timeMap.put(time, CollectionUtils.newEmptySet());
                    });

                    intendSnapShots.put(agent, timeMap);
                }

        );

        knowledges.stream().forEach(knowledge -> {

            Value agent = knowledge.getAgent();
            Value time = knowledge.getTime();

            knowledgeSnapshots.get(agent).get(time).add(knowledge.getFormula());

        });

        beliefs.stream().forEach(belief -> {

            Value agent = belief.getAgent();
            Value time = belief.getTime();

            beliefSnapshots.get(agent).get(time).add(belief.getFormula());

        });

        intends.stream().forEach(intend -> {

            Value agent = intend.getAgent();
            Value time = intend.getTime();

            intendSnapShots.get(agent).get(time).add(intend.getFormula());

        });


        return new  AgentSnapShot(knowledgeSnapshots, beliefSnapshots, intendSnapShots);
    }


    public Set<Formula> computeSnapShot(Map<Value, Map<Value, Set<Formula>>> allSnapShots, Value agent, Value time) {

        if(!allSnapShots.containsKey(agent)){
            return CollectionUtils.newEmptySet();
        }
        Map<Value, Set<Formula>> timeMap = allSnapShots.get(agent);

        if(!(time instanceof Constant)){
            //Call an oracle from Value.compareTo
            return CollectionUtils.newEmptySet();

        }

        return timeMap.entrySet().stream().filter(valueSetEntry -> {
            Value t = valueSetEntry.getKey();
            return time.compareTo(t) >= 0;

        }).map(valueSetEntry -> valueSetEntry.getValue())
        .reduce(CollectionUtils.newEmptySet(), CollectionUtils::union);
    }

    public Set<Formula> allKnownByAgentTillTime(Value agent, Value time) {

        return computeSnapShot(knowledgeSnapshots, agent, time);

    }
    public Set<Formula> allBelievedByAgentTillTime(Value agent, Value time) {

        return computeSnapShot(beliefSnapshots, agent, time);

    }

    public Set<Formula> allIntendedByAgentTillTime(Value agent, Value time) {

        return computeSnapShot(intendSnapShots, agent, time);

    }


}
