package com.naveensundarg.shadow.prover;

import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Reader;
import py4j.GatewayServer;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.*;
import java.util.stream.Collectors;


public final class Py4JServer {


    private CognitiveCalculusProver cognitiveCalculusProver;


    public Py4JServer(){

        cognitiveCalculusProver = new CognitiveCalculusProver();
    }


    public CognitiveCalculusProver getCognitiveCalculusProver(){
        return cognitiveCalculusProver;
    }

    public static void main(String[] args) throws UnknownHostException {

        System.out.println("--------------- Starting GatewayServer --------------- ");
        System.out.println("--------------- Started Py4J Gateway   --------------- ");

        InetAddress addr;
        System.setProperty("java.net.preferIPv4Stack", "true");
        addr = Inet4Address.getByName("0.0.0.0");
        GatewayServer server = new GatewayServer(new Py4JServer(),25333, 25334, addr,addr, 0, 0, null);
        System.out.println("--------------- Started Py4J Gateway   --------------- ");

        server.start();

    }

    public ArrayList newEmptyList(){

        return new ArrayList();
    }
    public String prove(ArrayList assumptionsArrayList, String goal) {

        boolean error ;
        StringBuilder  s = new StringBuilder("");

        ArrayList<String> assumptionsArrayStringList = new ArrayList<String>();

        assumptionsArrayList.forEach(x->{
            assumptionsArrayStringList.add(x.toString());
        });

        Set<Formula> assumptionsSet = assumptionsArrayStringList.stream().map(x->{
            try {
                return Reader.readFormulaFromString(x);
            } catch (Reader.ParsingException e) {
                s.append(e.getMessage());
                return null;
            }
        }).collect(Collectors.toSet());;

        error = assumptionsSet.stream().anyMatch(Objects::isNull);

        Formula goalFormula = null;
        try {
            goalFormula = Reader.readFormulaFromString(goal);
        } catch (Reader.ParsingException e) {
            s.append(e.getMessage());
            error = true;
        }

        if(error) {
            return s.toString();
        }

        Optional<Justification> optionalJustification =  cognitiveCalculusProver.prove(assumptionsSet, goalFormula);

        if(optionalJustification.isPresent()) {
            return optionalJustification.get().toString();
        }
        else {
            return "FAILED";
        }
    }
}