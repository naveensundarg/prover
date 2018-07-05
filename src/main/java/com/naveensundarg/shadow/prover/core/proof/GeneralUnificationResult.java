package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.FormulaVariable;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.Collections;
import java.util.Map;

/**
 * Created by naveensundarg on 7/26/16.
 */
public class GeneralUnificationResult {

    public static final GeneralUnificationResult FAILED = new  GeneralUnificationResult(Collections.emptyMap(), Collections.emptyMap());

    private  Map<Variable, Value> valueMap;

    private  Map<FormulaVariable, Value> formulaMap;


   public GeneralUnificationResult(Map<Variable, Value> valueMap, Map<FormulaVariable, Value> formulaMap) {

        this.valueMap = valueMap;
        this.formulaMap = formulaMap;

   }

    public Map<Variable, Value> getValueMap() {
        return valueMap;
    }

    public Map<FormulaVariable, Value> getFormulaMap() {
        return formulaMap;
    }

    @Override
    public String toString() {
        return "GeneralUnificationResult{" +
                "valueMap=" + valueMap +
                ", formulaMap=" + formulaMap +
                '}';
    }



}
