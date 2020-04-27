package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;

public interface UnaryModalFormula {
    Value getAgent();

    Value getTime();

    Formula getFormula();

}
