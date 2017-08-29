package com.naveensundarg.shadow.prover.representations;

import java.io.Serializable;

/**
 * Created by naveensundarg on 7/24/16.
 */
public abstract  class Phrase implements Serializable {



     protected int safeHashCode(Object o) {
       return (o != null) ? o.hashCode() : 0;
     }
}
