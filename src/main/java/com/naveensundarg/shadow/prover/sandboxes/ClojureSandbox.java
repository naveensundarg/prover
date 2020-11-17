package com.naveensundarg.shadow.prover.sandboxes;


import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.Namespace;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

class ClojureSandbox{


    static {



    }

    public  static void main(String[] args) {
        run1();
        //run2();;
    }
    public static void run1(){
        // Load the `require` function
        // require the our clojure interpreter namespace

        IFn require = Clojure.var("clojure.core", "require");
        IFn loadFile = Clojure.var("clojure.core", "load-file");

       loadFile.invoke("hyperlog/tools.clj");
        loadFile.invoke("hyperlog/core.clj");


        require.invoke(Clojure.read("interpreter.core"));

        // Load extra operations we can use in our script
        IFn load_tools = Clojure.var("interpreter.core", "load-tools!");
        load_tools.invoke();

        // load the `eval-script`
        IFn eval_script = Clojure.var("interpreter.core", "eval-script");

        // execute it
        System.out.println(eval_script.invoke("(reverse \"naveen\")", Sets.newSet()));



    }

    public static void run2(){
        // Load the `require` function
        // require the our clojure interpreter namespace

        IFn require = Clojure.var("clojure.core", "require");
        IFn loadFile = Clojure.var("clojure.core", "load-file");
        loadFile.invoke("tools.clj");
        loadFile.invoke("core.clj");

        require.invoke(Clojure.read("interpreter.core"));

        // Load extra operations we can use in our script
        IFn load_tools = Clojure.var("interpreter.core", "load-tools!");
        load_tools.invoke();

        // load the `eval-script`
        IFn eval_script = Clojure.var("interpreter.core", "eval-script");

        System.out.println(eval_script.invoke("(do  " +
                " (foo 2 3))", CollectionUtils.listOf()));

    }
}