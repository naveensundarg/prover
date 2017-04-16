package com.logic.prover;

import org.testng.IAnnotationTransformer;
import org.testng.TestNG;
import org.testng.annotations.ITestAnnotation;
import org.testng.xml.XmlClass;
import org.testng.xml.XmlSuite;
import org.testng.xml.XmlTest;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by naveensundarg on 3/30/17.
 */
public class TestNGEntryPoint {


        public TestNGEntryPoint(String className, String methodName) {
        // Create Suite List
        List suites = new ArrayList();

        // Add Suite to Suite List
        XmlSuite suite = new XmlSuite();
        suites.add(suite);
        suite.setName("Deontic Cognitive Calculus Tests Suite");

        // Add Test to Suite
        XmlTest test = new XmlTest(suite);
        test.setName("Deontic Cognitive Calculus Tests Suite");

        // Add Class List to Test
        List classes = new ArrayList();
        test.setXmlClasses(classes);

        // Add Class to Class List
        XmlClass clazz = new XmlClass(className);
        classes.add(clazz);

        // Run TestNG
        TestNG testNG = new TestNG();
        testNG.setXmlSuites(suites);
        testNG.addListener(new TestNGAnnotationTransformer(methodName));
        testNG.run();
    }

    public static class TestNGAnnotationTransformer implements IAnnotationTransformer {
        String methodToRun;
        public TestNGAnnotationTransformer(String methodName) {
            methodToRun = methodName;
        }
        public void transform(ITestAnnotation annotation, Class arg1, Constructor arg2, Method testMethod) {
            if (methodToRun.equals(testMethod.getName())) {
                annotation.setEnabled(true);
            }
        }
    }

}
