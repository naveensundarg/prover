import subprocess
from py4j.java_gateway import JavaGateway


q = None
gateway = None

def start():
    global q, gateway
    if(not(q)):
        q = subprocess.Popen("mvn exec:java -Dexec.mainClass=com.naveensundarg.shadow.prover.Py4JServer".split())
        gateway = JavaGateway()

def stop():
    global q, gateway
    if(q):
        q.kill()
        gateway = None


def prove(assumptions, goal):

    global gateway
    if(not(gateway)):
        start()

    lst = gateway.jvm.java.util.ArrayList()

    for assumption in assumptions:
        lst.append(assumption)

    return gateway.prove(lst, goal)