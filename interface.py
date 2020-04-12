import subprocess
from py4j.java_gateway import JavaGateway

gateway = None

def start():
    #global q,
    global gateway
    if(not(gateway)):
        #q = subprocess.Popen("mvn exec:java -Dexec.mainClass=com.naveensundarg.shadow.prover.Py4JServer".split())
        gateway = JavaGateway()

def stop():
    #global q,
    global gateway
    if(gateway):
        #q.kill()
        gateway = None


def prove(assumptions, goal):

    global gateway
    if(not(gateway)):
        start()

    lst = gateway.newEmptyList()

    for assumption in assumptions:
        lst.append(assumption)

    return gateway.prove(lst, goal)