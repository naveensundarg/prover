from py4j.java_gateway import JavaGateway
from py4j.java_gateway import GatewayParameters

gateway = None

def start():
    global gateway
    if not (gateway):
        gateway = JavaGateway(gateway_parameters=GatewayParameters(address=u'py-shadowprover'))

def stop():
    global gateway
    gateway = None


def prove(assumptions, goal):

    global gateway
    if not gateway:
        start()

    lst = gateway.newEmptyList()
    for assumption in assumptions:
        lst.append(assumption)

    return gateway.prove(lst, goal)


def proveFromDescription(description):
    global gateway
    if not gateway:
        start()

    return gateway.proveFromDescription(description)
