def uniformCostSearch(problem):
    """Search the node of least total cost first."""
    pQueue = PriorityQueue();
    explored = []
    head = Node(problem.getStartState(), list(), None, 0, 0)
    pQueue.push(head, 0)

    while (pQueue.isEmpty() == False):
    	top = pQueue.pop()
    	if (problem.isGoalState(top.currentState) == True):
    		return top.action
        if (top.currentState in explored):
            continue
    	explored.append(top.currentState)
    	childList = problem.getSuccessors(top.currentState)
    	for child in childList:
    		childState = child[0]
    		if not childState in explored:
    			actionList = copy.deepcopy(top.action)
    			actionList.append(child[1])
    			currentPcost = top.stepCost + child[2]
    			pQueue.push(Node(childState, actionList, top, child[2], currentPcost), currentPcost)
    return list()

def nullHeuristic(state, problem=None):
    """
    A heuristic function estimates the cost from the current state to the nearest
    goal in the provided SearchProblem.  This heuristic is trivial.
    """
    return 0

def aStarSearch(problem, heuristic=nullHeuristic):
    """Search the node that has the lowest combined cost and heuristic first."""
    pQueue = PriorityQueue();
    explored = []
    head = Node(problem.getStartState(), list(), None, 0, 0)
    pQueue.push(head, heuristic(problem.getStartState(), problem))

    while (pQueue.isEmpty() == False):
    	top = pQueue.pop()
    	if (problem.isGoalState(top.currentState) == True):
    		return top.action
        if (top.currentState in explored):
            continue
    	explored.append(top.currentState)
    	childList = problem.getSuccessors(top.currentState)
    	for child in childList:
    		childState = child[0]
    		if not childState in explored:
    			actionList = copy.deepcopy(top.action)
    			actionList.append(child[1])
    			currentPcost = top.pathCost + child[2]
    			aStarCost = currentPcost + heuristic(top.currentState, problem)
    			pQueue.push(Node(childState, actionList, top, child[2], currentPcost), aStarCost)
    return list()
