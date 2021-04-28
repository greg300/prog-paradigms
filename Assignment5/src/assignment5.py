from typing import List
from functools import reduce
import sys
import traceback
import copy

#################
### Problem  1 ##
#################

def closest_to (l, v):
    if len(l) == 0:
        return None
    closestElement = l[0]
    closestDifference = v
    for i in range(len(l)):
        diff = abs(v - l[i])
        if diff < closestDifference:
            closestDifference = diff
            closestElement = l[i]
    return closestElement

#################
### problem  2 ##
#################

def assoc_list (l):
    assocList = []
    elementsInAssocList = []

    for i in range(len(l)):
        if l[i] in elementsInAssocList:
            for a in range(len(assocList)):
                if assocList[a][0] == l[i]:
                    assocList[a][1] += 1
                    break
        else:
            assocList.append([l[i], 1])
            elementsInAssocList.append(l[i])

    assocTuples = []
    for i in range(len(assocList)):
        assocTuples.append((assocList[i][0], assocList[i][1]))

    return assocTuples

#################
### Problem  3 ##
#################

def buckets (f, l):
    bucketList = []

    while len(l) > 0:
        foundBucket = False
        for j in range(len(bucketList)):
            if f(l[0], bucketList[j][0]):
                bucketList[j].append(l[0])
                l.remove(l[0])
                foundBucket = True
                break

        if not foundBucket:
            bucketList.append([l[0]])
            l.remove(l[0])

    return bucketList

###################################
# Definition for a binary tree node
###################################

class TreeNode:
    def __init__(self, val=None, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

    # construct tree from a list of values `ls`
    def list_to_tree(self, ls):
        self.val = self.left = self.right = None # clear the current tree

        if not ls: # ls is None or l == []
            return # tree is None

        i = 0
        self.val = ls[i]
        queue = [self]
        while queue: # while queue is not empty
            i += 1
            node = queue.pop(0)
            if node.val is None:
                continue

            if 2*i -1 >= len(ls) or ls[2*i-1] is None:
                pass
            else:
                node.left = TreeNode(ls[2*i-1])
                queue.append(node.left)

            if 2*i >= len(ls) or ls[2*i] is None:
                pass
            else:
                node.right = TreeNode(ls[2*i])
                queue.append(node.right)


#################
### Problem  4 ##
#################

def level_order(root: TreeNode):
    if root is None:
        return []
    level = 0
    levelOrderList = []
    currentNodesToCheck = [root]
    nextNodesToCheck = []

    while len(currentNodesToCheck) > 0:
        levelOrderList.append([])
        nextNodesToCheck = []
        for i in range(len(currentNodesToCheck)):
            curr = currentNodesToCheck.pop(0)
            if curr is None:
                continue
            levelOrderList[level].append(curr.val)
            nextNodesToCheck.append(curr.left)
            nextNodesToCheck.append(curr.right)
        
        currentNodesToCheck = copy.copy(nextNodesToCheck)
        level += 1

    for i in range(len(levelOrderList)):
        if levelOrderList[i] == []:
            levelOrderList.remove(levelOrderList[i])

    return levelOrderList


#################
### Problem  5 ##
#################

def pathSumAux(root: TreeNode, rootLeafPaths, rootLeafPathsSums, pathNum, currPath, currSum):
    if root is None:
        return

    newPath = currPath + [root.val]
    newSum = currSum + root.val

    if pathNum[0] >= len(rootLeafPaths):  # There is no entry in the list of paths for the current path.
        rootLeafPaths.append(newPath)
        rootLeafPathsSums.append(newSum)
    else:  # There is already an entry in the list of paths for the current path.
        rootLeafPaths[pathNum[0]].append(root.val)
        rootLeafPathsSums[pathNum[0]] += root.val

    if root.left is None and root.right is None:
        pathNum[0] = pathNum[0] + 1
        return
    else:
        newPathCopy = copy.copy(newPath)
        pathSumAux(root.left, rootLeafPaths, rootLeafPathsSums, pathNum, newPath, newSum)
        pathSumAux(root.right, rootLeafPaths, rootLeafPathsSums, pathNum, newPathCopy, newSum)


def pathSum(root: TreeNode, targetSum: int) -> List[List[int]]:
    sumPaths = []  # To be returned.
    rootLeafPaths = []
    rootLeafPathsSums = []

    pathSumAux(root, rootLeafPaths, rootLeafPathsSums, [0], [], 0)

    for i in range(len(rootLeafPathsSums)):
        if rootLeafPathsSums[i] == targetSum:
            sumPaths.append(rootLeafPaths[i])
    
    return sumPaths


#################
### Test cases ##
#################

def main():
    print ("Testing your code ...")
    error_count = 0

    # Testcases for Problem 1
    try:
        assert (closest_to([2,4,8,9],7) == 8)
        assert (closest_to([2,3,7,9],5) == 3)
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
        # tb_info = traceback.extract_tb(tb)
        # filename, line, func, text = tb_info[-1]
        # print('An error occurred on line {} in statement {}'.format(line, text))
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    # Testcases for Problem 2
    try:
        result = assoc_list([1, 2, 2, 1, 3])
        result.sort(key=lambda x:x[0])
        assert (result == [(1,2), (2, 2), (3, 1)])

        result = assoc_list(["a","a","b","a"])
        result.sort(key=lambda x:x[0])
        assert (result == [("a",3), ("b",1)])

        result = assoc_list([1, 7, 7, 1, 5, 2, 7, 7])
        result.sort(key=lambda x:x[0])
        assert (result == [(1,2), (2,1), (5,1), (7,4)])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    # Testcases for Problem 3
    try:
        assert (buckets (lambda a, b : a == b, [1,2,3,4]) == [[1], [2], [3], [4]])
        assert (buckets (lambda a, b : a == b, [1,2,3,4,2,3,4,3,4]) == [[1], [2, 2], [3, 3, 3], [4, 4, 4]])
        assert (buckets (lambda a, b : a % 3 == b % 3, [1,2,3,4,5,6]) == [[1, 4], [2, 5], [3, 6]])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    ### Specify 3 trees for testing problems 4 & 5
    root_1 = TreeNode()
    root_1.list_to_tree([5,4,8,11,None,13,4,7,2,None,None,5,1])

    root_2 = TreeNode()
    root_2.list_to_tree([1,2,3])

    root_3 = TreeNode()
    root_3.list_to_tree([1,2])

    # Testcases for Problem 4
    try:
        assert (level_order(root_1) == [[5], [4, 8], [11, 13, 4], [7, 2, 5, 1]])
        assert (level_order(root_2) == [[1], [2, 3]])
        assert (level_order(root_3) == [[1], [2]])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    # Testcases for Problem 5
    try:
        assert (pathSum(root_1, 22) == [[5, 4, 11, 2], [5, 8, 4, 5]])
        assert (pathSum(root_2, 4) == [[1, 3]])
        assert (pathSum(root_3, 0) == [])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    print (f"{error_count} out of 5 programming questions are incorrect.")

main()
