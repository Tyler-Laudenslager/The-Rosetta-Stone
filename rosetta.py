#!/usr/bin/python3
#Tyler Laudenslager

import sys

def get_input():
    """
        takes a dependency list as input and returns
        a dictionary with the keys as the tasks
        and the values as a list of other tasks
        that need to be performed before the key
        task executes.

        Example:

        B        We can read this as B depends on A
        A        and C depends on D.
        C
        D       - input stored in a file
        C       - constrait if n = number of lines
        E        n >= 2 and n is even


        This function returns a dictionary in the
        form below.

        'B' : ['A']
        'C' : ['D','E']

        input - dependency list from standard input where
                every two lines can be paired together
                the first line depending on the second
                to execute first.

        returns - dict(string : [])


    """
    
    adjaceny_list = dict()
    #strip all whitespace and the newline character
    #from each line in the file
    read_sentences = [x.strip() for x in sys.stdin]

    #create task, dependent pairs from the sentences read from standard in
    for task, dependent in zip(read_sentences[0::2],read_sentences[1::2]):
        #if the task is already in the dictionary
        #add a new dependent task
        if task in set(adjaceny_list.keys()):
            adjaceny_list[task] += [dependent]
        #if the task is not in the dictionary add
        #the task to the dictionary
        else:
            adjaceny_list[task] = [dependent]

    return adjaceny_list

def find_no_dependencies(adjaceny_list):
    """ find all starting tasks that do not
        have dependencies.

        Example:

        'B' : ['A']
        'C' : ['D','E']

        returns - ['A','D','E]

        Notice how the tasks 'A', 'D, and 'E'
        are not keys in the dictionary. These tasks
        can be executed right away, because
        they are independent they do not depend
        on other tasks executing before them.

        input - data from the get_input() function
                - dictionary(string : [])

        returns - list that contains the starting
                  independent tasks that are in ascending
                  order with duplicates removed.

    """
    items = list()
    for _ , value in adjaceny_list.items():
        for item in value:
            #if a task is in the keys
            #it cannot be a independent task
            if item in set(adjaceny_list.keys()):
                continue
            #if a task is not in the keys
            #we add it to the list
            elif item not in set(items):
                items.append(item)
                
    return sorted(items)

def topo_sort(adjaceny_list):
    """ Sorts the tasks in the adjaceny list into
        topological order. All arbitrary tasks
        that have no dependencies get processed
        using the < operator as the final ordering
        of tasks. This guarentees that the ordering
        is deterministic.

        input - adjaceny_list - dict(string: [])
                                return value of
                                get_input()

        returns - [] final topological sort of the
                     tasks
 
        Example:

        'B' : ['A']
        'C' : ['D','E']

        topo_sort will return ['A','B','D','E','C']

        The example reads B depends on A to execute first
        before B can be processed. A, D, E do not depend
        on other tasks to be processed before they can
        be executed. However, since A comes before D and E
        we process A first then since A is processed we can
        process B because now B does not depend on another
        task. This logic applies for the rest of the adjaceny
        list as well.

    """
    #assume there is no cycle
    cycle = False
    #create the task list
    task_queue = list()
    #find all nodes in the graph that have no incoming edges
    no_dependents = find_no_dependencies(adjaceny_list)

    while no_dependents:
        #make sure we maintain the sorted order
        #of the tasks that have no dependent tasks
        no_dependents = sorted(no_dependents)
        new_node = no_dependents.pop(0)
        #add the task to the final ordering
        task_queue.append(new_node)
        #get all the keys that associate the task that was
        #just added to the final list 
        dependents = [x for x in adjaceny_list.keys() if new_node in set(adjaceny_list[x])]
        #remove the newly added task from the entire dictionary
        for node in sorted(dependents):
            adjaceny_list[node].remove(new_node)
            #if the key does not have any tasks it
            #depends on add the key to the no dependents
            #list
            if len(adjaceny_list[node]) == 0:
                no_dependents.insert(0,node)

    #if there is a key that associates
    #with a non empty list there is a cycle
    for value in adjaceny_list.values():
        if not value == []:
            cycle = True
        else:
            continue
    if cycle:
        return ["cycle"]
    else:
        return task_queue

#entry point of the program
def main():

    #print the contents of the list that
    #topo_sort returns seperated by a newline
    #character
    print(*topo_sort(get_input()),sep="\n")


#if this file is being run as the main file
#execute the code below in this case call
#the function main(). if the file is being
#imported from another file do not call
#main()
if __name__ == "__main__":
    main()





             
