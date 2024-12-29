#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#
# based on Timewarrior extension: catreport
# https://github.com/Fjanks/timew-catreport
# Author: Frank Stollmeier
# License: MIT
import sys
from node import Node
from timewreport.parser import TimeWarriorParser

def store_intervals_in_tree(intervals):
    '''Create and return a tree structure containing all tracked time intervals.
    
    Parameters
    ----------
    intervals:  list of intervals, as returned by TimeWarriorParser(stdin).get_intervals()
    '''
    root = Node('Totals', None)
    for interval in intervals:
        for tags in interval.get_tags():
            node = root.add_node(tags.split('.'))
            node.intervals.append(interval)
    return root

def print_report(root):
    '''Create the catreport.
    
    Parameters
    ----------
    root:   instance of class Node, as returned by store_intervals_in_tree()
    '''
    #tabular layout
    width_col1 = 60
    width_col2 = 30
    width_col3 = 30
    width_col4 = 30
    #print header
    print("\n")
    print("{0:<{wc1}}{1:<{wc2}}{2:<{wc3}}{3:<{wc4}}".format('Task', 'Time [h]', 'Days [d]', 'Share [%]', wc1 = width_col1, wc2 = width_col2, wc3 = width_col3, wc4 = width_col4))
    print((width_col1+width_col2+width_col3+width_col4)*"=")
    #print data
    def print_recursively(node, level = 0):
        #print line
        hours = node.get_cumulated_duration()/(60*60)
        days = hours/8
        if node.parent is None:
            share = 100
        else:
            share = 100 * hours / (node.parent.get_cumulated_duration()/(60*60))
        shift = level * '    '
        print("{0:<{wc1}}{1:<{wc2}}{2:<{wc3}}{3:<{wc4}}".format(shift + node.name, shift + "{:.2f}".format(hours), shift + "{:.2f}".format(days), shift + "{:.2f}".format(share) , wc1 = width_col1, wc2 = width_col2, wc3 = width_col3, wc4 = width_col4))
        
        #go down the tree
        for key in sorted(node.keys()):
            print_recursively(node[key], level + 1)
        if node.get_duration() > 0 and len(node) > 0:
            h = node.get_duration()/(60*60)
            d = h/8
            s = 100 * h / hours
            shift = (level + 1) * '    '
            print("{0:<{wc1}}{1:<{wc2}}{2:<{wc3}}{3:<{wc4}}".format(shift + 'Allgemein', shift + "{:.2f}".format(h), shift + "{:.2f}".format(d), shift + "{:.2f}".format(s) , wc1 = width_col1, wc2 = width_col2, wc3 = width_col3, wc4 = width_col4))
    
    print_recursively(root)
    print("\n")

    
def main(stdin):
    parser = TimeWarriorParser(stdin)
    tw_config = parser.get_config()
    tw_intervals = parser.get_intervals()
    
    root = store_intervals_in_tree(tw_intervals)
    print_report(root)
    #sys.exit(0)


if __name__ == "__main__":
    main(sys.stdin)
    #print(stdin.read())


######################################################################
##    The following code is just for development and debugging.     ##
######################################################################


def load_testdata(filename):
    '''This function allows testing functions with a static data set instead of the real data from timewarrior.
    To create a static data set from your real data, comment main(sys.stdin) and uncomment print(stdin.read()) in if __name__ == "__main__", and then run timew catreport > static-data
    '''
    with open(filename, "r") as f:
        parser = TimeWarriorParser(f)
    tw_config = parser.get_config()
    tw_intervals = parser.get_intervals()
    return tw_config.get_dict(), tw_intervals    

def test():
    config,intervals = load_testdata("./static-data")
    root = store_intervals_in_tree(intervals)
    print_report(root)

def show_intervals_with(keyword, intervals):
    '''Filter intervals by key.'''
    for i in intervals:
        if keyword in i.get_tags():
            print(i.get_date(),i.get_tags())
