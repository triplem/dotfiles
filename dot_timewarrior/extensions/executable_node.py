#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# based on Timewarrior extension: catreport
# https://github.com/Fjanks/timew-catreport
# Author: Frank Stollmeier
# License: MIT
import datetime

class Node(dict):
    '''This node represents a task or a category of tasks and may contain other nodes.'''
    
    def __init__(self, name, parent):
        '''
        Parameters
        ----------
        name:   string
        parent: instance of Node()
        '''
        self.intervals = []
        self.name = name
        self.parent = parent
        dict.__init__(self)
    
    def get_node(self, path_to_node, create_if_not_existing = False):
        '''Return a node which is somewhere deeper in the hierarchy. 
        If the specified node does not exist return None. If create_if_not_existing is True and the specified node does not exist, create the node and return the new node.
        
        Parameters
        ----------
        path_to_node:           list of strings, e. g. ["projectA", "subprojectA1", "task13"]
        create_if_not_existing: bool, optional, default is False.
        '''
        if len(path_to_node) == 0:
            return self
        else:
            child = path_to_node.pop(0)
            if child in self:
                return self[child].get_node(path_to_node, create_if_not_existing)
            elif create_if_not_existing:
                self[child] = Node(child, self)
                return self[child].get_node(path_to_node, create_if_not_existing)
            else:
                return None
    
    def add_node(self, path_to_node):
        '''Add a new node and return it.
        
        Parameters
        ----------
        path_to_node:   list of strings, e. g. ["projectA", "subprojectA1", "task13"]
        '''
        return self.get_node(path_to_node, create_if_not_existing = True)
    
    def is_leaf(self):
        '''Return True, if the node has no child nodes, and False, if it has child nodes.'''
        return len(self) == 0

    def get_duration_timedelta(self):
        '''Return the total number of seconds spend in this task/category excluding time spend in subcategories.'''
        return sum([i.get_duration() for i in self.intervals], datetime.timedelta())

    def get_duration(self):
        '''Return the total number of seconds spend in this task/category excluding time spend in subcategories.'''
        return sum([i.get_duration().total_seconds() for i in self.intervals])
    
    def get_cumulated_duration(self):
        '''Return the total number of seconds spend in this task/category including the spend in subcategories.'''
        return self.get_duration() + sum([child.get_cumulated_duration() for child in self.values()])

    def get_intervals_by_year_month(self, year, month):
        return list(filter(lambda x: x.get_start_date().year == year and x.get_start_date().month == month, self.intervals))

    def get_duration_by_year_month(self, year, month):
        '''Return the total number of seconds spend in this task/category excluding time spend in subcategories.'''        
        return sum([i.get_duration().total_seconds() for i in self.get_intervals_by_year_month(year, month)])
    
    def get_cumulated_duration_by_year_month(self, year, month):
        '''Return the total number of seconds spend in this task/category including the spend in subcategories.'''
        return self.get_duration_by_year_month(year, month) + sum([child.get_cumulated_duration_by_year_month(year, month) for child in self.values()])

    def get_lowest_start(self):
        '''Return the (lowest) start time in the intervals assigned to this node (excl. subnodes)'''
        starts = [i.get_start() for i in self.intervals]
        return min(starts)

    def get_highest_end(self):
        '''Returns the (highest) end time in the intervals assigned to this node (excl. subnodes)'''
        ends = [i.get_end() for i in self.intervals]
        return max(ends)
