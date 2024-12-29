#!/usr/bin/env python3
import sys
from timewreport.parser import TimeWarriorParser
from datetime import timedelta
from dateutil import rrule
import report_utils
from node import Node
import calendar

parser = TimeWarriorParser(sys.stdin)
config = parser.get_config()

if "temp.report.start" not in config.get_dict() or not parser.get_intervals():
    print("There is no data in the database")
    exit()

start = report_utils.get_start(config)
end = report_utils.get_end(config)
business_days = report_utils.get_business_days(start, end)

total_chargeable = int(config.get_value("total_chargeable", 180))

interval_dict_year_month_tag = report_utils.nested_dict()

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

root = store_intervals_in_tree(parser.get_intervals())



def print_report(root):
    '''Create the catreport.
    
    Parameters
    ----------
    root:   instance of class Node, as returned by store_intervals_in_tree()
    '''
    #tabular layout
    width_col1 = 20
    width_col2 = 12
    width_total = 10

    monthly_chargeable = total_chargeable / 12

    #print header
    print(f"Monthly by Tag, for {start:%Y-%m-%d %H:%M:%S} - {end:%Y-%m-%d %H:%M:%S} with Chargeable: {total_chargeable}")
    print("")
    print("")

    months = {1: "Jan", 2: "Feb", 3: "Mar", 4: "Apr", 5: "May", 6: "Jun", 7: "Jul", 8: "Aug", 9: "Sep", 10: "Oct", 11: "Nov", 12: "Dec"}
    headline = "{:<{wc}} |".format("Task", wc = width_col1)

    for dt in rrule.rrule(rrule.MONTHLY, dtstart=start, until=(end-timedelta(days=1))):
        headline = headline + " {:<{wc}} |".format(months[dt.month] + "/" + str(dt.year), wc = width_col2)

    headline = headline + " {:<{wc}} |".format("Total", wc = width_total)
    print(headline)

    separator = "="*width_col1 + "=|"
    for dt in rrule.rrule(rrule.MONTHLY, dtstart=start, until=(end-timedelta(days=1))):
        separator += "="*width_col2 + "==|"
    separator += "="*width_total + "==|"
    print(separator)

    def print_recursively(node, level = 0):
        #print line
        hours = node.get_cumulated_duration()/(60*60)
        days = hours/8
        if node.parent is None:
            share = 100
        else:
            share = 100 * hours / (node.parent.get_cumulated_duration()/(60*60))
        shift = level * '  '

        line = "{:<{wc}} |".format(shift + node.name, wc = width_col1)

        total_hours = 0
        for dt in rrule.rrule(rrule.MONTHLY, dtstart=start, until=(end-timedelta(days=1))):
            hours = node.get_cumulated_duration_by_year_month(dt.year, dt.month)/(60*60)
            days = hours/8

            if level == 0:
                business_days = report_utils.get_business_days_by_year_month(dt.year, dt.month)
                formatted = "{:>.2f}/{:<}".format(days, business_days)
                line = line + " {:>{wc}} |".format(formatted, wc = width_col2)
            else:
                if days > 0:
                    days_formatted = "{:>.2f}".format(days)
                else:
                    days_formatted = ""

                if node.name == "Customer":
                    days_formatted = days_formatted + "/" + "{:<.2f}".format(monthly_chargeable)

                line = line + " {:>{wc}} |".format(days_formatted, wc = width_col2)

            total_hours = total_hours + hours

        if level == 0:
            business_days = report_utils.get_business_days(start, end)
            formatted = "{:>.2f}/{:<}".format(total_hours/8, business_days)
            line = line + " {:>{wc}} |".format(formatted, wc = width_total)
        else:
            if node.name == "Customer":
                formatted = "{:>.2f}/{:<}".format(total_hours/8, total_chargeable)
                line = line + " {:>{wc}} |".format(formatted, wc = width_total)
            else:
                line = line + " {:>{wc}.2f} |".format(total_hours/8, wc = width_total)
        print(line)

        #go down the tree
        for key in sorted(node.keys()):
            print_recursively(node[key], level + 1)
        
        if node.get_duration() > 0 and len(node) > 0:
            shift = (level + 1) * '  '

            com_line = "{0:<{wc}} |".format(shift + 'Allgemein', wc = width_col1)
            com_total_hours = 0
            for dt in rrule.rrule(rrule.MONTHLY, dtstart=start, until=(end-timedelta(days=1))):
                h = node.get_duration_by_year_month(dt.year, dt.month)/(60*60)
                d = h/8

                if d > 0:
                    com_line = com_line + " {:>{wc}.2f} |".format(d, wc = width_col2)
                else:
                    com_line = com_line + " {:>{wc}} |".format("", wc = width_col2)
                com_total_hours = com_total_hours + h

            com_line = com_line + " {:>{wc}.2f} |".format(com_total_hours/8, wc = width_total)
            print(com_line)
    
    print_recursively(root)
    print("\n")

print_report(root)
