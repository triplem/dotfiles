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

interval_dict_year_month_tag = report_utils.nested_dict()

def store_intervals_in_tree(intervals):
    '''Create and return a tree structure containing all tracked time intervals.

    Parameters
    ----------
    intervals:  list of intervals, as returned by TimeWarriorParser(stdin).get_intervals()
    '''
    root = Node('Totals', None)
    for interval in intervals:
        node = root.add_node([interval.get_start_date().strftime("%Y-%m-%d")])
        node.intervals.append(interval)
        for tags in interval.get_tags():
            tag_node = node.add_node([tags])
            tag_node.intervals.append(interval)

    return root

root = store_intervals_in_tree(parser.get_intervals())

default_width = 10
tag_width = 20
annotation_width = 50
print("{:{width}} | {:<{tag_width}} | {:<5} | {:<11} | {:>10} | {:<30}".format("Date", "Tag", "Start", "End", "Duration", "Annotations", width=default_width, tag_width=tag_width))
print(f"{'-' * default_width} | {'-' * tag_width} | {'-' * 5} | {'-' * 11} | {'-' * 10} | {'-' * 120} " )

total_duration = timedelta(minutes=0)
for date in sorted(root.keys()):
    if not root[date].is_leaf():
        parent = root[date]
        cur_date = parent.get_lowest_start().strftime("%d.%m.%Y")
        for tag in sorted(parent.keys()):
            node = parent[tag]
            annotation_list = list(dict.fromkeys([i.get_annotation() for i in root[date][tag].intervals]))
            annotations = "/ ".join(a or '' for a in annotation_list)
            print("{:{width}} | {:<{tag_width}} | {:<5} | {:<11} | {:>10} | {:<120}".format(cur_date, tag, '', '', str(node.get_cumulated_duration()/(60*60)), annotations, width=default_width, tag_width=tag_width))
            intervals = root[date][tag].intervals

        duration = parent.get_duration()/(60*60)
        calc_duration = parent.get_duration_timedelta()
        if calc_duration > timedelta(hours=6) and calc_duration < timedelta(hours=9):
            calc_duration += timedelta(minutes=30)
        elif calc_duration>=timedelta(hours=9):
            calc_duration += timedelta(minutes=45)
        calc_end = (parent.get_lowest_start() + calc_duration).strftime("%H:%M")
        calc_dur = str(calc_duration.total_seconds()/(60*60))

        total_duration += parent.get_duration_timedelta()

        print("{:{width}} | {:<{tag_width}} | {:<5} | {:<11} | {:>10} | {:<120}".format(cur_date, 'Gesamt', parent.get_lowest_start().strftime("%H:%M"), parent.get_highest_end().strftime("%H:%M") + "/" + calc_end, str(parent.get_duration()/(60*60)) + "/" + calc_dur, '', width=default_width, tag_width=tag_width))

start_str = start.strftime("%d.%m.%Y")
end_str = end.strftime("%d.%m.%Y")
dur_str = total_duration.total_seconds()/(60*60)
print("")
print(f"Timesheet for {start_str} - {end_str}")
print(f"Total worked time: {dur_str}")
