#!/usr/bin/env python3
import sys
from timewreport.parser import TimeWarriorParser
from datetime import datetime, timedelta
import report_utils

parser = TimeWarriorParser(sys.stdin)
config = parser.get_config()

def print_time_sum(date, time_sum):
    if time_sum > timedelta(hours=6) and time_sum < timedelta(hours=9):
        time_sum = time_sum + timedelta(minutes=30)
    elif time_sum >= timedelta(hours=9):
        time_sum = time_sum + timedelta(minutes=45)
    print("{:{width}} |  {:>10} |  {:>10} |  {:>10} | {:>10} | {:<{awidth}} |".format("Gesamt", date.strftime("%d.%m.%Y"), "", str(time_sum), "", "", width=max_width, awidth=80))

if "temp.report.start" not in config.get_dict() or not parser.get_intervals():
    print("There is no data in the database")
    exit()

start = report_utils.get_start(config)
end = report_utils.get_end(config)
business_days = report_utils.get_business_days(start, end)

interval_dict_date_tag = report_utils.nested_dict()

for interval in parser.get_intervals():
    if not interval_dict_date_tag[interval.get_start_date()][interval.get_tags()[0]]:
        interval_dict_date_tag[interval.get_start_date()][interval.get_tags()[0]] = []
    interval_dict_date_tag[interval.get_start_date()][interval.get_tags()[0]].append(interval) 

print(f"Daily Totals by Tag, for {start:%Y-%m-%d %H:%M:%S} - {end:%Y-%m-%d %H:%M:%S}")
print("")
print("")

max_width = 20
intervals = []
total_line = {}
for date in interval_dict_date_tag:
    for tag in interval_dict_date_tag[date]:
        report_line = {}
        for interval in interval_dict_date_tag[date][tag]:
            report_line["date"] = date
            report_line["tag"] = tag

            if len(tag) > max_width:
                max_width = len(tag)

            if "start" not in report_line:
                report_line["start"] = interval.get_start()
            report_line["end"] = interval.get_end()
            
            if "duration" not in report_line:
                report_line["duration"] = interval.get_duration()
            else:
                report_line["duration"] = report_line["duration"] + interval.get_duration()
            
            if interval.get_annotation():
                if "annotations" not in report_line:
                    report_line["annotations"] = []
                    report_line["annotations"].append(interval.get_annotation())
                else:
                    if interval.get_annotation() not in report_line["annotations"]:
                        report_line["annotations"].append(interval.get_annotation())

            if "duration" not in total_line:
                total_line["duration"] = interval.get_duration()
            else:
                total_line["duration"] = total_line["duration"] + interval.get_duration()

        intervals.append(report_line)

if config.get_boolean("color", True):
    print(
        "\x1b[4m{:{width}} |\x1b[4m{:>10}   |\x1b[4m{:>10}   |\x1b[4m{:>10}   |\x1b[4m{:>10}  |\x1b[4m{:^{awidth}}  \x1b[4m\x1b[0m|".format("Tag", "Date", "Start", "Duration", "End", "Annotations", width=max_width, awidth=80)
    )
else:
    print("{:{width}} {:>10} {:>10}  {:>10}  {:>10}  {:>10}  {:>10}  ".format("Tag", "Date", "Start", "Duration", "End", "Annotations", width=max_width))
    print(f"{'-' * max_width} {'----------'}")

date = intervals[0]["date"] - timedelta(days=10)
time_sum = timedelta(seconds=0)
for idx, interval in enumerate(intervals):
    if interval["date"] > date:
        if idx > 0:
            print_time_sum(date, time_sum)
        date = interval["date"]
        time_sum = interval["duration"]
    else:
        time_sum += interval["duration"]

    annotation = ""
    if "annotations" in interval:
        annotation = "/ ".join(interval["annotations"])
    print("{:{width}} |  {:>10} |  {:>10} |  {:>10} | {:>10} | {:<{awidth}} |".format(interval["tag"], interval["date"].strftime("%d.%m.%Y"), interval["start"].strftime("%H:%M"), str(interval["duration"]), interval["end"].strftime("%H:%M"), annotation, width=max_width, awidth=80))

print_time_sum(date, time_sum)


print("")
print("Working Hours: {:.2f}".format(total_line["duration"].total_seconds()/60/60))
print("Working Days: {:.2f}".format(total_line["duration"].total_seconds()/60/60/8))
print("Possible Days: " + str(business_days))


