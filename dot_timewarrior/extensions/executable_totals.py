#!/usr/bin/env python3
###############################################################################
#
# Copyright 2016 - 2020, Thomas Lauf, Paul Beckingham, Federico Hernandez.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# https://www.opensource.org/licenses/mit-license.php
#
###############################################################################

import datetime
import json

import sys
from dateutil import tz

DATE_FORMAT = "%Y%m%dT%H%M%SZ"

def format_seconds_hours(seconds):
    """
    Convert seconds to a formatted string

    Convert seconds: 3661
    To formatted: "   1:01:01"
    """
    hours = seconds / 3600
    return "{:10.2f}".format(hours)

def format_seconds_days(seconds):
    """
    Convert seconds to a formatted string

    Convert seconds: 3661
    To formatted: "   1:01:01"
    """
    days = seconds / 3600 / 8
    return "{:10.2f}".format(days)

def calculate_totals(input_stream):
    from_zone = tz.tzutc()
    to_zone = tz.tzlocal()

    # Extract the configuration settings.
    header = 1
    configuration = dict()
    body = ""
    for line in input_stream:
        if header:
            if line == "\n":
                header = 0
            else:
                fields = line.strip().split(": ", 2)
                if len(fields) == 2:
                    configuration[fields[0]] = fields[1]
                else:
                    configuration[fields[0]] = ""
        else:
            body += line

    # Sum the seconds tracked by tag
    totals = dict()
    untagged = None
    j = json.loads(body)
    for object in j:
        start = datetime.datetime.strptime(object["start"], DATE_FORMAT)

        if "end" in object:
            end = datetime.datetime.strptime(object["end"], DATE_FORMAT)
        else:
            end = datetime.datetime.utcnow()

        tracked = end - start

        if "tags" not in object or object["tags"] == []:
            if untagged is None:
                untagged = tracked
            else:
                untagged += tracked
        else:
            for tag in object["tags"]:
                if tag in totals:
                    totals[tag] += tracked
                else:
                    totals[tag] = tracked

    # Determine largest tag width
    max_width = len("Total")
    for tag in totals:
        if len(tag) > max_width:
            max_width = len(tag)

    if "temp.report.start" not in configuration:
        return ["There is no data in the database"]

    start_utc = datetime.datetime.strptime(configuration["temp.report.start"], DATE_FORMAT)
    start_utc = start_utc.replace(tzinfo=from_zone)
    start = start_utc.astimezone(to_zone)

    if "temp.report.end" in configuration:
        end_utc = datetime.datetime.strptime(configuration["temp.report.end"], DATE_FORMAT)
        end_utc = end_utc.replace(tzinfo=from_zone)
        end = end_utc.astimezone(to_zone)
    else:
        end = datetime.datetime.now()

    if len(totals) == 0 and untagged is None:
        return [f"No data in the range {start:%Y-%m-%d %H:%M:%S} - {end:%Y-%m-%d %H:%M:%S}"]

    # Compose report header.
    output = ["", f"Total by Tag, for {start:%Y-%m-%d %H:%M:%S} - {end:%Y-%m-%d %H:%M:%S}", ""]

    # Compose table header.
    if configuration["color"] == "on":
        output.append(
            "\x1b[4m{:{width}} \x1b[0m \x1b[4m{:>10}\x1b[4m{:>10}\x1b[0m".format("Tag", "Total (H)", "Total (D)", width=max_width)
        )
    else:
        output.append("{:{width}} {:>10} {:>10} {:>10}".format("Tag", "Total (H)", "Total (D)", width=max_width))
        output.append(f"{'-' * max_width} {'----------'}")

    # Compose table row.
    grand_total = 0
    for tag in sorted(totals):
        seconds = int(totals[tag].total_seconds())
        formatted_hours = format_seconds_hours(seconds)
        formatted_days = format_seconds_days(seconds)
        grand_total += seconds
        output.append(f"{tag:{max_width}} {formatted_hours:10} {formatted_days:10}")

    if untagged is not None:
        seconds = int(untagged.total_seconds())
        formatted_hours = format_seconds_hours(seconds)
        formatted_days = format_seconds_days(seconds)
        grand_total += seconds
        output.append(f"{tag:{max_width}} {formatted_hours:10} {formatted_days:10}")

    # Compose total.
    if configuration["color"] == "on":
        output.append(
            "\x1b[4m{:{width}} \x1b[0m \x1b[4m{:>10}\x1b[4m{:>10}\x1b[0m".format("", "", "", width=max_width)
        )
    else:
        output.append(f"{'-' * max_width} {'----------'}")

    output.append("{:{width}} {:10} {:10}".format("Total", format_seconds_hours(grand_total), format_seconds_days(grand_total), width=max_width))
    output.append("")

    return output


if __name__ == "__main__":
    print("~" * 100)
    for line in calculate_totals(sys.stdin):
        print(line)