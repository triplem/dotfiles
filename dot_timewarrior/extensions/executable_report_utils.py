from datetime import datetime, timedelta
from dateutil import tz
from collections import defaultdict
import numpy as np
import holidays
import calendar

DATE_FORMAT = "%Y%m%dT%H%M%SZ"
from_zone = tz.tzutc()
to_zone = tz.tzlocal()

def get_date_from_config(config, name, default):
    date = datetime.strptime(config.get_value(name, default), DATE_FORMAT)
    date = date.replace(tzinfo=from_zone)
    return date.astimezone(to_zone)

def get_start(config):
    return get_date_from_config(config, "temp.report.start", datetime(1970,1,1))

def get_end(config):
    return get_date_from_config(config, "temp.report.end", datetime.now().strftime(DATE_FORMAT))

def get_business_days(start, end):
    np_start = np.datetime64(start.strftime("%Y-%m-%d"))
    np_end = np.datetime64(end.strftime("%Y-%m-%d"))
    return np.busday_count(
        np_start, np_end, 
        holidays = list(map(str, holidays.country_holidays("DE", subdiv="HE", years=[start.year, end.year]).keys())))

def get_business_days_by_year_month(year, month):
    _, ld = calendar.monthrange(year, month)
    start = datetime(year, month, 1)
    end = datetime(year, month, ld) + timedelta(days=1)
    return get_business_days(start, end)

nested_dict = lambda: defaultdict(nested_dict)
interval_dict_date_tag = nested_dict()
