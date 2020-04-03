import numpy as np
from datetime import datetime

def poly(x, p):
    """
    Thanks to https://stackoverflow.com/questions/41317127/python-equivalent-to-r-poly-function
    """
    x = np.array(x)
    X = np.transpose(np.vstack((x**k for k in range(p+1))))
    return np.linalg.qr(X)[0][:,1:]

def dt_to_dec(dt):
    """Convert a datetime to decimal year.
    Thanks to https://stackoverflow.com/questions/29851357/python-datetime-to-decimal-year-one-day-off-where-is-the-bug
    """
    year_start = datetime(dt.year, 1, 1)
    year_end = year_start.replace(year=dt.year+1)
    return dt.year + ((dt - year_start).total_seconds() /  # seconds so far
        float((year_end - year_start).total_seconds()))  # seconds in year