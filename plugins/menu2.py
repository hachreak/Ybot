# -*- coding: utf-8 -*-
#
# Simple Ybot echo plugin
#
# Usage:
#
# Ybot echo some text
#
import sys
from os import path
sys.path.append(path.dirname(path.dirname(path.abspath(__file__))))

from R1D2.r1 import serialize_menu
from R1D2.r1.filter import filter_menu, make_filter
from R1D2.r1.menu import get_menu
from R1D2.r1.types import Restaurant

filter_ = make_filter(['today'])
menu = filter_menu(get_menu(Restaurant.r2), filter_)
smenu = serialize_menu(menu)

print(unicode("""
:fork_and_knife: Hey Y'@/all, it's lunch time! :clock12:\n
{0}
""".format(u"\n".join([
    unicode("{0} ({1} {2})".format(plate['name'], plate['price'], plate['currency']))
    for plate in smenu
]))))
