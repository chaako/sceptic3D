# Copyright (C) 2006, 2007 Free Software Foundation, Inc.
#
# GNU tar is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3, or (at
# your option) any later version.
#
# GNU tar is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU tar; if not, write to the Free Software
# Foundation, Inc. 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

1{s,/\*,@comment ,
b
}
2,/.*\*\//{s,\*/,,;s/^/@comment/
b
}
/\/* END \*\//,$d
s/\([{}]\)/@\1/g
s,/\*,&@r{,
s,\*/,}&,
