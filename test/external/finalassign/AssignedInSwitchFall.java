// error: variable x might already have been assigned
class AssignedInSwitchFall { int m(int i) { final int x; switch (i) { case 0: x = 1; case 1: x = 2; break; default: x = 3; } return x; } }
