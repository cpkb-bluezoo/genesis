// error: cannot assign a value to final variable x
class ReassignedInitialised { int m() { final int x = 1; x = 2; return x; } }
