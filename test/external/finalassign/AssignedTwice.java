// error: variable x might already have been assigned
class AssignedTwice { int m() { final int x; x = 1; x = 2; return x; } }
