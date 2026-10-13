// error: variable x might already have been assigned
class AssignedAfterBranch { int m(boolean c) { final int x; if (c) { x = 1; } x = 2; return x; } }
