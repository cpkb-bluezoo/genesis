// error: variable x might already have been assigned
class CompoundAfterAssign { int m() { final int x; x = 1; x += 1; return x; } }
