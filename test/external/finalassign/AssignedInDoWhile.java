// error: variable x might be assigned in loop
class AssignedInDoWhile { int m(boolean c) { final int x; do { x = 1; } while (c); return x; } }
