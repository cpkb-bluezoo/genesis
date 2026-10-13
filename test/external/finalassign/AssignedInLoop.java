// error: variable x might be assigned in loop
class AssignedInLoop { int m(boolean c) { final int x; while (c) { x = 1; } return 0; } }
