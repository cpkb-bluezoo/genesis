// error: variable x might be assigned in loop
class AssignedInForLoop { int m() { final int x; for (int i = 0; i < 2; i++) { x = i; } return 0; } }
