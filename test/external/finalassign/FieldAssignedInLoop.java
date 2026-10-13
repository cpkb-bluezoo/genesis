// error: variable x might be assigned in loop
class FieldAssignedInLoop { final int x; FieldAssignedInLoop(int n) { for (int i = 0; i < n; i++) { this.x = i; } } }
