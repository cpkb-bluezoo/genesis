// error: variable x might already have been assigned
class FieldAssignedTwice { final int x; FieldAssignedTwice() { x = 1; x = 2; } }
