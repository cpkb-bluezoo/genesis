// error: variable x might already have been assigned
class FieldThisAssignedTwice { final int x; FieldThisAssignedTwice() { this.x = 1; this.x = 2; } }
