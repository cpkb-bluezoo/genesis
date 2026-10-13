// error: variable X might already have been assigned
class StaticFieldAssignedTwice { static final int X; static { X = 1; X = 2; } }
