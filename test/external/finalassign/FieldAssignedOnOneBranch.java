// error: variable x might not have been initialized
class FieldAssignedOnOneBranch { final int x; FieldAssignedOnOneBranch(boolean c) { if (c) { x = 1; } } }
