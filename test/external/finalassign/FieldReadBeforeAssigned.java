// error: variable x might not have been initialized
class FieldReadBeforeAssigned { final int x; FieldReadBeforeAssigned() { int y = x; x = 1; } }
