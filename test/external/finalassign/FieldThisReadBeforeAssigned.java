// error: variable x might not have been initialized
class FieldThisReadBeforeAssigned { final int x; FieldThisReadBeforeAssigned() { int y = this.x; x = y; } }
