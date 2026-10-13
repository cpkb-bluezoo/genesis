// error: variable x might not have been initialized
class FieldOneConstructorMissing { final int x; FieldOneConstructorMissing() { x = 1; } FieldOneConstructorMissing(int y) { } }
