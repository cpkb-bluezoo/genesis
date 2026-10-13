// error: variable x might not have been initialized
class FieldParameterShadows { final int x; FieldParameterShadows(int x) { x = 5; } }
