// error: variable x might already have been assigned
class FieldAssignedInInitializerAndConstructor { final int x; { x = 1; } FieldAssignedInInitializerAndConstructor() { x = 2; } }
