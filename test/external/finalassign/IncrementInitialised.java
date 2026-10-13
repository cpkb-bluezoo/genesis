// error: cannot assign a value to final variable x
class IncrementInitialised { int m() { final int x = 1; x++; return x; } }
