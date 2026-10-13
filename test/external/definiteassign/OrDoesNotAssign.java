class OrDoesNotAssign { int m(boolean a) { int x; if (a || (x = 1) > 0) { return x; } return 0; } }
