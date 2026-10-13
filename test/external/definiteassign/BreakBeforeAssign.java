class BreakBeforeAssign { int m(boolean c) { int x; while (true) { if (c) { break; } x = 1; } return x; } }
