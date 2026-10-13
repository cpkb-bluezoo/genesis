class FinalLocalNotConstant { int m(boolean c) { final boolean on = c; int x; if (on) { x = 1; } return x; } }
