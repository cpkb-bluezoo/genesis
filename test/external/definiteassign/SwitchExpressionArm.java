class SwitchExpressionArm { int m(int i) { int x; int r = switch (i) { case 0 -> { x = 1; yield 1; } default -> 2; }; return x + r; } }
