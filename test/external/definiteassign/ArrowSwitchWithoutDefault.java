class ArrowSwitchWithoutDefault { int m(int i) { int x; switch (i) { case 0 -> x = 1; case 1 -> x = 2; } return x; } }
