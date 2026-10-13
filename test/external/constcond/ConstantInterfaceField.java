class ConstantInterfaceField { interface Flags { boolean ON = true; } int m() { int x; if (Flags.ON) { x = 1; } return x; } }
