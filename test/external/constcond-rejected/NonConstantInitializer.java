class NonConstantInitializer { static final boolean ON = Boolean.parseBoolean("true"); int m() { int x; if (ON) { x = 1; } return x; } }
