class QualifiedConstant { static class Config { static final boolean ON = true; } int m() { int x; if (Config.ON) { x = 1; } return x; } }
