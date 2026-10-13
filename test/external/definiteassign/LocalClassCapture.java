class LocalClassCapture { int m() { int x; class L { int get() { return x; } } return new L().get(); } }
