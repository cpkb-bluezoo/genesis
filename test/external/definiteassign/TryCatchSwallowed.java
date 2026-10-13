class TryCatchSwallowed { int m(String s) { int x; try { x = Integer.parseInt(s); } catch (RuntimeException e) { } return x; } }
