// error: variable x might already have been assigned
class AssignedInTryAndCatch { int m(String s) { final int x; try { x = Integer.parseInt(s); } catch (RuntimeException e) { x = 2; } return x; } }
