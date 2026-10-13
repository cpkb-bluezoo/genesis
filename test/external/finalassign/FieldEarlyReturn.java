// error: variable x might not have been initialized
class FieldEarlyReturn { final int x; FieldEarlyReturn(boolean c) { if (c) return; x = 1; } }
