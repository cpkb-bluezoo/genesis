// error: variable y might not have been initialized
enum EnumBodyFieldRead { A { final int y; { int z = y; y = 1; } }; }
