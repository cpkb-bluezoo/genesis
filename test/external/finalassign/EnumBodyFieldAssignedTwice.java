// error: variable y might already have been assigned
enum EnumBodyFieldAssignedTwice { A { final int y; { y = 1; y = 2; } }; }
