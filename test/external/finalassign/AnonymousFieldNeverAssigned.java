// error: variable x not initialized in the default constructor
class AnonymousFieldNeverAssigned { Object m() { return new Object() { final int x; }; } }
