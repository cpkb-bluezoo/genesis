// error: variable a might not have been initialized
class FieldReadInInitializer { final int a; int b = a + 1; FieldReadInInitializer() { a = 1; } }
