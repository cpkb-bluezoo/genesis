// error: variable x might not have been initialized
class FieldReadInLambda { final int x; FieldReadInLambda() { Runnable r = () -> System.out.println(x); x = 1; } }
