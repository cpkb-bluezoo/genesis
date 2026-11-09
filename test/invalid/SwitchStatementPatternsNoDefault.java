public class SwitchStatementPatternsNoDefault {
    static void f(Object o) {
        switch (o) {
            case String s -> System.out.println(s);
        }
    }
}
