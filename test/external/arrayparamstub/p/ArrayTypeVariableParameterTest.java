package p;

/* A static generic method of another file of the batch, with "T[]" among its parameters, is
 * called with an array: the stub's parameter is an array of the method's variable, not an int. */
public class ArrayTypeVariableParameterTest {
    static class Name {
        boolean ancestorOf(Name other) {
            return other == this;
        }
    }

    private final Name[] filtered = new Name[] { new Name() };

    State go(Name name) {
        return State.search(this.filtered, 0, 1, name::ancestorOf);
    }

    public static void main(String[] args) {
        ArrayTypeVariableParameterTest test = new ArrayTypeVariableParameterTest();
        if (test.go(test.filtered[0]) != State.PRESENT) {
            throw new AssertionError("not found");
        }
        System.out.println("PASS");
    }
}
