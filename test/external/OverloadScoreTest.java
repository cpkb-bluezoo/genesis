import java.util.HashMap;

import ovlpick.Picker;

/* invokeMethod(Class, String, Object...) beats invokeMethod(Object, String, Object...),
 * and arrays(T[][]) beats arrays(T[]) for Integer[][]. A type that is not a Delegate
 * stays on pick(Object). */
public class OverloadScoreTest implements Picker.Marker {
    @Override
    public String id() {
        return "m";
    }

    public static void main(String[] args) {
        if (!"class".equals(Picker.invokeMethod(String.class, "reset"))) {
            throw new RuntimeException("class literal");
        }
        Class<?> type = Void.class;
        if (!"class".equals(Picker.invokeMethod(type, "reset"))) {
            throw new RuntimeException("class variable");
        }
        if (!"class".equals(Picker.invokeMethod(String.class, "getThrottlePolicy", new HashMap<String, String>()))) {
            throw new RuntimeException("class and map");
        }
        Integer[][] grid = new Integer[][] { { 1, 2 }, { 3, 4 } };
        if (!"2d".equals(Picker.arrays(grid))) {
            throw new RuntimeException("grid " + Picker.arrays(grid));
        }
        Integer[] row = new Integer[] { 1 };
        if (!"1d".equals(Picker.arrays(row))) {
            throw new RuntimeException("row " + Picker.arrays(row));
        }
        if (!"object".equals(Picker.pick(new OverloadScoreTest()))) {
            throw new RuntimeException("marker");
        }
        Picker.Real real = new Picker.Real();
        if (!"delegate".equals(Picker.pick(real))) {
            throw new RuntimeException("delegate");
        }
        System.out.println("PASS");
    }
}
