import wcres.Picker;
import static wcres.Picker.pick;

/*
 * Regression test: the result of a call on a receiver with a wildcard type argument
 * ("holder.get()" for "Holder<?>") was a wildcard to overload resolution, which fits any
 * parameter: an overload of a class file declared before the generic one was taken for it
 * (AssertJ's assertThat(AssertDelegateTarget) for assertThat(propertySource.getProperty(..)):
 * VerifyError). An Object result fits only the generic overload.
 */
public class WildcardResultOverloadTest {

    static String run(Picker.Holder<?> holder) {
        return pick(holder.get());
    }

    public static void main(String[] args) {
        String actual = run(new Picker.Holder<Object>(new Object() {
            @Override
            public String toString() {
                return "a";
            }
        }));
        if (!"generic a".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
