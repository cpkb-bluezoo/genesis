import pushcovlib.BaseTests;
import pushcovlib.LeafTests;
import pushcovlib.Props;

/**
 * Mirrors Spring PushRegistryPropertiesConfigAdapterTests (testFixtures) ->
 * StepRegistryPropertiesConfigAdapterTests (bridges from classfile parent) ->
 * DatadogPropertiesConfigAdapterTests (covariant override). Calls
 * createProperties() through the generic base descriptor.
 */
public class PushCovariantOverrideBridgeFromClassfileTest {

    public static void main(String[] args) {
        BaseTests<Props, ?> tests = new LeafTests();
        if (!"NarrowProps".equals(tests.stepViaBase())) {
            throw new RuntimeException("expected NarrowProps got " + tests.stepViaBase());
        }
        System.out.println("PushCovariantOverrideBridgeFromClassfileTest passed!");
    }
}
