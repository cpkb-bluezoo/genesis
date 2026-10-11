/*
 * Regression test: "CR::new" passed where a Function<RunnerConfiguration, CR>
 * is expected picked a constructor taking a Supplier (one of several
 * constructors of the same arity) and failed with BootstrapMethodError when
 * the call site was linked. The constructor has to be chosen by the
 * functional interface's parameter types, with a varargs sibling around.
 */
import java.util.function.*;
abstract class Base2<SELF extends Base2<SELF, C>, C> {
	static final class RunnerConfiguration<C> { }
	final Function<RunnerConfiguration<C>, SELF> factory;
	final Supplier<C> supplier;
	Base2(Function<RunnerConfiguration<C>, SELF> f, Supplier<C> s) { factory = f; supplier = s; }
	Base2(RunnerConfiguration<C> c, Function<RunnerConfiguration<C>, SELF> f) { factory = f; supplier = null; }
	SELF make() { return factory.apply(new RunnerConfiguration<C>()); }
}
public final class ConstructorRefOverloadByParameterTypeTest extends Base2<ConstructorRefOverloadByParameterTypeTest, String> {
	public ConstructorRefOverloadByParameterTypeTest(Supplier<String> s) { super(ConstructorRefOverloadByParameterTypeTest::new, s); }
	public ConstructorRefOverloadByParameterTypeTest(Supplier<String> s, Class<?>... more) { super(ConstructorRefOverloadByParameterTypeTest::new, s); }
	private ConstructorRefOverloadByParameterTypeTest(RunnerConfiguration<String> c) { super(c, ConstructorRefOverloadByParameterTypeTest::new); }
	public static void main(String[] a) {
		if (new ConstructorRefOverloadByParameterTypeTest(() -> "x").make().supplier != null) {
			throw new RuntimeException("FAIL: copy used the Supplier constructor");
		}
		System.out.println("ConstructorRefOverloadByParameterTypeTest passed!");
	}
}
