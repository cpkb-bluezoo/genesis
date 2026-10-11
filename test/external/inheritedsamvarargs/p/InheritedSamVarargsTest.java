package p;

/**
 * A lambda for a varargs parameter of an interface whose abstract method is inherited, with
 * type arguments, from another interface ("extends Function<Srv, Srv>"), of another file.
 */
public class InheritedSamVarargsTest {
	public static void main(String[] args) {
		SrvFactory factory = new SrvFactory();
		factory.addCustomizers((srv) -> srv.option("a"));
		factory.addCustomizers((srv) -> srv.option("b").option("c"), (srv) -> srv.option("d"));
		String log = factory.build().log();
		if (!"a;b;c;d;".equals(log)) {
			throw new AssertionError(log);
		}
		System.out.println("PASS");
	}
}
