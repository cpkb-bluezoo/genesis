import java.util.function.BiFunction;
import java.util.function.Function;

public class LambdaForInheritedGenericSamTest {
	static class Server {
		final StringBuilder log = new StringBuilder();

		Server option(String name) {
			log.append(name).append(';');
			return this;
		}
	}

	@FunctionalInterface
	interface Customizer extends Function<Server, Server> {
	}

	@FunctionalInterface
	interface Joiner extends BiFunction<String, Integer, String> {
	}

	@FunctionalInterface
	interface Twice extends Customizer {
	}

	static Server run(Server server, Customizer... customizers) {
		for (Customizer customizer : customizers) {
			server = customizer.apply(server);
		}
		return server;
	}

	public static void main(String[] args) {
		Server server = run(new Server(), (s) -> s.option("a"), (s) -> s.option("b").option("c"));
		Customizer direct = (s) -> s.option("d");
		Twice twice = (s) -> s.option("e");
		Joiner joiner = (text, count) -> text + count.intValue();
		if (!"a;b;c;".equals(server.log.toString()) || !"d;".equals(direct.apply(new Server()).log.toString())
				|| !"e;".equals(twice.apply(new Server()).log.toString()) || !"x3".equals(joiner.apply("x", 3))) {
			throw new AssertionError(server.log);
		}
		System.out.println("PASS");
	}
}
