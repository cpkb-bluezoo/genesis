import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class FlatMapLambdaResultInferenceTest {
	interface Op {
		String name();
	}

	interface Endpoint<O extends Op> {
		Collection<O> operations();
	}

	interface WebOp extends Op {
	}

	interface WebEndpoint extends Endpoint<WebOp> {
	}

	static WebOp op(final String name) {
		return new WebOp() {
			public String name() {
				return name;
			}
		};
	}

	static WebEndpoint endpoint(final WebOp... ops) {
		return new WebEndpoint() {
			public Collection<WebOp> operations() {
				return Arrays.asList(ops);
			}
		};
	}

	private Stream<String> resources(String mapping, WebOp operation) {
		return Stream.of(mapping + ":" + operation.name(), mapping + "/" + operation.name());
	}

	List<String> collect(String mapping, Collection<WebEndpoint> endpoints) {
		return endpoints.stream()
			.flatMap((endpoint) -> endpoint.operations().stream())
			.flatMap((operation) -> resources(mapping, operation))
			.collect(Collectors.toList());
	}

	public static void main(String[] args) {
		List<WebEndpoint> endpoints = new ArrayList<WebEndpoint>();
		endpoints.add(endpoint(op("a"), op("b")));
		endpoints.add(endpoint(op("c")));
		List<String> all = new FlatMapLambdaResultInferenceTest().collect("m", endpoints);
		if (!"[m:a, m/a, m:b, m/b, m:c, m/c]".equals(all.toString())) {
			throw new AssertionError(all);
		}
		System.out.println("PASS");
	}
}
