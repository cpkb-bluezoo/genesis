import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

public record NestedRecordMethodRefCompactConstructorTest(String x) {
	static class Mapper {
		static Mapper get() { return new Mapper(); }
		<T> Source<T> from(Supplier<T> supplier) { return new Source<T>(supplier.get()); }
	}

	static class Source<T> {
		final T value;
		Source(T value) { this.value = value; }
		<R> Source<R> as(Function<T, R> adapter) { return new Source<R>(value == null ? null : adapter.apply(value)); }
		void to(Consumer<T> consumer) { if (value != null) { consumer.accept(value); } }
	}

	public record Lb(Pick pickfirst, Grpc grpc) {
		public Lb {
			if (pickfirst == null && grpc == null) {
				throw new IllegalArgumentException();
			}
		}

		Map<String, Object> config() {
			LinkedHashMap<String, Object> out = new LinkedHashMap<>();
			Mapper map = Mapper.get();
			map.from(this::pickfirst).as(Pick::config).to((c) -> out.put("pick", c));
			map.from(this::grpc).as(Grpc::config).to((c) -> out.put("grpc", c));
			return out;
		}

		public record Pick(Boolean shuffle) {
			Map<String, Object> config() { return new LinkedHashMap<>(); }
		}

		public record Grpc(List<Lb> child, String serviceName) {
			public Grpc {
				child.forEach(this::check);
			}

			private void check(Lb lb) { }

			Map<String, Object> config() { return new LinkedHashMap<>(); }
		}
	}

	public static void main(String[] a) {
		Lb lb = new Lb(new Lb.Pick(true), null);
		if (lb.config().size() != 1 || !new Lb.Grpc(List.of(lb), "n").child().get(0).equals(lb)) {
			throw new AssertionError("config");
		}
		System.out.println("PASS");
	}
}
