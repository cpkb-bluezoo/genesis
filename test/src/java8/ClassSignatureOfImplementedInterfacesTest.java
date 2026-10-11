import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Iterator;
import java.util.List;
import java.util.function.Function;

public class ClassSignatureOfImplementedInterfacesTest {
	static class Mapping {
	}

	static class Mappings implements Iterable<Mapping>, Comparable<Mappings> {
		public Iterator<Mapping> iterator() {
			return null;
		}

		public int compareTo(Mappings other) {
			return 0;
		}
	}

	static class Base {
	}

	static class Generic<T> extends Base implements Function<T, List<String>>, Runnable {
		public List<String> apply(T t) {
			return null;
		}

		public void run() {
		}
	}

	interface Both extends Function<String, Integer>, Comparable<String>, Runnable {
	}

	static String show(Type type) {
		if (type instanceof ParameterizedType) {
			ParameterizedType p = (ParameterizedType) type;
			StringBuilder sb = new StringBuilder(((Class<?>) p.getRawType()).getSimpleName()).append('<');
			for (Type arg : p.getActualTypeArguments()) {
				sb.append(show(arg)).append(',');
			}
			return sb.append('>').toString();
		}
		return type instanceof Class ? ((Class<?>) type).getSimpleName() : type.toString();
	}

	static String interfaces(Class<?> type) {
		StringBuilder sb = new StringBuilder();
		for (Type t : type.getGenericInterfaces()) {
			sb.append(show(t)).append(' ');
		}
		return sb.toString().trim();
	}

	public static void main(String[] args) {
		String mappings = interfaces(Mappings.class);
		String generic = interfaces(Generic.class);
		String both = interfaces(Both.class);
		if (!"Iterable<Mapping,> Comparable<Mappings,>".equals(mappings)
				|| !"Function<T,List<String,>,> Runnable".equals(generic)
				|| !"Function<String,Integer,> Comparable<String,> Runnable".equals(both)
				|| Generic.class.getGenericSuperclass() != Base.class) {
			throw new AssertionError(mappings + " | " + generic + " | " + both + " | " + Generic.class.getGenericSuperclass());
		}
		System.out.println("PASS");
	}
}
