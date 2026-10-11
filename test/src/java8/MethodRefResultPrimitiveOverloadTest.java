import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class MethodRefResultPrimitiveOverloadTest {
	interface MethodFilter {
		boolean matches(Method method);
	}

	interface MetadataLookup<T> {
		T inspect(Method method);
	}

	static Set<Method> selectMethods(Class<?> type, MethodFilter filter) {
		Set<Method> found = new LinkedHashSet<Method>();
		for (Method method : type.getDeclaredMethods()) {
			if (filter.matches(method)) {
				found.add(method);
			}
		}
		return found;
	}

	static <T> Map<Method, T> selectMethods(Class<?> type, MetadataLookup<T> lookup) {
		Map<Method, T> found = new HashMap<Method, T>();
		for (Method method : type.getDeclaredMethods()) {
			T value = lookup.inspect(method);
			if (value != null) {
				found.put(method, value);
			}
		}
		return found;
	}

	static class Target {
		public void tagged() {
		}

		public void plain() {
		}
	}

	boolean isTagged(Method method) {
		return method.getName().equals("tagged");
	}

	String label(Method method) {
		return method.getName().equals("plain") ? "label" : null;
	}

	public static void main(String[] args) {
		MethodRefResultPrimitiveOverloadTest test = new MethodRefResultPrimitiveOverloadTest();
		Set<Method> methods = selectMethods(Target.class, test::isTagged);
		if (methods.size() != 1) {
			throw new AssertionError("filter " + methods);
		}
		Map<Method, String> labels = selectMethods(Target.class, test::label);
		if (labels.size() != 1 || !"label".equals(labels.values().iterator().next())) {
			throw new AssertionError("lookup " + labels);
		}
		Set<Method> viaThis = test.viaThis();
		if (viaThis.size() != 1) {
			throw new AssertionError("this " + viaThis);
		}
		System.out.println("PASS");
	}

	Set<Method> viaThis() {
		return selectMethods(Target.class, this::isTagged);
	}
}
