import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Set;

public class InnerClassExtendsOuterTypeVariableTest<K> {
	class Keys extends ArrayList<K> {
	}

	static class Holder<Z> {
		class Items extends ArrayList<Z> {
		}
	}

	static class Registry<K> extends LinkedHashMap<K, String> {
		@Override
		public Set<K> keySet() {
			return new KeySet(super.keySet());
		}

		private class KeySet extends LinkedHashSet<K> {
			KeySet(Set<K> keySet) {
				super(keySet);
			}

			@Override
			public String toString() {
				return "keys" + super.toString();
			}
		}
	}

	public static void main(String[] args) {
		InnerClassExtendsOuterTypeVariableTest<String> outer = new InnerClassExtendsOuterTypeVariableTest<String>();
		ArrayList<String> keys = outer.new Keys();
		keys.add("a");
		String first = keys.get(0).toUpperCase();
		Holder<Integer> holder = new Holder<Integer>();
		ArrayList<Integer> items = holder.new Items();
		items.add(41);
		int next = items.get(0) + 1;
		Registry<String> registry = new Registry<String>();
		registry.put("x", "y");
		String key = registry.keySet().iterator().next().toUpperCase();
		String shown = registry.keySet().toString();
		if (!"A".equals(first) || next != 42 || !"X".equals(key) || !"keys[x]".equals(shown)) {
			throw new AssertionError(first + next + key + shown);
		}
		System.out.println("PASS");
	}
}
