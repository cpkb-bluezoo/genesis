package p;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Type::method naming a method the type inherits (from a superclass or
 * superinterface read from a class file).
 */
public class InheritedMethodRefTest {
	public static void main(String[] args) {
		Function<Hint, String> name = Hint::getName;
		if (!"a".equals(name.apply(new Hint("a")))) {
			throw new AssertionError("getName");
		}
		List<String> seen = new ArrayList<String>();
		Set<String> set = new TreeSet<String>();
		set.add("x");
		set.add("y");
		BiConsumer<Set<String>, Consumer<String>> each = Set::forEach;
		each.accept(set, seen::add);
		if (!seen.toString().equals("[x, y]")) {
			throw new AssertionError("forEach " + seen);
		}
		System.out.println("PASS");
	}
}
