/*
 * Regression test: "StringBuilder[] x = { create() }" - the generic call in the array initializer was not typed by
 * the array's element type, so the reified varargs array was an Object[] (ArrayStoreException).
 */
public class ArrayInitializerGenericCallTest {
	@SafeVarargs @SuppressWarnings("unchecked")
	static <T> T create(T... reified) { try { return (T) reified.getClass().getComponentType().getDeclaredConstructor().newInstance(); } catch (ReflectiveOperationException e) { throw new IllegalStateException(e); } }
	private final StringBuilder[] builders = { create() };
	static CharSequence[] seqs() { return new CharSequence[] { new StringBuilder("x") }; }
	public static void main(String[] a) { StringBuilder[] local = { create(), create() }; if (new ArrayInitializerGenericCallTest().builders.length != 1 || local.length != 2) {
			throw new RuntimeException("FAIL");
		}
		System.out.println("ArrayInitializerGenericCallTest passed!"); }
}
