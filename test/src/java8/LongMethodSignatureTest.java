public class LongMethodSignatureTest {
	/* Every parameter type is written out in full: the key of the method in its class's table is
	 * far longer than any fixed buffer a short signature would fit. */
	static String describe(java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p0,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p1,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p2,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p3,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p4,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p5,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p6,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p7,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p8,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p9,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p10,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p11,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p12,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p13,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p14,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p15,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p16,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p17,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p18,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p19,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p20,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p21,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p22,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p23,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p24,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p25,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p26,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p27,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p28,
			java.util.concurrent.atomic.AtomicReferenceFieldUpdater<Object, String> p29) {
		return "described";
	}

	static String describe(int plain) {
		return "plain";
	}

	public static void main(String[] args) {
		String longer = describe(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null);
		String shorter = describe(1);
		if (!"described".equals(longer) || !"plain".equals(shorter)) {
			throw new AssertionError(longer + shorter);
		}
		System.out.println("PASS");
	}
}
