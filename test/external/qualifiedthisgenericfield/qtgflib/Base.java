package qtgflib;
public abstract class Base<SELF extends Base<SELF, A>, A> {
	protected final A actual;
	protected Base(A actual) { this.actual = actual; }
}
