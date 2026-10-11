package p;

public final class Hint extends NamedHint implements Comparable<Hint> {
	public Hint(String name) {
		super(name);
	}

	public int compareTo(Hint other) {
		return getName().compareTo(other.getName());
	}
}
