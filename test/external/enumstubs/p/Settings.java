package p;
public class Settings {
	public enum Compression { NONE, LZ4 }
	public Compression getCompression() { return Compression.LZ4; }
}
