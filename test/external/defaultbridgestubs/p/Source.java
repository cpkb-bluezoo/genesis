package p;
public sealed interface Source permits Iter { Handle prefix(String p); static Source adapt(Handle h) { return null; } }
