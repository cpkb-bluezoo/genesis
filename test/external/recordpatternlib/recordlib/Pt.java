package recordlib;

import java.util.List;

/* Components are named differently from the variables that match them in the client */
public record Pt(int x, String label, List<String> tags) {}
