package itersrc;

import java.util.Collections;
import java.util.Iterator;

public class MapSrc implements IterSrc {

    @Override
    public Iterator<String> iterator() {
        return Collections.<String>emptyList().iterator();
    }
}
