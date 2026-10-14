package nestedenum;

public class Labels {
    public enum Nested {
        OK, FAIL
    }

    public static class Holder {
        public static final int LIMIT = 9;
    }

    public static final class Definition {
        public enum ValidValues {
            FILE_NAME;

            public String getValue() {
                return name();
            }
        }
    }
}
