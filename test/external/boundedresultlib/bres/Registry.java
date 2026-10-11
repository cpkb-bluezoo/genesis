package bres;

public interface Registry {

    interface Appender {
        Layout<? extends java.io.Serializable> getLayout();
    }

    interface Layout<S extends java.io.Serializable> {
        String pattern();
    }

    <T extends Appender> T getAppender(String name);
}
