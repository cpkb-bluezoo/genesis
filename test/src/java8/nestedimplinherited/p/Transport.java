package nestedimplinherited.p;

interface Transport {
    interface Listener {
        int port();
        void close();
    }
    Listener listen(int port);
}
