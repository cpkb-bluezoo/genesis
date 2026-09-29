package genesislib;

/*
 * Compiled by genesis itself (see run-tests.sh) into its OWN directory,
 * separate compile batch from the client that references it - mirrors
 * gumdrop's own H2FrameHandler interface (a different module from
 * HttpProtocolHandler, which references its SETTINGS_* constants via
 * qualified case labels: "case H2FrameHandler.SETTINGS_...:").
 */
public interface FrameSettings {
    int HEADER_TABLE_SIZE = 0x1;
    int ENABLE_PUSH = 0x2;
    int MAX_CONCURRENT_STREAMS = 0x3;
    int INITIAL_WINDOW_SIZE = 0x4;
}
