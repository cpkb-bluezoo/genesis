/**
 * Test labeled statements with break and continue.
 */
public class LabeledStatements {
    
    /**
     * Labeled break from nested loop.
     * Returns the first pair (i,j) where i*j == n, encoded as i*100 + j.
     * Returns -1 if no such pair found.
     */
    public static int findFactorPair(int n) {
        outer:
        for (int i = 2; i < n; i = i + 1) {
            for (int j = 2; j < n; j = j + 1) {
                if (i * j == n) {
                    return i * 100 + j;  /* Return encoded pair */
                }
                if (i * j > n) {
                    break outer;  /* No point continuing - values too big */
                }
            }
        }
        return -1;
    }
    
    /**
     * Labeled continue to skip entire outer iteration.
     * Count pairs (i,j) where i < j and both divide n evenly.
     */
    public static int countDivisorPairs(int n) {
        int count = 0;
        outer:
        for (int i = 1; i <= n; i = i + 1) {
            if (n % i != 0) {
                continue outer;  /* i doesn't divide n, skip to next i */
            }
            for (int j = i + 1; j <= n; j = j + 1) {
                if (n % j == 0) {
                    count = count + 1;
                }
            }
        }
        return count;
    }
    
    /**
     * Multiple levels of nesting with labeled break.
     * Find first triple (i,j,k) where i+j+k == n and i < j < k.
     * Returns encoded as i*10000 + j*100 + k.
     */
    public static int findTriple(int n) {
        search:
        for (int i = 1; i < n; i = i + 1) {
            for (int j = i + 1; j < n; j = j + 1) {
                for (int k = j + 1; k < n; k = k + 1) {
                    if (i + j + k == n) {
                        return i * 10000 + j * 100 + k;
                    }
                    if (i + j + k > n) {
                        break;  /* Inner break - just exit k loop */
                    }
                }
                if (i + j >= n) {
                    continue search;  /* Skip rest of j iterations */
                }
            }
        }
        return -1;
    }
    
    /**
     * Labeled while loop with break.
     */
    public static int searchMatrix(int target) {
        int row = 0;
        rowLoop:
        while (row < 3) {
            int col = 0;
            while (col < 3) {
                int val = row * 3 + col;  /* Simulated matrix value */
                if (val == target) {
                    return row * 10 + col;  /* Return encoded position */
                }
                col = col + 1;
            }
            row = row + 1;
        }
        return -1;
    }
    
    /**
     * Labeled do-while loop.
     */
    public static int labeledDoWhile(int limit) {
        int sum = 0;
        int i = 0;
        outer:
        do {
            int j = 0;
            do {
                if (i + j > limit) {
                    break outer;
                }
                sum = sum + 1;
                j = j + 1;
            } while (j < 3);
            i = i + 1;
        } while (i < 10);
        return sum;
    }
    
    /**
     * Labeled block (non-loop) with break.
     */
    public static int labeledBlock(int n) {
        int result = 0;
        block:
        {
            if (n < 0) {
                break block;
            }
            result = n * 2;
            if (result > 100) {
                break block;
            }
            result = result + 10;
        }
        return result;
    }
}

