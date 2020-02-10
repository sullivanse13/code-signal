package code_signal;

import org.junit.Test;

import java.util.BitSet;

import static org.junit.Assert.*;

public class CornerOf0sAnd1sTest {

    int differentRightmostBit(int n, int m) {
        return Integer.lowestOneBit(n ^ m);
    }

    @Test
    public void testDifferentRightmostBit() {
        assertEquals(2, differentRightmostBit(11,13));
        //assertEquals(2, secondRightmostZeroBit(4));
    }


    int secondRightmostZeroBit(int n) {

        System.out.println(Integer.toBinaryString(n));
        System.out.println(Integer.lowestOneBit(n));

        int power2OfLowestOne = 1 << (Integer.lowestOneBit(n)-1);

        System.out.println(Integer.toBinaryString(power2OfLowestOne));
        System.out.println(Integer.toBinaryString(power2OfLowestOne ^ n));
        System.out.println(Integer.toBinaryString(0 ^ n));

        int zeroCount = 0;
        int bitPlace = -1;
        int x = n;
        while(x > 0 && zeroCount < 2) {
            bitPlace++;
            if((x & 1) == 0) {
                zeroCount++;
            }
            x = x >> 1;
        }

        return (int) Math.pow(2, bitPlace);
    }

    @Test
    public void testSecondRightmostZeroBit() {
        assertEquals(8, secondRightmostZeroBit(37));
        //assertEquals(2, secondRightmostZeroBit(4));
    }



    int killKthBit(int n, int k) {
        return n & ~(1 << (k-1));
    }

    @Test
    public void testKillKthBit() {

        assertEquals(0, killKthBit(1,1));
        assertEquals(1, killKthBit(3,2));
        assertEquals(33, killKthBit(37,3));
        assertEquals(37, killKthBit(37,4));
        assertEquals(2147450879, killKthBit(2147483647,16));
    }

}