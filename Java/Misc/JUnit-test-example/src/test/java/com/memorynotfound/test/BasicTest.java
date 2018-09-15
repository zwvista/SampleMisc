package com.memorynotfound.test;

import org.junit.Test;
import static org.junit.Assert.*;

public class BasicTest {

    @Test
    public void exampleUnitTest(){
        assertNull("This should evaluate to null", null);
        assertNotNull("Newly created object should not be null", new Object());
        Object sameObject = new Object();
        assertSame("This should be the same object", sameObject, sameObject);
        assertEquals("1 should be equal to 1", 1, 1);
        assertNotEquals("Objects should not be equal", new Object(), new Object());
        assertTrue("This should evaluate to true", true);
        assertFalse("This should evaluate to false", false);
        assertArrayEquals("Arrays should be equal", new int[]{1, 2}, new int[]{1, 2});
    }

    @Test
    public void assertNullTest(){
        assertNull("This should evaluate to null", null);
    }

    @Test
    public void assertNotNullTest(){
        assertNotNull("Newly created object should not be null", new Object());
    }

    @Test
    public void assertSameTest(){
        Object sameObject = new Object();
        assertSame("This should be the same object", sameObject, sameObject);
    }

    @Test
    public void assertEqualTest(){
        assertEquals("1 should be equal to 1", 1, 1);
    }

    @Test
    public void assertNotEqualTest(){
        assertNotEquals("Objects should not be equal", new Object(), new Object());
    }

    @Test
    public void assertTrueTest(){
        assertTrue("This should evaluate to true", true);
    }

    @Test
    public void assertFalseTest(){
        assertFalse("This should evaluate to false", false);
    }

    @Test
    public void assertArrayEqualTest(){
        assertArrayEquals("Arrays should be equal", new int[]{1, 2}, new int[]{1, 2});
    }

    @Test(expected = ArithmeticException.class)
    public void exceptionTest(){
        float temp = 5 / 0;
    }

    @Test
    public void exceptionFailTest(){
        try {
            float temp = 5 / 0;
            fail("Exception should have been thrown");
        } catch (ArithmeticException e){
            // should fail
        }
    }
}
