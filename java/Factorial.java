public class Factorial {
    public static void main (String [] args) {
        int input = 0;
        try {
            input = Integer.parseInt (args[0]);
            int result = factorial (input);
            System.out.println (result);
        } catch (java.lang.NumberFormatException e) {
            System.err.print (args[0]);
            System.err.println (" doesn't look like an integer.");
        } catch (java.lang.ArrayIndexOutOfBoundsException e) {
            System.err.println ("Usage: [me] 23");
        } finally {
            System.err.println ("OK, we're outta here.");
        }
    }

    public static int factorial (int x) {
        if (x < 0)
            return 0;
        int fact = 1;
        while (x > 1) {
            fact = fact * x;
            x = x - 1;
        }
        return fact;
    }
}
