class HelloWorld {
    public native void displayHelloWorld();
    public native void doSomethingMoreInteresting(CallBack s, String label);

    static {
        System.loadLibrary("Hello");
    }
    
    public static void main(String[] args) {
        HelloWorld hw = new HelloWorld();

        CBTester cbt = new CBTester ();
        hw.doSomethingMoreInteresting (cbt, "Testing some more.");
        System.out.print ("This had better be two: ");
        System.out.println (cbt.get_calls ());
    }
}
