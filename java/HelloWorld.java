class HelloWorld {
    public native void displayHelloWorld();

    static {
        System.loadLibrary("Hello");
    }
    
    public static void main(String[] args) {
        new HelloWorld().displayHelloWorld();
    }
}
