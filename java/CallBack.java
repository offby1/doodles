class CallBack {
    public boolean DoIt (int nDone, int nTotal, String label) {
        System.out.print (label);
        System.out.print (": ");
        System.out.print (nDone);
        System.out.print (" done out of ");
        System.out.print (nTotal);
        System.out.println (" total.");
        return (nDone < nTotal);
    }
}

class CBTester extends CallBack {
    private int calls = 0;

    public int get_calls () {return calls;}

    public boolean DoIt (int nDone, int nTotal, String label) {
        calls ++;
        return super.DoIt (nDone, nTotal, label);
    }
}