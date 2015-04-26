package util;

public class Indenter {

    private static Indenter instance;
    
    private int n;
    
    private Indenter() {
        this.n = 0;
    }
    
    public static Indenter getInstance() {
        if(instance == null) {
            instance = new Indenter();
        }
        
        return instance;
    }
    
    public String indent(int i) {
        n = n + i;
        return indent();
    }
    
    public String indent() {
        StringBuffer sb = new StringBuffer("");
        for(int i = 0; i < n; i++) {
            sb.append(" ");
        }
        return sb.toString();
    }
    
    public void reset() {
        n = 0;
    }
    
}
