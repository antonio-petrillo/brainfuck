package src;

public class Driver {
    public static void main(String[] args) {
        String out = Brainfuck.execute(
                "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.");
        System.out.println("\n" + out);
    }
}
