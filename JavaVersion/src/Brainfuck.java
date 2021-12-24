package src;

import java.nio.charset.Charset;
import java.util.Scanner;

public class Brainfuck {

    private static class Interpreter {
        byte[] memory;
        int pointer;
        int ic;

        public Interpreter() {
            memory = new byte[30000];
            pointer = 0;
            ic = 0;
        }

        public byte getData() {
            return memory[pointer];
        }

        public void setData(byte b) {
            memory[pointer] = b;
        }

        public void incrementData() {
            memory[pointer] = (byte) (getData() + 1);
        }

        public void decrementData() {
            memory[pointer] = (byte) (getData() - 1);
        }

        public void incrementPointer() {
            pointer = (pointer + 1) % 30000;
        }

        public void decrementPointer() {
            pointer = (pointer - 1) % 30000;
        }

        public int getIc() {
            return ic;
        }

        public void incrementIc() {
            ic++;
        }

        public void decrementIc() {
            ic--;
        }
    }

    public static String execute(String program) {
        Interpreter i = new Brainfuck.Interpreter();
        Scanner readFromCLI = new Scanner(System.in);
        StringBuilder out = new StringBuilder();
        while (i.getIc() < program.length()) {
            switch (program.charAt(i.getIc())) {
            case '+':
                i.incrementData();
                break;
            case '-':
                i.decrementData();
                break;
            case '>':
                i.incrementPointer();
                break;
            case '<':
                i.decrementPointer();
                break;
            case '.':
                byte[] b = new byte[1];
                b[0] = i.getData();
                String s = new String(b, Charset.defaultCharset());
                out.append(s);
                break;
            case ',':
                i.setData(readFromCLI.nextByte());
                break;
            case '[':
                if (i.getData() == 0) {
                    int numOfBracket = 0;
                    boolean finished = false;
                    while (!finished) {
                        i.incrementIc();
                        switch (program.charAt(i.getIc())) {
                        case '[':
                            numOfBracket++;
                            break;
                        case ']':
                            if (numOfBracket == 0) {
                                finished = true;
                            } else {
                                numOfBracket--;
                            }
                            break;
                        }
                    }
                }
                break;
            case ']':
                if (i.getData() != 0) {
                    int numOfBracket = 0;
                    boolean finished = false;
                    while (!finished) {
                        i.decrementIc();
                        switch (program.charAt(i.getIc())) {
                        case ']':
                            numOfBracket++;
                            break;
                        case '[':
                            if (numOfBracket == 0) {
                                finished = true;
                            } else {
                                numOfBracket--;
                            }
                            break;
                        }
                    }
                }
                break;
            default:
                // nothing
            }
            i.incrementIc();
        }
        readFromCLI.close();
        return out.toString();
    }
}
