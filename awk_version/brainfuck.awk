#!/usr/bin/awk -f

BEGIN{
    ic=1 # split insert in the first pos the empty string ""
    pointer=0
    memory[0] = 0
    loaded_program[0] = ""
    readed_byte = ""
}

{
    split($0, loaded_program, "")
    while(ic <= length ($0)){
        switch(loaded_program[ic]){
            case "+":
                memory[pointer]++
                break;
            case "-":
                memory[pointer]--
                break;
            case ".":
                printf("%c", memory[pointer])
                break;
            case ",":
                getline input < "/dev/tty"
                readed_byte = sprintf("%c", substr(input, 1 ,1))
                memory[pointer] = readed_byte
                break;
            case ">":
                pointer = (pointer + 1) % 30000
                break;
            case "<":
                pointer = (pointer - 1) % 30000
                break;
            case "[":
                if(memory[pointer] == 0){
                    num_of_bracket = 0;
                    finished = 0;
                    while(!finished){
                        ic++;
                        if(loaded_program[ic] == "]"){
                           if(num_of_bracket == 0){
                               finished = 1;
                           }else{
                              num_of_bracket--;
                           }
                        }else if(loaded_program[ic] == "["){
                           num_of_bracket++;
                        }
                    }
                }
                break;
            case "]":
                if(memory[pointer] != 0){
                    num_of_bracket = 0;
                    finished = 0;
                    while(!finished){
                        ic--;
                        if(loaded_program[ic] == "["){
                           if(num_of_bracket == 0){
                               finished = 1;
                           }else{
                              num_of_bracket--;
                           }
                        }else if(loaded_program[ic] == "]"){
                           num_of_bracket++;
                        }
                    }
                }
                break;
        }
        ic++;
    }

}
