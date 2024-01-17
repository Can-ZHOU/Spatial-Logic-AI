import java.io.*;
import java.util.*;


public class Reflexive{

   public static void main (String[] args) throws Exception {

         String filename = args[0];
         BufferedReader br0 = new BufferedReader(new FileReader(filename));
         int numberOfLines = 0;
         String nline = null;
         while (true){
           nline = br0.readLine();
           if (nline == null) break;
           else numberOfLines++;
         }
         br0.close();

         BufferedReader br1 = new BufferedReader(new FileReader(filename));
         String[] text1 = new String[numberOfLines]; 
         for (int i = 0; i < text1.length; i++) {
            text1[i] = br1.readLine();
         }
         br1.close();
         HashMap <String, String> remember = new HashMap <String, String> ();
         BufferedWriter bw = new BufferedWriter(new FileWriter(filename + "-reflexive"));
         for (int i = 0; i < text1.length; i++) {
         StringTokenizer st = new StringTokenizer(text1[i]," "); // tokenize each line
         String relation = st.nextToken().substring(1);
         String arg1 = st.nextToken();
         int number1 = arg1.indexOf("#");
         String arg2 = st.nextToken();
         int number2 = arg2.indexOf("#");
         if (arg1.startsWith("http://www.ordnancesurvey.co.uk"))
               arg1 = "os"+ arg1.substring(number1+1); 
         else arg1 = "sm" + arg1.substring(number1+1);
         String line1 = "[" + arg1 + " " + "BPT" + " " + arg1 + "]";
         int number3 = arg2.length()-1;
         if (arg2.startsWith("http://www.ordnancesurvey.co.uk")) 
               arg2 = "os"+ arg2.substring(number2+1, number3-1); 
         else arg2 = "sm" + arg2.substring(number2+1, number3-1);
         String line2 = "[" + arg2 + " " + "BPT" + " " + arg2 + "]";
         
         if (!remember.containsKey(arg1)) bw.write(line1 + "\n");
         remember.put(arg1,"yes");
         if (!remember.containsKey(arg1)) bw.write(line2 + "\n");
         remember.put(arg2,"yes"); 
         }
      bw.close();

     }
} 
