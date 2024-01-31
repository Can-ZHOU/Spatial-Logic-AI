import java.io.*;
import java.util.*;


public class Convert{

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
         for (int i = 0; i < numberOfLines; i++) {
            text1[i] = br1.readLine();
         }
         br1.close();
         BufferedWriter bw = new BufferedWriter(new FileWriter(filename + "-abox"));
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

         if (arg2.startsWith("http://www.ordnancesurvey.co.uk")) 
               arg2 = "os"+ arg2.substring(number2+1); 
         else arg2 = "sm" + arg2.substring(number2+1);
         String line = "[" +arg1 + " " + relation + " " + arg2 + "\n";
         bw.write(line);
         }
      bw.close();

     }
} 
