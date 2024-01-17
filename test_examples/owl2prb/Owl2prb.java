import java.io.*;
import java.util.*;


// Convert GeoMap data in OWL format to prb format.
public class Owl2prb {

    public static void main (String[] args) throws Exception {

	String infilename = args[0];
	File infile = new File(infilename);
	String outfilename = infile.getName() + "_abox";

	String relation = null;
	ArrayList<String> relations = new ArrayList<String>();

	BufferedReader br = new BufferedReader(new FileReader(infile));
	while ((relation = br.readLine()) != null){
	    relations.add(relation);
	}
	br.close();

	BufferedWriter bw = new BufferedWriter(new FileWriter(outfilename));
	bw.write("vars LBPT_mappings = [\n");
	for (String r : relations) {
	    // tokenize each line
	    StringTokenizer st = new StringTokenizer(r, " "); 
	    String predicate = st.nextToken().substring(1);
	    String arg1 = st.nextToken();
	    if (arg1.startsWith("http://www.ordnancesurvey.co.uk"))
		arg1 = "os"+ arg1.substring(arg1.indexOf("#") + 1); 
	    else
		arg1 = "sm" + arg1.substring(arg1.indexOf("#") + 1);
	     
	    String arg2 = st.nextToken();
	    if (arg2.startsWith("http://www.ordnancesurvey.co.uk")) 
		arg2 = "os"+ arg2.substring(arg2.indexOf("#") + 1); 
	    else arg2 =
		     "sm" + arg2.substring(arg2.indexOf("#") + 1);

	    bw.write( "[" +arg1 + " " + predicate + " " + arg2 + "\n" );
	}
	bw.write("];\n");
	bw.close();

    }
} 
