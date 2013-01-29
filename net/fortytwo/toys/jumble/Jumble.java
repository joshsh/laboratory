/*
  10/15/03

*/

import java.io.*;
import java.util.Random;
import java.util.Vector;

public class Jumble {

  public static String permute(String s) {

      Random rnd = new Random();

      String s2 = "";
      int n = s.length();
      
      int i=0, j=0;
      i=java.lang.Math.abs(rnd.nextInt()) % fak(n);
      Vector aPerm = onePerm(i, n);

      //int [] a2 = new int[n];
      for (i=0; i<n; i++) {
         j = ((Integer)aPerm.elementAt(i)).intValue();
         s2 += s.charAt(j);
      }
      return s2;
  }

   /** Computes the i-th permutation with k elements. The elements of the permutation are
    *  the integer numbers from 0 to k-1. The order of the permutation is undefined, 
    *  but reproducable, which means that always the same order is used.
    *  @param   i   number of permutation (must be 0<i<k!)
    *  @param   k   number of elements in this permutation
    *  @result  a Vector with a 
    */
   private static Vector onePerm(int i, int k)
      {
      Vector thisPerm = new Vector();
      
      if (k==1)
         {// one elemental perm
         thisPerm.addElement(new Integer(0));
         }
      else
         {// other perms
         thisPerm = onePerm((i/k), (k-1));
         thisPerm.insertElementAt(new Integer(k-1), (i%k));
         }
      return thisPerm;
      }

  private static int fak(int i) {
    int r=1;
      
    if (i==0) 
      return 1;
    else {
      while (i>1)
        r *= i--;
      return r;
    }
  }

  public static void main(String []args) {

    if (args.length < 2) {
      System.out.println("You need to specify an input and output file.");
      System.exit(0);
    }

    BufferedReader input = null;
    PrintWriter output = null;

    try
    {
      input = new BufferedReader(new FileReader(args[0]));
    }
    catch(java.io.FileNotFoundException e)
    {
      System.out.println("There was an error opening the input file.");
      System.exit(0);
    }

    try
    {
      output = new PrintWriter(new FileOutputStream(args[1]));
    }
    catch(java.io.FileNotFoundException e)
    {
      System.out.println("There was an error opening the output file.");
      System.exit(0);
    }

    int i=0, n=0;
    char ch='\0', ch2='\0';
    String s_in="", s_med="", s_out="";
    boolean stop=false, interior=false, holding=false;
    while(!stop) {
      try {
        s_in = input.readLine();
        if (s_in != null) {
          n = s_in.length();
          for (i=0; i<n; i++) {
            ch = s_in.charAt(i);
            if (((ch>64)&&(ch<91))||((ch>96)&&(ch<123))) {
              if (interior) {
                if (holding)
                  s_med += ch2;
                holding = true;
                ch2 = ch;
              } else
                s_out += ch;
              interior = true;
            } else {
              if (s_med.length() > 0) {
                s_out += permute(s_med);
                s_med = "";
              }
              if (holding)
                s_out += ch2;
              interior = false;
              holding = false;
              s_out += ch;
            }
          }
          if (s_med.length() > 0) {
            s_out += permute(s_med);
            s_med = "";
          }
          if (holding)
            s_out += ch2;
          interior = false;
          holding = false;
          output.println(s_out);
System.out.println(s_out);
          s_out = "";
        } else
          stop = true;
      } catch(java.io.IOException e) {
        stop = true;
      }
    }

    try
    {
      input.close();
      output.close();
    }
    catch(Exception e)
    {
      //..
    }

  }

}
