/*
  6/10/03

  This will take a text (say, the Hitchiker's Guide to the Galaxy) and
  "compress" it for abbreviated reading.  But you'd better have the text
  memorized or the output will be nothing but gibberish to you.

  i'd
  he'd
  it'd
  she'd
  they'd
  we'd
  you'd
  he'll
  i'll
  it'll
  she'll
  they'll
  we'll
  you'll
  i'm
  they're
  we're
  you're
  he's
  it's
  she's
  aren't
  can't
  couldn't
  didn't
  don't
  hadn't
  hasn't
  haven't
  isn't
  mightn't
  mustn't
  shan't
  shouldn't
  weren't
  won't
  wouldn't
  i've
  they've
  we've
  you've

  + all genetive uses of "*'s" (e.g. Arthur's)
*/

import java.io.*;

public class scrunch {

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

    int i=0, j=0, k=0, l=0;
    char ch = '\0', ch1 = '\0', ch2 = '\0';
    String s = "", s2 = "";
    boolean stop=false, clear=true, dowrite=false, check=false;
    while (!stop) {
      try
      {
        clear = true;
        s = input.readLine();
        if (s != null) {
          j = s.length();
          for (i=0; i<j; i++) {
            ch = s.charAt(i);
            if (ch > 32) {
              dowrite = false;
              if (ch=='\'') {
				dowrite = true;
				check = false;
				if ((!clear)&&(j>i+1)) {
				  ch1 = s.charAt(i+1);
				  if (j>i+2) {
					ch2 = s.charAt(i+2);
					if ( ((ch1=='v')&&(ch2=='e'))
					   ||((ch1=='l')&&(ch2=='l'))
					   ||((ch1=='r')&&(ch2=='e'))
					   )
					  check = true;
					if (check) {
					  if (j==i+3)
					    dowrite = false;
					  else {
						ch1 = s.charAt(i+3);
					    dowrite = (((ch1>47)&&(ch1<58))||((ch1>64)&&(ch1<91))||((ch1>96)&&(ch1<123)));
				      }
				    }
			      }
			      if (dowrite) {
                    if ((ch1=='m')||(ch1=='d')||(ch1=='s')||(ch1=='t'))
                      check = true;
                    if (check) {
					  if (j==i+2)
					    dowrite = false;
					  else {
					    ch1 = s.charAt(i+2);
					    dowrite = (((ch1>47)&&(ch1<58))||((ch1>64)&&(ch1<91))||((ch1>96)&&(ch1<123)));
				      }
				    }
			      }
			    }
              } else if (/*((ch>47)&&(ch<58))||*/((ch>64)&&(ch<91))||((ch>96)&&(ch<123))) {
                if (clear) {
                  dowrite = true;
                  clear = false;
                }
              } else {
                dowrite = true;
                clear = true;
              }
              if (dowrite) {
                if (k == 80) {
                  output.println(s2);
                  s2 = "";
                  k=0;
                }
                k++;
                s2 += ch;
              }
            } else
              clear = true;
          }
        } else
          stop = true;
      }
      catch(java.io.IOException e)
      {
        stop = true;
      }
    }
    if (k > 0)
      output.println(s2);

    try
    {
      input.close();
      output.close();
    }
    catch(java.io.IOException e)
    {
      //..
    }

  }

}