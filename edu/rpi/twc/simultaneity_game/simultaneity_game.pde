/**
   Start by clicking any square. When you do, another square will change. Click the square which
   changed to earn a point.  This will cause the next square to change, and so on. Ignore the
   squares which change randomly, and keep in mind that you are scored on accuracy, not speed.
   
   Sound clips produced by the AT&T TTS demo: http://www2.research.att.com/~ttsweb/tts/demo.php
   Note: these clips are for limited, private use only.
 */
 
import ddf.minim.*;

Minim minim;
AudioSample clicked,  intro, yes, no, correct, sorry;

int width = 500;
int height = 500;
int border = 50;
int padding = 10;

int pointsPerGame = 20;
int correctAnswers;
int incorrectAnswers;

class Cell {
  int i;
  int j;
  
  Cell(int i, int j) {
    this.i = i;
    this.j = j;
  }
  
  boolean equals(Cell other) {
    return i == other.i && j == other.j;
  }
}

class Matrix {
  int rows;
  int cols;
  int xlen, ylen;
  double xsize, ysize;
  int[][] colors;
  
  Matrix(int rows, int cols) {
    this.rows = rows;
    this.cols = cols;
   
    xsize = width / (double) cols;
    ysize = height / (double) rows;
  
    xlen = (int) xsize - padding;
    ylen = (int) ysize - padding;
    
     colors = new int[rows][cols];
     
    initialize();
  }
  
  Cell cellAt(int x, int y) {
    x -= border;
    y -= border;
    if (x < 0 || x >= width || y < 0 || y >= height) {
      return null;
    } else {
      int i = (int) (y / ysize);
      int j =(int) ( x / xsize); 
     
      if (x > (j * xsize) + xlen || y > (i * ysize) + ylen) {
        return null;
      } else {
        return new Cell(i, j);
      }  
    }
  } 
  
  void initialize() {    
    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        int c = (int) random(100);
        colors[i][j] = c;
        draw(i, j);
      }
    }
  }
   
  Cell randomCell() {
    int i = (int) random(rows);
    int j = (int) random(cols);
     
    return new Cell(i, j);
  } 
  
  void draw(Cell c) {
    draw(c.i, c.j);
  }
  
  void draw(int i, int j) {
    stroke(100);
      
    int y = border + (int) (i * ysize);
    int x = border + (int) (j * xsize);
    
    // Change the color of the cell randomly, but also make sure it changes noticeably.
    int c = (colors[i][j] + 20 + (int) random(60)) % 100;
    
    colors[i][j] = c;
    fill(c, 100, 100);
    rect(x, y, xlen, ylen);  
  }
}

class Runner implements Runnable {
  void run() {
    while (true) {
      Object m = "";
      
      try {
        long w = (long) random(3000);
        
        synchronized (m) {
          m.wait(w);
        }
      } catch (InterruptedException e) {
        System.err.println("interrupted!");
      }
      
       Cell r = matrix.randomCell();
       
       matrix.draw(r);   
    }
  }
}

Matrix matrix;
Cell targetCell = null;

void newGame() {
  correctAnswers = 0;
  incorrectAnswers = 0;
}

void setup() {
  colorMode(HSB, 100);
  background(100);
  size(width + (2 * border), height + (2 * border));
  
  matrix = new Matrix(3, 3);
  
    minim = new Minim(this);
  clicked = minim.loadSample("ahkey.aiff", 256);
  intro = minim.loadSample("intro.wav");
  yes = minim.loadSample("yes.wav");
  no = minim.loadSample("no.wav");
  correct = minim.loadSample("correct.wav");
  sorry = minim.loadSample("sorry.wav");
  
  intro.trigger();
  
  Runner r = new Runner();
  Thread t = new Thread(r);
  t.start();
}
		  
void draw() {
  //line(150, 25, mouseX, mouseY);
}
  
void updateScore(boolean isCorrect) {
  if (isCorrect) {
             correctAnswers++;
  } else {
    incorrectAnswers++;
  }
  
      int percentCorrect = (100 * correctAnswers) / (correctAnswers + incorrectAnswers);
       System.out.println("" + correctAnswers + "/" + (correctAnswers + incorrectAnswers) + " (" + percentCorrect + "%)"); 
}

 void mousePressed() {
   /*
   print("(");
   print(mouseX);
   print(", ");
   print(mouseY);
   println(")");*/
   
   Cell c = matrix.cellAt(mouseX, mouseY);
   if (null != c) {
     /*
     print("(");
     print(c.i);
     print(", ");
     print(c.j);
     println(")"); */
   
     if (null != targetCell) {
       if (c.equals(targetCell)) {
         updateScore(true);
         //println("correct!");
         //yes.trigger();
         correct.trigger();
       } else {
         updateScore(false);
         //println("wrong!");
         //no.trigger();
         sorry.trigger();
         targetCell = null;
         return;
       }       
     }
     
     targetCell = matrix.randomCell();
     
     // Don't let the same cell which was clicked, be changed (unless there is only one row in the matrix).
     if (targetCell.equals(c)) {
       targetCell.i = (targetCell.i + 1) % matrix.rows;
     }
     
     matrix.draw(targetCell);
   
     clicked.trigger();   
   }
 }
//*/  
    
