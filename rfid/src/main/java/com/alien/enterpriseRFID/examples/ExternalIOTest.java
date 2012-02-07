/**
 * Copyright 2007 Alien Technology Corporation. All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * <p>
 * 1)	Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * <p>
 * 2)	Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * <p>
 * 3)	Neither the name of Alien Technology Corporation nor the names of any
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL ALIEN TECHNOLOGY CORPORATION OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * <p>
 * For further information, contact :
 * <p>
 * Alien Technology
 * 18220 Butterfield Blvd.
 * Morgan Hill, CA 95037
 */

package com.alien.enterpriseRFID.examples;

import com.alien.enterpriseRFID.reader.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * Connects to a Reader on COM port #1 and presents a graphical view of the
 * External Inputs and Outputs. Clicking on an Output toggles its value.
 *
 * @version 1.3 Jan 2007
 * @author David Krull
 */
public class ExternalIOTest extends JFrame implements Runnable, ActionListener {
  private AlienClass1Reader reader;
  private Thread            motor;
  private int               newOutputValue = -1; // -1 means no change to outputs

  // Keep the buttons in an array for easy access later
  // Some readers may not utilize all the buttons (they will be hidden)
  private IOToggleButton[]  inputButtonArray = new IOToggleButton[4];
  private IOToggleButton[]  outputButtonArray = new IOToggleButton[8];

  // Labels below the buttons for displaying the actual reader value
  private JLabel            inputsValueLabel, outputsValueLabel;

  // These will be set depending on the type of reader connected
  private int               maxInputs, maxOutputs;


/**
 * Constructor
 */
public ExternalIOTest() {
  // Set up the user interface
  super("External I/O Tester");
  CreateGUI();
  addWindowListener(new WindowAdapter() {
    public void windowClosing(WindowEvent e) {
      System.exit(0);
    }
  });

  // Set up the reader
  try {
    reader = new AlienClass1Reader("COM1"); // Create reader object
    reader.open(); // Open the reader connection

    // Look at this reader's ReaderType, and set appropriate maxInputs & maxOuputs
    String readerType = reader.getReaderType();
    if (readerType.matches(".*ALR-96.0.*")) {
      // ALR-96x0 has 2 inputs, 2 outputs
      maxInputs = 2; maxOutputs = 2;
    } else if (readerType.matches(".*ALR-.800.*")) {
      // ALR-x800 has 4 inputs, 8 outputs
      maxInputs = 4; maxOutputs = 8;
    } else {
      // All other fixed readers have 4 inputs, 4 outputs
      maxInputs = 4; maxOutputs = 4;
    }

    // Hide the input/output buttons not supported by this reader
    for (int i=maxInputs; i<inputButtonArray.length; i++) inputButtonArray[i].setVisible(false);
    for (int i=maxOutputs; i<outputButtonArray.length; i++) outputButtonArray[i].setVisible(false);

  } catch (AlienReaderException ex) {
    ex.printStackTrace();
  }

  // Set up the motor thread to repeatedly poll the reader and update the UI
  motor = new Thread(this);
  motor.start();
}


/**
 * Called by the motor thread while it's running.
 */
public void run() {
  do {
    try {
      // Changing outputs is done in the same thread as the polling to avoid
      // confusion between different reader responses
      if (newOutputValue != -1) {
        // The outputs have changed - tell the reader
        reader.setExternalOutput(newOutputValue);
        newOutputValue = -1;
      }
      int readerInputs = reader.getExternalInput();
      int readerOutputs = reader.getExternalOutput();
      updateDisplay(readerInputs, readerOutputs);
      Thread.sleep(100);
    } catch (AlienReaderException ex) {
      ex.printStackTrace();
      motor = null;
    } catch (InterruptedException ie) {
    }
  } while (motor != null);
}


/**
 * Decodes the inputs and outputs of the reader, and updates the display.
 *
 * @param inputs the integer value representing the reader's external inputs
 * @param outputs the integer value representing the reader's external outputs
 */
private void updateDisplay(int inputs, int outputs) {
  int i;

  inputsValueLabel.setText("ExternalInput = "+inputs); // convert to String
  for (i=0; i<maxInputs; i++) {
    // enable an input button only in the ExternalInput bit mask has that bit set
    inputButtonArray[i].setSelected( (inputs & 1<<i) > 0);
  }

  outputsValueLabel.setText("ExternalOutput = "+outputs); // convert to String
  for (i=0; i<maxOutputs; i++) {
    // enable an output button only in the ExternalOutput bit mask has that bit set
    outputButtonArray[i].setSelected( (outputs & 1<<i) > 0);
  }
}


/**
 * Handles mouse click events on the output buttons.
 *
 * @param e the details of the event
 */
public void actionPerformed(ActionEvent e) {
  // When outputs change, queue up the newOutputValue for later.
  // It will be sent to the reader in the motor thread.
  newOutputValue = 0;
  for (int i=0; i<maxOutputs; i++) {
    if (outputButtonArray[i].isSelected()) {
      newOutputValue += 1<<i;
    }
  }
}


/**
 * Creates all of the user interface elements and lays them out nicely in the frame.
 */
private void CreateGUI() {
  Container contentPane = getContentPane();
  contentPane.setLayout(new BorderLayout());

  // Title goes at the top of frame
  JLabel titleLabel = new JLabel("External I/O Tester", JLabel.CENTER);
  titleLabel.setFont(new Font("Serif", Font.BOLD, 18));
  contentPane.add(titleLabel, BorderLayout.NORTH);

  //
  // Inputs panel
  // Has buttons across in the center, and Input Value at the bottom
  JPanel inputsPanel = new JPanel(new BorderLayout(0, 0));
  inputsPanel.setBorder(BorderFactory.createCompoundBorder(
        BorderFactory.createTitledBorder(
        BorderFactory.createLineBorder(Color.black), "Inputs",  TitledBorder.CENTER, TitledBorder.CENTER, new Font("SansSerif", Font.BOLD, 12), Color.black),
        BorderFactory.createEmptyBorder(10,10,10,10)));

  // All of the buttons go in their own panel, which will be in the center of the inputsPanel
  JPanel inputsButtonPanel = new JPanel(new FlowLayout());
  // Add them to the panel backwards (4,3,2,1)
  for (int i=3; i>=0; i--) {
    inputButtonArray[i] = new IOToggleButton(i+1, this);
    inputButtonArray[i].setEnabled(false);
    inputsButtonPanel.add(inputButtonArray[i]);
  }
  inputsPanel.add(inputsButtonPanel, BorderLayout.CENTER);

  // Input Value label goes at the bottom of the inputsPanel
  inputsValueLabel = new JLabel("", JLabel.CENTER);
  inputsValueLabel.setFont(new Font("SansSerif", Font.BOLD, 18));
  inputsPanel.add(inputsValueLabel, BorderLayout.SOUTH);

  //
  // Outputs panel
  // Has buttons across the center, and the Output Value at the bottom
  JPanel outputsPanel = new JPanel(new BorderLayout(0, 0));
  outputsPanel.setBorder(BorderFactory.createCompoundBorder(
        BorderFactory.createTitledBorder(
        BorderFactory.createLineBorder(Color.black), "Outputs",  TitledBorder.CENTER, TitledBorder.CENTER, new Font("SansSerif", Font.BOLD, 12), Color.black),
        BorderFactory.createEmptyBorder(10,10,10,10)));

  // All of the buttons go in their own panel, which will be in the center of the outputsPanel
  JPanel outputsButtonPanel = new JPanel(new FlowLayout());
  // Add them to the panel backwards (8,7,6,5,4,3,2,1)
  for (int i=7; i>=0; i--) {
    outputButtonArray[i] = new IOToggleButton(i+1, this);
    outputsButtonPanel.add(outputButtonArray[i]);
  }
  outputsPanel.add(outputsButtonPanel, BorderLayout.CENTER);

  // Output Value label goes at the bottom of the outputsPanel
  outputsValueLabel = new JLabel("", JLabel.CENTER);
  outputsValueLabel.setFont(new Font("SansSerif", Font.BOLD, 18));
  outputsPanel.add(outputsValueLabel, BorderLayout.SOUTH);

  // Inputs and Outputs panels to side-by-side in centerPanel
  JPanel centerPanel = new JPanel();
  centerPanel.add(inputsPanel);
  centerPanel.add(outputsPanel);

  // Add the center panel to center of the frame
  contentPane.add(centerPanel, BorderLayout.CENTER);
}


public static void main(String[] args) {
  ExternalIOTest frame = new ExternalIOTest();
  frame.pack();
  frame.setVisible(true);
}


/**
 * Convenience class to help set up the input/output buttons.
 */
private class IOToggleButton extends JToggleButton {
  public IOToggleButton(int buttonNum, ActionListener actionListener) {
    super("" + buttonNum);
    setSelected(false);
    addActionListener(actionListener);
    setPreferredSize(new Dimension(50, 75));
    setFont(new Font("SansSerif", Font.BOLD, 24));
    setFocusPainted(false);
  }
}


} // End of class ExternalIOTest