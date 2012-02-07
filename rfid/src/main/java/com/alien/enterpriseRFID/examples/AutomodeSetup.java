/**
 * Copyright (c) 2006 Alien Technology Corporation. All rights reserved.
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
import java.io.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * Connects to a Reader presents a graphical view of its AutoMode parameters.
 * Each phase of the AutoMode state engine is represented be one of a series of
 * tabs. Within each tab are fields for settings the parameters appropriate to
 * that phase. The commands sent to the reader are echoed in a text window for
 * inspection.
 *
 * @version 1.2 July 2008
 * @author David Krull
 */
public class AutomodeSetup extends JFrame implements ActionListener {

  private AlienClass1Reader reader;
  private JTextArea         outputText;
  private JLabel            connectStatusLabel;
  private JButton           connectButton;
  private String            connectButtonOn  = "Disconnect COM1";
  private String            connectButtonOff = "Connect to COM1 ";
  private JButton           logClearButton, logSaveButton, logProfileButton;

  private JButton           setupOnOffButton;
  private JButton           setupSaveSettingsButton;

  private JCheckBox         waitOutput1Checkbox, waitOutput2Checkbox;

  private JRadioButton      startImmediateRadio, startTriggerRadio;
  private JCheckBox         startInput1Checkbox, startInput2Checkbox;
  private JRadioButton      startRisingEdgeRadio, startFallingEdgeRadio;

  private JCheckBox         workOutput1Checkbox, workOutput2Checkbox;
  private JRadioButton      workActionAcquireRadio, workActionProgramRadio, workActionProgramLockRadio;
  private JRadioButton      workActionEraseRadio, workActionKillRadio;
  private FormField         workProgramIDField, workProgramPasscodeField;
  private JButton           workProgramIDButton, workProgramPasscodeButton;

  private FormField         stopTimeField;
  private JButton           stopTimeButton;
  private JCheckBox         stopInput1Checkbox, stopInput2Checkbox;
  private JRadioButton      stopRisingEdgeRadio, stopFallingEdgeRadio;

  private JCheckBox         evalTrueOutput1Checkbox, evalTrueOutput2Checkbox;
  private FormField         evalTruePauseField;
  private JButton           evalTruePauseButton;
  private JCheckBox         evalFalseOutput1Checkbox, evalFalseOutput2Checkbox;
  private FormField         evalFalsePauseField;
  private JButton           evalFalsePauseButton;

  private JButton           notifyOnOffButton;
  private JComboBox         notifyFormatPulldown;
  private JComboBox         notifyTriggerPulldown;
  private FormField         notifyTimeField;
  private JButton           notifyTimeButton;
  private JRadioButton      notifySerialRadio, notifyNetworkRadio, notifyMailRadio;
  private FormField         notifyNetworkAddressField, notifyNetworkPortField;
  private JButton           notifyNetworkButton;
  private FormField         notifyMailAddressField;
  private JButton           notifyMailAddressButton;
  private FormField         notifyMailServerField, notifyMailFromField;
  private JButton           notifyMailServerButton, notifyMailFromButton;


public AutomodeSetup() {
  // Set up the user interface
  super("AutoMode Setup");
  CreateGUI();
  addWindowListener(new WindowAdapter() {
    public void windowClosing(WindowEvent e) {
      System.exit(0);
    }
  });

  // Set up the reader
  connectToReader("COM1");
}


private void connectToReader(String address) {
  try {
    reader = new AlienClass1Reader(); // Create reader object
    reader.setConnection(address); // Specify address
    reader.open(); // Open the reader connection
    connectStatusLabel.setText("Connected");
    connectStatusLabel.setForeground(Color.black);
    connectButton.setText(connectButtonOn);
    populateGUIElements();
  } catch (Exception ex) {
    System.err.println("Failed to connect to a reader at COM1");
    ex.printStackTrace();
  }
}

private void disconnectFromReader() {
  reader.close();
  connectStatusLabel.setText("Not Connected!");
  connectStatusLabel.setForeground(Color.red);
  connectButton.setText(connectButtonOff);
}

public void actionPerformed(ActionEvent e) {
  Object eSource = e.getSource();

  try {
    // Controls components
    if (eSource == connectButton) {
      if (connectButton.getText().equals(connectButtonOff)) {
        connectToReader("COM1");
      } else {
        disconnectFromReader();
      }
    }
    if (eSource == logClearButton) {
      outputText.setText("");
    }
    if (eSource == logSaveButton) {
      saveLog();
    }
    if (eSource == logProfileButton) {
      profileReader();
    }

    // Setup components
    if (eSource == setupOnOffButton) {
      if (setupOnOffButton.getText().equals("Automode is ON")) {
        setupOnOffButton.setText("Automode is OFF");
        outputText.append("set AutoMode = Off\n");
        reader.setAutoMode(AlienClass1Reader.OFF);
      } else {
        setupOnOffButton.setText("Automode is ON");
        outputText.append("set AutoMode = On\n");
        reader.setAutoMode(AlienClass1Reader.ON);
      }
    }
    if (eSource == setupSaveSettingsButton) {
      outputText.append("save\n");
      reader.doReaderCommand("save");
      outputText.append(reader.getReaderReply() + "\n");
    }

    // Wait components
    if ((eSource == waitOutput1Checkbox) || (eSource == waitOutput2Checkbox)) {
      int outputValue = getIOValueFromCheckboxes(waitOutput1Checkbox, waitOutput2Checkbox);
      outputText.append("set AutoWaitOutput = " + outputValue + "\n");
      reader.setAutoWaitOutput(outputValue);
    }

    // Start Components
    if (eSource == startImmediateRadio) {
      outputText.append("set AutoStartTrigger = 0, 0\n");
      reader.setAutoStartTrigger(0, 0);
      startInput1Checkbox.setEnabled(false);
      startInput2Checkbox.setEnabled(false);
      startRisingEdgeRadio.setEnabled(false);
      startFallingEdgeRadio.setEnabled(false);
    }
    if ( (eSource == startTriggerRadio) ||
        (eSource == startInput1Checkbox) ||
        (eSource == startInput2Checkbox) ||
        (eSource == startRisingEdgeRadio) ||
        (eSource == startFallingEdgeRadio)) {
      startInput1Checkbox.setEnabled(true);
      startInput2Checkbox.setEnabled(true);
      startRisingEdgeRadio.setEnabled(true);
      startFallingEdgeRadio.setEnabled(true);
      int outputValue = getIOValueFromCheckboxes(startInput1Checkbox, startInput2Checkbox);
      if (startRisingEdgeRadio.isSelected()) {
        outputText.append("set AutoStartTrigger = " + outputValue + ", 0\n");
        reader.setAutoStartTrigger(outputValue, 0);
      } else {
        outputText.append("set AutoStartTrigger = 0, " + outputValue + "\n");
        reader.setAutoStartTrigger(0, outputValue);
      }
    }

    // Work Components
    if ( (eSource == workOutput1Checkbox) || (eSource == workOutput2Checkbox)) {
      int outputValue = getIOValueFromCheckboxes(workOutput1Checkbox, workOutput2Checkbox);
      outputText.append("set AutoWorkOutput = " + outputValue + "\n");
      reader.setAutoWorkOutput(outputValue);
    }
    if (eSource == workActionAcquireRadio) {
      outputText.append("set AutoAction = Acquire\n");
      reader.setAutoAction("Acquire");
      workProgramIDField.setEnabled(false);
      workProgramIDButton.setEnabled(false);
      workProgramPasscodeField.setEnabled(false);
      workProgramPasscodeButton.setEnabled(false);
    }
    if (eSource == workActionProgramRadio) {
      outputText.append("set Function = Programmer\n");
      outputText.append("set AutoAction = Program\n");
      reader.doReaderCommand("set Function = Programmer");
      reader.setAutoAction("Program");
      workProgramIDField.setEnabled(true);
      workProgramIDButton.setEnabled(true);
      workProgramPasscodeField.setEnabled(false);
      workProgramPasscodeButton.setEnabled(false);
    }
    if (eSource == workActionProgramLockRadio) {
      outputText.append("set function = Programmer\n");
      outputText.append("set AutoAction = Program And Lock\n");
      reader.doReaderCommand("set Function = Programmer");
      reader.setAutoAction("Program and Lock");
      workProgramIDField.setEnabled(true);
      workProgramIDButton.setEnabled(true);
      workProgramPasscodeField.setEnabled(true);
      workProgramPasscodeButton.setEnabled(true);
    }
    if (eSource == workActionEraseRadio) {
      outputText.append("set function = Programmer\n");
      outputText.append("set AutoAction = Erase\n");
      reader.doReaderCommand("set Function = Programmer");
      reader.setAutoAction("Erase");
      workProgramIDField.setEnabled(false);
      workProgramIDButton.setEnabled(false);
      workProgramPasscodeField.setEnabled(false);
      workProgramPasscodeButton.setEnabled(false);
    }
    if (eSource == workActionKillRadio) {
      outputText.append("set function = Programmer\n");
      outputText.append("set AutoAction = Kill\n");
      reader.doReaderCommand("set Function = Programmer");
      reader.setAutoAction("Kill");
      workProgramIDField.setEnabled(false);
      workProgramIDButton.setEnabled(false);
      workProgramPasscodeField.setEnabled(true);
      workProgramPasscodeButton.setEnabled(true);
    }
    if ( (eSource == workProgramIDButton) || (eSource == workProgramIDField)) {
      String programID = workProgramIDField.getText();
      outputText.append("set ProgramID = " + programID + "\n");
      reader.doReaderCommand("set ProgramID = " + programID);
    }
    if ( (eSource == workProgramPasscodeButton) || (eSource == workProgramPasscodeField)) {
      String programPasscode = workProgramPasscodeField.getText();
      outputText.append("set ProgramPassCode = " + programPasscode + "\n");
      reader.doReaderCommand("set ProgramPassCode = " + programPasscode);
    }

    // Stop Components
    if ((eSource == stopTimeButton) || (eSource == stopTimeField)) {
      outputText.append("set AutoStopTimer = " + stopTimeField.getText() + "\n");
      reader.setAutoStopTimer(stopTimeField.getInt());
    }
    if ((eSource == stopInput1Checkbox) ||
        (eSource == stopInput2Checkbox) ||
        (eSource == stopRisingEdgeRadio) ||
        (eSource == stopFallingEdgeRadio)) {
      int outputValue = getIOValueFromCheckboxes(stopInput1Checkbox, stopInput2Checkbox);
      if (stopRisingEdgeRadio.isSelected()) {
        outputText.append("set AutoStopTrigger = " + outputValue + ", 0\n");
        reader.setAutoStopTrigger(outputValue, 0);
      } else {
        outputText.append("set AutoStopTrigger = 0, " + outputValue + "\n");
        reader.setAutoStopTrigger(0, outputValue);
      }
    }

    // True Evaluation Components
    if ((eSource == evalTrueOutput1Checkbox) || (eSource == evalTrueOutput2Checkbox)) {
      int outputValue = getIOValueFromCheckboxes(evalTrueOutput1Checkbox, evalTrueOutput2Checkbox);
      outputText.append("set AutoTrueOutput = " + outputValue + "\n");
      reader.setAutoTrueOutput(outputValue);
    }
    if ((eSource == evalTruePauseButton) || (eSource == evalTruePauseField)) {
      outputText.append("set AutoTruePause = " + evalTruePauseField.getText() + "\n");
      reader.setAutoTruePause(evalTruePauseField.getInt());
    }

    // False Evaluation Components
    if ((eSource == evalFalseOutput1Checkbox) || (eSource == evalFalseOutput2Checkbox)) {
      int outputValue = getIOValueFromCheckboxes(evalFalseOutput1Checkbox, evalFalseOutput2Checkbox);
      outputText.append("set AutoFalseOutput = " + outputValue + "\n");
      reader.setAutoFalseOutput(outputValue);
    }
    if ((eSource == evalFalsePauseButton) || (eSource == evalFalsePauseField)) {
      outputText.append("set AutoFalsePause = " + evalFalsePauseField.getText() + "\n");
      reader.setAutoFalsePause(evalFalsePauseField.getInt());
    }

    // Notify Components
    if (eSource == notifyOnOffButton) {
      if (notifyOnOffButton.getText().equals("Notify is ON")) {
        notifyOnOffButton.setText("Notify is OFF");
        outputText.append("set NotifyMode = Off\n");
        reader.setNotifyMode(AlienClass1Reader.OFF);
      } else {
        notifyOnOffButton.setText("Notify is ON");
        outputText.append("set NotifyMode = On\n");
        reader.setNotifyMode(AlienClass1Reader.ON);
      }
    }
    if (eSource == notifyFormatPulldown) {
      String notifyFormat = (String)notifyFormatPulldown.getSelectedItem();
      outputText.append("set NotifyFormat = " + notifyFormat + "\n");
      reader.setNotifyFormat(notifyFormat);
    }
    if (eSource == notifyTriggerPulldown) {
      String notifyTrigger = (String)notifyTriggerPulldown.getSelectedItem();
      outputText.append("set NotifyTrigger = " + notifyTrigger + "\n");
      reader.setNotifyTrigger(notifyTrigger);
    }
    if ((eSource == notifyTimeButton) || (eSource == notifyTimeField)) {
      outputText.append("set NotifyTime = " + notifyTimeField.getText() + "\n");
      reader.setNotifyTime(notifyTimeField.getInt());
    }
    if (eSource == notifySerialRadio) {
      outputText.append("set NotifyAddress = Serial\n");
      reader.setNotifyAddress("Serial");
    }
    if ((eSource == notifyNetworkRadio) ||
        (eSource == notifyNetworkButton) ||
        (eSource == notifyNetworkAddressField) ||
        (eSource == notifyNetworkPortField)) {
      if (notifyNetworkAddressField.getText().equals("")) {
        notifyNetworkAddressField.setText("IPAddress");
      }
      if (notifyNetworkPortField.getText().equals("")) {
        notifyNetworkPortField.setText("port");
      }
      outputText.append("set NotifyAddress = " + notifyNetworkAddressField.getText() + ":" + notifyNetworkPortField.getText() + "\n");
      reader.setNotifyAddress(notifyNetworkAddressField.getText() + ":" + notifyNetworkPortField.getText());
    }
    if ((eSource == notifyMailRadio) ||
        (eSource == notifyMailAddressButton) ||
        (eSource == notifyMailAddressField)) {
      if (notifyMailAddressField.getText().equals("")) {
        notifyMailAddressField.setText("email@company.com");
      }
      outputText.append("set NotifyAddress = " + notifyMailAddressField.getText() + "\n");
      reader.setNotifyAddress(notifyMailAddressField.getText());
    }
    if ((eSource == notifyMailServerButton) || (eSource == notifyMailServerField)) {
      outputText.append("set MailServer = " + notifyMailServerField.getText() + "\n");
      reader.setMailServer(notifyMailServerField.getText());
    }
    if ((eSource == notifyMailFromButton) || (eSource == notifyMailFromField)) {
      outputText.append("set MailFrom = " + notifyMailFromField.getText() + "\n");
      reader.setMailFrom(notifyMailFromField.getText());
    }
  } catch (Exception ex) {
    outputText.append("*** Error ***\n");
    System.err.println("\nError. There has been an error communicating with the reader.");
    ex.printStackTrace();
  }
}


/**
 * Create a profile of the reader by listing the commands needed to put a fresh
 * reader into the automode configuration matching the reader connected on COM1.
 */
private void profileReader() {
//  outputText.append("\nBegin Profile\n");

//  outputText.append("\nEnd Profile\n");
}


/**
 * Save the contents of the log to a location chosen by the user.
 */
private void saveLog() {
  // File Choose Browse Button
  JFileChooser fileChooser = new JFileChooser();
  fileChooser.setPreferredSize(new Dimension(600, 400));
  fileChooser.setCurrentDirectory(new File("", ""));
  fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
  int returnVal = fileChooser.showSaveDialog(this);
  if (returnVal == JFileChooser.APPROVE_OPTION) {
    File file = fileChooser.getSelectedFile();
    try {
      StringReader stringReader = new StringReader(outputText.getText());
      BufferedReader bufferedReader = new BufferedReader(stringReader);
      FileWriter fileWriter = new FileWriter(file);
      String line = null;
      String newLine = System.getProperty("line.separator");
      do {
        line = bufferedReader.readLine();
        if (line != null) {
          fileWriter.write(line);
          fileWriter.write(newLine);
        }
      } while (line != null);
      fileWriter.close();
    } catch (Exception ex) {
      System.err.println("Error saving log");
      ex.printStackTrace();
    }
  }
}


/**
 * Calculate the integer value of the External I/O state, given two JCheckBoxes
 * representing those I/O pins.
 */
private int getIOValueFromCheckboxes(JCheckBox pin1, JCheckBox pin2) {
  int value = 0;
  if (pin1.isSelected()) value = 1;
  if (pin2.isSelected()) value += 2;
  return value;
}


/**
 * Set two JCheckboxes, according to integer value of the External I/O state.
 */
private void setCheckboxesFromIOValue(JCheckBox pin1, JCheckBox pin2, int value) {
  pin1.setSelected((value&1)>0);
  pin2.setSelected((value&2)>0);
}


/**
 * Creates all of the user interface elements and lays them out nicely in the frame.
 */
private void CreateGUI() {
  Container contentPane = getContentPane();
  contentPane.setLayout(new BorderLayout());

  // Title goes at the top of frame
  JPanel titlePanel = new JPanel();
  JLabel titleLabel = new JLabel("<html>AutoMode Setup Utility<br>", JLabel.CENTER);
  titleLabel.setFont(new Font("SansSerif", Font.BOLD, 18));
  titlePanel.add(titleLabel);
  contentPane.add(titlePanel, BorderLayout.NORTH);

  // Tabbed Pane
  JTabbedPane tabsPane = new JTabbedPane();
  tabsPane.setTabPlacement(JTabbedPane.LEFT);
  tabsPane.setPreferredSize(new Dimension(400, 410));

  // Add the various tabs to the tabsPane
  tabsPane.addTab("Setup", createSetupTab());
  tabsPane.addTab("Wait", createWaitTab());
  tabsPane.addTab("Start", createStartTab());
  tabsPane.addTab("Work", createWorkTab());
  tabsPane.addTab("Stop", createStopTab());
  tabsPane.addTab("Eval", createEvalTab());
  tabsPane.addTab("Notify", createNotifyTab());

  // Output Log panel
  JPanel outputLogPanel = new JPanel();
  outputLogPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 0));
  JScrollPane outputScroller = new JScrollPane();
  outputText = new JTextArea();
  outputText.setPreferredSize(new Dimension(350, 400));
  outputText.setFont(new Font("Monospaced", Font.PLAIN, 12));
  outputScroller.getViewport().add(outputText);
  outputScroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
  outputScroller.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
  outputLogPanel.add(outputScroller);

  // Controls panel
  JPanel controlsPanel = new JPanel();
  controlsPanel.setLayout(new BoxLayout(controlsPanel, BoxLayout.PAGE_AXIS));
  controlsPanel.setBorder(BorderFactory.createTitledBorder(
      BorderFactory.createEmptyBorder(),
      "Controls",
      TitledBorder.CENTER,
      TitledBorder.CENTER,
      new Font("SansSerif", Font.BOLD, 18),
      Color.black)
  );

  connectStatusLabel = new JLabel("Not Connected");
  connectStatusLabel.setFont(new Font("Sans-serif", Font.BOLD, 14));
  connectStatusLabel.setForeground(Color.red);
  controlsPanel.add(connectStatusLabel);

  connectButton = new JButton("Connect");
  connectButton.setFocusPainted(false);
  connectButton.addActionListener(this);
  controlsPanel.add(connectButton);

  controlsPanel.add(Box.createVerticalGlue());
/*
  logProfileButton = new JButton("Profile Reader");
  logProfileButton.setFocusPainted(false);
  logProfileButton.addActionListener(this);
  controlsPanel.add(logProfileButton);

  controlsPanel.add(Box.createVerticalGlue());
*/
  logClearButton = new JButton("Clear Log");
  logClearButton.setFocusPainted(false);
  logClearButton.addActionListener(this);
  controlsPanel.add(logClearButton);

  logSaveButton = new JButton("Save Log");
  logSaveButton.setFocusPainted(false);
  logSaveButton.addActionListener(this);
  controlsPanel.add(logSaveButton);


  // Add the tabs panel, output log panel, and controls panel to the window
  contentPane.add(tabsPane, BorderLayout.WEST);
  contentPane.add(outputLogPanel, BorderLayout.CENTER);
  contentPane.add(controlsPanel, BorderLayout.EAST);
}


//
// Setup Tab
//
private JPanel createSetupTab() {
  JPanel setupTab = new TabPanel("Automode Setup");
  setupTab.add(Box.createVerticalStrut(10));

/*
  // Reader Connection Panel
  JPanel setupConnectionPanel = new ParameterGroup("Reader's IP Address or COM Port");
  setupTab.add(setupConnectionPanel);
  setupAddressField = new FormField(15, true, this);
  setupConnectionPanel.add(setupAddressField);
  setupAddressButton = new SetButton(this);
  setupAddressButton.setText("Connect");
  setupConnectionPanel.add(setupAddressButton);
*/
  // On/Off Button
  JPanel setupOnOffPanel = new ParameterGroup("Automode On/Off");
  setupTab.add(setupOnOffPanel);
  setupOnOffButton = new JButton("Automode is OFF");
  setupOnOffButton.setFocusPainted(false);
  setupOnOffButton.addActionListener(this);
  setupOnOffButton.setPreferredSize(new Dimension(160, 30));
  setupOnOffPanel.add(setupOnOffButton);

  // Save Settings Button
  JPanel setupSaveSettingsPanel = new ParameterGroup("Save Settings to Flash");
  setupTab.add(setupSaveSettingsPanel);
  setupSaveSettingsButton = new JButton("Save Settings");
  setupSaveSettingsButton.setFocusPainted(false);
  setupSaveSettingsButton.addActionListener(this);
  setupSaveSettingsButton.setPreferredSize(new Dimension(160, 30));
  setupSaveSettingsPanel.add(setupSaveSettingsButton);

  return setupTab;
}


//
// Wait Tab
//
private JPanel createWaitTab() {
  JPanel waitTab = new TabPanel("Waiting");
  // Wait Pins
  JPanel waitOutputPinsPanel = new ParameterGroup("Output Pins");
  waitTab.add(waitOutputPinsPanel);
  waitOutput1Checkbox = new JCheckBox("Output 1");
  waitOutput1Checkbox.addActionListener(this);
  waitOutputPinsPanel.add(waitOutput1Checkbox);
  waitOutput2Checkbox = new JCheckBox("Output 2");
  waitOutput2Checkbox.addActionListener(this);
  waitOutputPinsPanel.add(waitOutput2Checkbox);

  return waitTab;
}


//
// Start Tab
//
private JPanel createStartTab() {
  JPanel startTab = new TabPanel("Start");

  // Start Trigger
  ButtonGroup startTriggerGroup = new ButtonGroup();
  startImmediateRadio = new JRadioButton("Immediately", true);
  startImmediateRadio.addActionListener(this);
  startImmediateRadio.setAlignmentX(Component.LEFT_ALIGNMENT);
  startTriggerGroup.add(startImmediateRadio);
  startTab.add(startImmediateRadio);
  startTriggerRadio = new JRadioButton("External Trigger");
  startTriggerRadio.addActionListener(this);
  startTriggerRadio.setAlignmentX(Component.LEFT_ALIGNMENT);
  startTriggerGroup.add(startTriggerRadio);
  startTab.add(startTriggerRadio);

  // Start Trigger Pins
  JPanel startInputPinsPanel = new ParameterGroup("Input Pins");
  startTab.add(startInputPinsPanel);
  startInput1Checkbox = new JCheckBox("Input 1");
  startInput1Checkbox.setEnabled(false);
  startInput1Checkbox.addActionListener(this);
  startInputPinsPanel.add(startInput1Checkbox);
  startInput2Checkbox = new JCheckBox("Input 2");
  startInput2Checkbox.setEnabled(false);
  startInput2Checkbox.addActionListener(this);
  startInputPinsPanel.add(startInput2Checkbox);

  // Start Trigger Rising/Falling
  JPanel startEdgesPanel = new ParameterGroup("Pin Edges");
  startTab.add(startEdgesPanel);
  ButtonGroup startEdgesGroup = new ButtonGroup();
  startRisingEdgeRadio = new JRadioButton("Rising", true);
  startRisingEdgeRadio.setEnabled(false);
  startRisingEdgeRadio.addActionListener(this);
  startEdgesGroup.add(startRisingEdgeRadio);
  startEdgesPanel.add(startRisingEdgeRadio);
  startFallingEdgeRadio = new JRadioButton("Falling");
  startFallingEdgeRadio.setEnabled(false);
  startFallingEdgeRadio.addActionListener(this);
  startEdgesGroup.add(startFallingEdgeRadio);
  startEdgesPanel.add(startFallingEdgeRadio);

  return startTab;
}


//
// Working Tab
//
private JPanel createWorkTab() {
  JPanel workTab = new TabPanel("Working");

  // Working Pins
  JPanel workOutputPinsPanel = new ParameterGroup("Output Pins");
  workTab.add(workOutputPinsPanel);
  workOutput1Checkbox = new JCheckBox("Output 1");
  workOutput1Checkbox.addActionListener(this);
  workOutputPinsPanel.add(workOutput1Checkbox);
  workOutput2Checkbox = new JCheckBox("Output 2");
  workOutput2Checkbox.addActionListener(this);
  workOutputPinsPanel.add(workOutput2Checkbox);

  // Working Action
  JPanel workActionPanel = new ParameterGroup("Action");
  workActionPanel.setLayout(new BoxLayout(workActionPanel, BoxLayout.PAGE_AXIS));
  workTab.add(workActionPanel);
  ButtonGroup workActionGroup = new ButtonGroup();
  workActionAcquireRadio = new JRadioButton("Acquire", true);
  workActionAcquireRadio.addActionListener(this);
  workActionGroup.add(workActionAcquireRadio);
  workActionPanel.add(workActionAcquireRadio);
  workActionProgramRadio = new JRadioButton("Program");
  workActionProgramRadio.addActionListener(this);
  workActionGroup.add(workActionProgramRadio);
  workActionPanel.add(workActionProgramRadio);
  workActionProgramLockRadio = new JRadioButton("Program & Lock");
  workActionProgramLockRadio.addActionListener(this);
  workActionGroup.add(workActionProgramLockRadio);
  workActionPanel.add(workActionProgramLockRadio);
  workActionEraseRadio = new JRadioButton("Erase");
  workActionEraseRadio.addActionListener(this);
  workActionGroup.add(workActionEraseRadio);
  workActionPanel.add(workActionEraseRadio);
  workActionKillRadio = new JRadioButton("Kill");
  workActionKillRadio.addActionListener(this);
  workActionGroup.add(workActionKillRadio);
  workActionPanel.add(workActionKillRadio);
  JPanel programIDPanel = new ParameterGroup("Program Tag ID (xx xx xx xx xx xx xx xx)");
  workProgramIDField = new FormField(13, false, this);
  programIDPanel.add(workProgramIDField);
  workProgramIDButton = new SetButton(this);
  workProgramIDButton.setEnabled(false);
  programIDPanel.add(workProgramIDButton);
  workActionPanel.add(programIDPanel);
  JPanel programPasscodePanel = new ParameterGroup("Kill/Lock Passcode (xx)");
  workProgramPasscodeField = new FormField(2, false, this);
  programPasscodePanel.add(workProgramPasscodeField);
  workProgramPasscodeButton = new SetButton(this);
  workProgramPasscodeButton.setEnabled(false);
  programPasscodePanel.add(workProgramPasscodeButton);
  workActionPanel.add(programPasscodePanel);

  return workTab;
}


//
// Stop Tab
//
private JPanel createStopTab() {
  JPanel stopTab = new TabPanel("Stop");

  // Stop Timer
  JPanel stopTimePanel = new ParameterGroup("Delay (msec)");
  stopTab.add(stopTimePanel);
  stopTimeField = new FormField(6, true, this);
  stopTimePanel.add(stopTimeField);
  stopTimeButton = new SetButton(this);
  stopTimePanel.add(stopTimeButton);

  // Stop Trigger Pins
  JPanel stopTriggerPanel = new ParameterGroup("External Trigger");
  stopTab.add(stopTriggerPanel);
  JPanel stopInputPinsPanel = new ParameterGroup("Input Pins");
  stopTriggerPanel.add(stopInputPinsPanel);
  stopInput1Checkbox = new JCheckBox("Input 1");
  stopInput1Checkbox.addActionListener(this);
  stopInputPinsPanel.add(stopInput1Checkbox);
  stopInput2Checkbox = new JCheckBox("Input 2");
  stopInput2Checkbox.addActionListener(this);
  stopInputPinsPanel.add(stopInput2Checkbox);

  // Stop Trigger Rising/Falling
  JPanel stopEdgesPanel = new ParameterGroup("Pin Edges");
  stopTriggerPanel.add(stopEdgesPanel);
  ButtonGroup stopEdgesGroup = new ButtonGroup();
  stopRisingEdgeRadio = new JRadioButton("Rising", true);
  stopRisingEdgeRadio.addActionListener(this);
  stopEdgesGroup.add(stopRisingEdgeRadio);
  stopEdgesPanel.add(stopRisingEdgeRadio);
  stopFallingEdgeRadio = new JRadioButton("Falling");
  stopFallingEdgeRadio.addActionListener(this);
  stopEdgesGroup.add(stopFallingEdgeRadio);
  stopEdgesPanel.add(stopFallingEdgeRadio);

  return stopTab;
}


//
// Evaluation Tab
//
private JPanel createEvalTab() {
  JPanel evalTab = new TabPanel("Evaluation");

  // Eval True
  JPanel evalTruePanel = new ParameterGroup("True Evaluation");
  evalTab.add(evalTruePanel);
  JPanel evalTruePinsPanel = new ParameterGroup("Output Pins");
  evalTruePanel.add(evalTruePinsPanel);
  evalTrueOutput1Checkbox = new JCheckBox("Output 1");
  evalTrueOutput1Checkbox.addActionListener(this);
  evalTruePinsPanel.add(evalTrueOutput1Checkbox);
  evalTrueOutput2Checkbox = new JCheckBox("Output 2");
  evalTrueOutput2Checkbox.addActionListener(this);
  evalTruePinsPanel.add(evalTrueOutput2Checkbox);
  JPanel evalTruePausePanel = new ParameterGroup("Pause (msec)");
  evalTruePanel.add(evalTruePausePanel);
  evalTruePauseField = new FormField(6, true, this);
  evalTruePausePanel.add(evalTruePauseField);
  evalTruePauseButton = new SetButton(this);
  evalTruePausePanel.add(evalTruePauseButton);

  // Eval False
  JPanel evalFalsePanel = new ParameterGroup("False Evaluation");
  evalTab.add(evalFalsePanel);
  JPanel evalFalsePinsPanel = new ParameterGroup("Output Pins");
  evalFalsePanel.add(evalFalsePinsPanel);
  evalFalseOutput1Checkbox = new JCheckBox("Output 1");
  evalFalseOutput1Checkbox.addActionListener(this);
  evalFalsePinsPanel.add(evalFalseOutput1Checkbox);
  evalFalseOutput2Checkbox = new JCheckBox("Output 2");
  evalFalseOutput2Checkbox.addActionListener(this);
  evalFalsePinsPanel.add(evalFalseOutput2Checkbox);
  JPanel evalFalsePausePanel = new ParameterGroup("Pause (msec)");
  evalFalsePanel.add(evalFalsePausePanel);
  evalFalsePauseField = new FormField(6, true, this);
  evalFalsePausePanel.add(evalFalsePauseField);
  evalFalsePauseButton = new SetButton(this);
  evalFalsePausePanel.add(evalFalsePauseButton);

  return evalTab;
}


//
// Notify Tab
//
private JPanel createNotifyTab() {
  JPanel notifyTab = new TabPanel("Notify");
  JPanel notifyBasicPanel = new ParameterGroup("Notify Basic");
  notifyTab.add(notifyBasicPanel);
  notifyBasicPanel.setBorder(BorderFactory.createEmptyBorder());

  // Notify On/Off
  JPanel notifyOnOffPanel = new ParameterGroup("Notify On/Off");
  notifyBasicPanel.add(notifyOnOffPanel);
  notifyOnOffButton = new JButton("Notify is OFF");
  notifyOnOffButton.addActionListener(this);
  notifyOnOffButton.setFocusPainted(false);
  notifyOnOffButton.setPreferredSize(new Dimension(110, 26));
  notifyOnOffPanel.add(notifyOnOffButton);

  // Notify Format
  JPanel notifyFormatPanel = new ParameterGroup("Format");
  notifyBasicPanel.add(notifyFormatPanel);
  notifyFormatPulldown = new JComboBox(new String[]{"Text", "XML", "Custom"});
  notifyFormatPanel.add(notifyFormatPulldown);
  notifyFormatPulldown.addActionListener(this);

  // Notify Trigger Panel
  JPanel notifyTriggerPanel = new ParameterGroup("Trigger");
  notifyTab.add(notifyTriggerPanel);

  // Taglist Trigger
  JPanel notifyTaglistEventPanel = new ParameterGroup("Taglist Event");
  notifyTriggerPanel.add(notifyTaglistEventPanel);
  notifyTriggerPulldown = new JComboBox(new String[]{"OFF", "Add", "Remove", "Change", "True", "False", "TrueFalse"});
  notifyTaglistEventPanel.add(notifyTriggerPulldown);
  notifyTriggerPulldown.addActionListener(this);

  // Notify Time
  JPanel notifyTimePanel = new ParameterGroup("Time (sec)");
  notifyTriggerPanel.add(notifyTimePanel);
  notifyTimeField = new FormField(6, true, this);
  notifyTimePanel.add(notifyTimeField);
  notifyTimeButton = new SetButton(this);
  notifyTimePanel.add(notifyTimeButton);

  // Notify Address
  JPanel notifyAddressPanel = new ParameterGroup("Destination");
  notifyTab.add(notifyAddressPanel);
  notifyAddressPanel.setLayout(new BoxLayout(notifyAddressPanel, BoxLayout.PAGE_AXIS));
  ButtonGroup notifyAddressGroup = new ButtonGroup();

  // Notify Address (Serial)
  notifySerialRadio = new JRadioButton("Serial");
  notifySerialRadio.addActionListener(this);
  notifyAddressGroup.add(notifySerialRadio);
  notifyAddressPanel.add(notifySerialRadio);

  // Notify Address (Network)
  JPanel notifyNetworkRow = new ComplexFormRow();
  notifyAddressPanel.add(notifyNetworkRow);
  notifyNetworkRadio = new JRadioButton("Network");
  notifyNetworkRadio.addActionListener(this);
  notifyAddressGroup.add(notifyNetworkRadio);
  notifyNetworkRow.add(notifyNetworkRadio);
  notifyNetworkAddressField = new FormField(10, true, this);
  notifyNetworkRow.add(notifyNetworkAddressField);
  notifyNetworkRow.add(new JLabel(" : "));
  notifyNetworkPortField = new FormField(3, true, this);
  notifyNetworkRow.add(notifyNetworkPortField);
  notifyNetworkRow.add(Box.createHorizontalStrut(5));
  notifyNetworkButton = new SetButton(this);
  notifyNetworkRow.add(notifyNetworkButton);

  // Notify Address (E-mail)
  // radio button line
  JPanel notifyMailRow = new ComplexFormRow();
  notifyAddressPanel.add(notifyMailRow);
  notifyMailRow.setAlignmentX(Component.LEFT_ALIGNMENT);
  notifyMailRadio = new JRadioButton("E-mail");
  notifyMailRadio.addActionListener(this);
  notifyAddressGroup.add(notifyMailRadio);
  notifyMailRow.add(notifyMailRadio);
  notifyMailRow.add(Box.createHorizontalStrut(13));
  notifyMailAddressField = new FormField(14, true, this);
  notifyMailRow.add(notifyMailAddressField);
  notifyMailRow.add(new JLabel(" "));
  notifyMailAddressButton = new SetButton(this);
  notifyMailRow.add(notifyMailAddressButton);

  // mail configuration panel
  JPanel notifyMailConfigPanel = new ParameterGroup("E-mail Configuration");
  notifyAddressPanel.add(notifyMailConfigPanel);
  JPanel notifyMailServerPanel = new ParameterGroup("Mail Server");
  notifyMailConfigPanel.add(notifyMailServerPanel);
  notifyMailServerField = new FormField(8, true, this);
  notifyMailServerPanel.add(notifyMailServerField);
  notifyMailServerButton = new SetButton(this);
  notifyMailServerPanel.add(notifyMailServerButton);
  JPanel notifyMailFromPanel = new ParameterGroup("\"From\" Address");
  notifyMailConfigPanel.add(notifyMailFromPanel);
  notifyMailFromField = new FormField(8, true, this);
  notifyMailFromPanel.add(notifyMailFromField);
  notifyMailFromButton = new SetButton(this);
  notifyMailFromPanel.add(notifyMailFromButton);

  return notifyTab;
}


private void populateGUIElements() throws Exception {
  int intValue;
  int[] intArray;
  String stringValue;

  // Setup components
  if (reader.getAutoMode() == AlienClass1Reader.ON) {
    setupOnOffButton.setText("Automode is ON");
  } else {
    setupOnOffButton.setText("Automode is OFF");
  }

  // Wait components
  intValue = reader.getAutoWaitOutput();
  setCheckboxesFromIOValue(waitOutput1Checkbox, waitOutput2Checkbox, intValue);

  // Start components
  intArray = reader.getAutoStartTrigger();
  if ((intArray[0] == 0) && (intArray[1] == 0)) {
    // start immediately
    startImmediateRadio.setSelected(true);
    startInput1Checkbox.setEnabled(false);
    startInput2Checkbox.setEnabled(false);
    startRisingEdgeRadio.setEnabled(false);
    startFallingEdgeRadio.setEnabled(false);
  } else {
    if (intArray[1] == 0) {
      // rising edge
      intValue = intArray[0];
      startRisingEdgeRadio.setSelected(true);
    } else {
      // falling edge
      intValue = intArray[1];
      startFallingEdgeRadio.setSelected(true);
    }
    setCheckboxesFromIOValue(startInput1Checkbox, startInput2Checkbox, intValue);
    startTriggerRadio.setSelected(true);
    startInput1Checkbox.setEnabled(true);
    startInput2Checkbox.setEnabled(true);
    startRisingEdgeRadio.setEnabled(true);
    startFallingEdgeRadio.setEnabled(true);
  }

  // Work components
  intValue = reader.getAutoWorkOutput();
  setCheckboxesFromIOValue(workOutput1Checkbox, workOutput2Checkbox, intValue);
  stringValue = reader.getAutoAction();
  if (stringValue.equals("Acquire")) {
    workActionAcquireRadio.setSelected(true);
    workProgramIDField.setEnabled(false);
    workProgramIDButton.setEnabled(false);
  } else if (stringValue.equals("Program")) {
    workActionProgramRadio.setSelected(true);
    workProgramIDField.setEnabled(true);
    workProgramIDButton.setEnabled(true);
  } else if (stringValue.equals("Program And Lock")) {
    workActionProgramLockRadio.setSelected(true);
    workProgramIDField.setEnabled(true);
    workProgramIDButton.setEnabled(true);
    workProgramPasscodeField.setEnabled(true);
    workProgramPasscodeButton.setEnabled(true);
  } else if (stringValue.equals("Erase")) {
    workActionEraseRadio.setSelected(true);
    workProgramIDField.setEnabled(false);
    workProgramIDButton.setEnabled(false);
    workProgramPasscodeField.setEnabled(false);
    workProgramPasscodeButton.setEnabled(false);
  } else if (stringValue.equals("Kill")) {
    workActionKillRadio.setSelected(true);
    workProgramIDField.setEnabled(false);
    workProgramIDButton.setEnabled(false);
    workProgramPasscodeField.setEnabled(true);
    workProgramPasscodeButton.setEnabled(true);
  }

  // Stop components
  stopTimeField.setText("" + reader.getAutoStopTimer());
  intArray = reader.getAutoStopTrigger();
  if (intArray[1] == 0) {
    // rising edge
    intValue = intArray[0];
    stopRisingEdgeRadio.setSelected(true);
  } else {
    // falling edge
    intValue = intArray[1];
    stopFallingEdgeRadio.setSelected(true);
  }
  setCheckboxesFromIOValue(stopInput1Checkbox, stopInput2Checkbox, intValue);

  // Evaluation components
  intValue = reader.getAutoTrueOutput();
  setCheckboxesFromIOValue(evalTrueOutput1Checkbox, evalTrueOutput2Checkbox, intValue);
  evalTruePauseField.setText("" + reader.getAutoTruePause());
  intValue = reader.getAutoFalseOutput();
  setCheckboxesFromIOValue(evalFalseOutput1Checkbox, evalFalseOutput2Checkbox, intValue);
  evalFalsePauseField.setText("" + reader.getAutoFalsePause());

  // Notify components
  if (reader.getNotifyMode() == AlienClass1Reader.ON) {
    notifyOnOffButton.setText("Notify is ON");
  } else {
    notifyOnOffButton.setText("Notify is OFF");
  }
  notifyFormatPulldown.removeActionListener(this);
  notifyFormatPulldown.setSelectedItem(reader.getNotifyFormat());
  notifyFormatPulldown.addActionListener(this);
  notifyTriggerPulldown.removeActionListener(this);
  notifyTriggerPulldown.setSelectedItem(reader.getNotifyTrigger());
  notifyTriggerPulldown.addActionListener(this);
  notifyTimeField.setText("" + reader.getNotifyTime());
  stringValue = reader.getNotifyAddress();
  if (stringValue.endsWith("Serial)")) {
    notifySerialRadio.setSelected(true);
  } else if (stringValue.endsWith("Network)")) {
    notifyNetworkRadio.setSelected(true);
    String networkAddress = stringValue.substring(0, stringValue.indexOf(" "));
    String networkIPAddress = networkAddress.substring(0, networkAddress.indexOf(":"));
    String networkPort = networkAddress.substring(networkAddress.indexOf(":")+1);
    notifyNetworkAddressField.setText(networkIPAddress);
    notifyNetworkPortField.setText(networkPort);
  } else if (stringValue.endsWith("Mail)")) {
    notifyMailRadio.setSelected(true);
    String mailAddress = stringValue.substring(0, stringValue.indexOf(" "));
    notifyMailAddressField.setText(mailAddress);
  }
  notifyMailServerField.setText(reader.getMailServer());
  notifyMailFromField.setText(reader.getMailFrom());
}


public static void main(String[] args) {
  AutomodeSetup frame = new AutomodeSetup();
  frame.pack();
  frame.setVisible(true);
}

private class TabPanel extends JPanel {
  // subPanel holds the TabPanel's contents. It is squeezed into the NORTH area
  // of the TabPanel so that the components are as small as possible
  protected JPanel subPanel;

  public TabPanel(String title) {
    title = " " + title + " "; // Pad the title a bit
    setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEmptyBorder(),
        title,
        TitledBorder.CENTER,
        TitledBorder.CENTER,
        new Font("SansSerif", Font.BOLD, 18),
        Color.black)
    );
    setLayout(new BorderLayout());
    subPanel = new JPanel();
    subPanel.setLayout(new BoxLayout(subPanel, BoxLayout.PAGE_AXIS));
    add(subPanel, BorderLayout.NORTH);
  }

  public Component add(Component component) {
    subPanel.add(component);
    return component;
  }
}

private class ParameterGroup extends JPanel {
  public ParameterGroup(String title) {
    title = " " + title + " ";
    setBorder(BorderFactory.createTitledBorder(
          BorderFactory.createLineBorder(Color.black),
          title,
          TitledBorder.DEFAULT_JUSTIFICATION,
          TitledBorder.TOP)
    );
    setAlignmentX(Component.LEFT_ALIGNMENT);
  }
}

private class ComplexFormRow extends JPanel {
  public ComplexFormRow() {
    super(new FlowLayout(FlowLayout.LEFT, 0, 3));
    setAlignmentX(Component.LEFT_ALIGNMENT);
  }
}

private class FormField extends JTextField {
  public FormField(int numColumns, boolean isEnabled, ActionListener actionListener) {
    setColumns(numColumns);
    setEnabled(isEnabled);
    addActionListener(actionListener);
  }

  public int getInt() {
    return new Integer(getText()).intValue();
  }
}

private class SetButton extends JButton {
  public SetButton(ActionListener actionListener) {
    super("Set");
    addActionListener(actionListener);
    setFont(getFont().deriveFont(10f));
    setMargin(new Insets(0,3,0,3));
  }
}

} // End of class AutomodeTest