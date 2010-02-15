package net.fortytwo.homework.operatingsystems.scheduling;

import java.util.LinkedList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Sep 18, 2008
 * Time: 2:22:30 PM
 * To change this template use File | Settings | File Templates.
 */
public class SchedulingSimulator {

    /*
Consider a computer system with 10 processes in the ready queue, numbered from 0 to 9.
The burst time of each process is tBi=1.2ms if it is an odd one-digit number<8, tBi=2.2ms
if i is even, and tB9=42ms. After executing for the burst time, each process goes into disk
service that takes time tD. In each question below consider three values of tD equal to 2,
10, 30ms, corresponding to a fast, medium and (very!) slow disk, respectively. The
context switching time Ts is 0.2ms.
     */
    public static void main(final String[] args) throws Exception {

        List<Process> initialProcesses = new LinkedList<Process>();

        float[] diskServiceTimes = {2.0f, 10.0f, 30.0f};

        for (float diskServiceTime : diskServiceTimes) {
            for (int i = 0; i < 10; i++) {
                float burstTime = (0 == i % 2)
                        ? 2.2f
                        : (9 == i)
                        ? 42.0f
                        : 1.2f;
                
                Process.Type type = (9 == i)
                        ? Process.Type.COMPUTATIONAL
                        : Process.Type.INTERACTIVE;

                String name = "" + i;

                Process p = new Process(type, burstTime, diskServiceTime, name);
                initialProcesses.add(p);
            }

            ReadyQueue q = new ReadyQueue(initialProcesses);

            //...
        }
    }
}
