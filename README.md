SpeedyLogger Zero touch Speeduino Data Logger
=============================================

SpeedyLogger is a bare metal application which runs on the Raspberry Pi. It connects to the Speeduino engine management ECU's Serial3 port, and automatically logs real-time engine data to the SD card. It will probably work on a pi 1 if needed, although some minor changes would be nedeed to the source to include the right libraries. On a Pi 2, boot time is about 1 second, after which logging will begin assuming the serial line is connected to the Arduino.

This software is very much an Alpha release. No guarantees as to how effective it will be for you but I will do my best to help anyone who has issues getting it going. See the 'what's coming next' section for known limitations.

Hardware Installation
=====================

You should engineer your installation so that power is applied to the Raspberry Pi at the same time as the speeduino ECU. The Pi takes about one second to power up, and as soon as it is ready it will connect to the Speeduino so the Mega needs to be powered on at that time. SpeedyLogger will keep trying to connect until it is successful at a rate of once every 2 seconds.

Powering Off
============

It is safe to remove the power from the Pi without any shutdown sequence, even when logging.

Serial Connection
=================

For this application to work you must connect the serial 3 port on your Arduino Mega to the UART on the Raspberry Pi. The pin headers differ between models so you should ensure that you are using the right pins on the Raspberry Pi. There are plenty of guides on the internet if you do not already have this information.

You must also enable the secondary serial support in your Speeduino tune using TunerStudio. This can be found in the accessories menu under the CanBus / Secondary Serial Interface option. Choose enable in the dialog. 

BEWARE!! The arudino MEGA 2560 which your speeduino is running on uses a 5V signal on its UART. Since the Raspberry Pi is a 3.3v device, connecting the transmit pin from the Arduino directly to the receive pin on the Pi will damage the input on the Raspberry Pi. In order to avoid this you must use a level converter between the Arduino and the Pi. This can be done with a simple voltage divider, of which there are many examples on the web. Alternatively you can purchase a proper level converter board to convert between the 3.3v and 5v devices. You only need to protect the RX (Receive) pin on the Pi. The arduino will be fine receiving 3.3v as logic '1'.

Supported Pi Models
===================

Raspberry Pi Zero

Raspberry Pi 1

Raspberry Pi 2

Raspberry Pi 3


I have not been able to extensively test all variations (A / A+ / B etc) so you may have to report an issue if something does not work.

I will add support for Raspberry Pi 4 as soon as Ultibo supports it.

Software Installation
=====================

You do not need a compiler to install the software, as the binaries are already present in this repository in the following location;

source/bin/rpi1

source/bin/zero

source/bin/rpi2

source/bin/rpi3

Obviously, choose the correct folder according to which version of the Pi you have.

In order to install the software you will need a FAT32 formatted SD card to go in your Pi. SpeedyLogger does not run on Raspian or any other operating system, so do not try and use a card with any OS already installed unless you are happy to lose it, as this installation process will delete the contents to the extent that it won't boot the OS that was present (even if you skip the format step). 

1. Format your SD card to FAT32
2. Download this repository using either git clone or download in a zip file from the link above (top right of the page just above the source folders).
3. Unzip as needed and navigate to the correct folder for the version of the Pi you have, as above.
4. Copy all of the files from that folder to the root of the SD Card you formatted in step 1 earlier.
5. Insert the SD card into your Pi, and power it on. The logger should load immediately and start working (assuming you have connected the serial port first).

Log Files
=========

Once up and running, SpeedyLogger will begin logging as soon as it can establish a serial connection to your Speeduino board. You can check that it is running using one of two methods:

1. Connect your Pi to an HDMI monitor and check the console. It should contain a few messages demonstrating that it is up and running and be displaying a few key data values on the screen. These values are displayed for every message received from the Pi.
2. Look at the status LED on the board; it should be flashing on and off at approximately 250ms intervals (on a RPi2 it's a green LED) if connected, and 100ms intervals if not connected to the Serial 3 port. The flash rate can be a bit inconsistent depending on load as it does not use a hardware timer.

Once connected, SpeedyLogger will start logging data to a log file which will be named 'dl<<number>>.msl' where <<number>> is a zero padded number of 6 digits in length. Each time a connection is lost, or the system is restarted, logging will move to the next log file in the sequence. The next log file number is stored in the file "datalog.inf".

For diagnostic and bench testing purposes the list of log files present on the card can be viewed by connecting to the Pi over a network. Only wired networks are supported at present. Once connected to the network, you will need a telnet client such as puTTY. Telnet to the IP address of your Pi and you will see a command prompt. Type 'help datalog' to see the list of *relevant* commands available (i.e. there are others, but that is beyond the scope of this readme). To find the IP address of your Pi, use the admin pages of your router. The commands available are as follows:

dir - list the files on the card

type <filename>  - dump the file to the screen. If you are logging when you do this, the logging will definitely be interrupted. Use datalog pause first.

datalog pause - pause data logging.

datalog resume - resume logging after a pause.

datalog next - jump to the next datalog in the sequence (e.g. if on dl000006.msl then move to dl000007.msl).

datalog cleanup - delete all log files on the disk except the currently running log

datalog reset - put the datalog number back to 0. This may result in the log file number immediately moving on to number 1 as it usually has the effect of interrupting the serial connection. 


Disk usage: To give you some idea of the size of card you need, 1 minute of data is around 85kb but this may vary between installations.

if you are not connected to a network, then to view or access the files you must remove the card from the Pi. You should only remove the card when the power is off.

What's Coming Next?
===================

As this is alpha software, there is plenty to do to make it work better. At the moment for example, not all of the fields are being properly inerpreted if they are being scaled in some way. I'll probably extend it to read the speeduino .ini file so that items can be processed more accurately. This will mean you'll have to copy that file to the card during installation in the future, and keep it updated inline with the firmware version you are using.

Sometimes the management of the log files falls a bit short. If you add some demand (usually from the telnet terminal) that puts too much load on the device, then it may start skipping log files leaving multiple files with small amounts of data in them. So work on this is needed to recognise when the load is too high and back off a bit. This is more of a bench testing problem than a real-world problem though.

The long term objective is to produce a grahical dash that is instant on much like the OEM digital dashboards in current vehicles, whilst obviously retaining the logging capability. This is a long way off yet, although some work has been done on this already using some rudimentary graphics. This is the real reason for using a Pi rather than just an Arduino with an SD card attached, since it offers HDMI output out of the box and has video acceleration built in.

Contributing or Changing the source
===================================

SpeedyLogger is built with the Ultibo platform. Please see www.ultibo.org for further details on this system. It uses Lazarus as the development environment, which is available on both Windows and Linux. The lazarus implementation is specific to Ultibo as it has some Ultibo specific extensions. On Windows there is an installer which will get you up and running quickly, but on Linux the environment must be compiled from source. On my Ubuntu installation I was unable to get this to work due to the need for some missing 32 bit tools which I could not find, so the current recommendation is to use Windows as the developnent environment.   If anyone has success getting Linux up and running do please let me know how! You may have some luck with help on the Ultibo forums should you need it (I have not asked, I just took the path of least resistance and used Windows!).


What is Speeduino
=================

In case you found this page on github through serendipity, the Speeduino project is an open source engine management system built by Josh Stewart. You can find this system on github here:

https://github.com/noisymime/speeduino

See also www.speeduino.com and www.speeduino.com/forum

