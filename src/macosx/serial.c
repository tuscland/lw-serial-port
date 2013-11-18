// From: http://stackoverflow.com/questions/6947413/how-to-open-read-and-write-from-serial-port-in-c

#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <IOKit/serial/ioss.h>

int
serial_set_options(int fd, int speed, int parity)
{
    struct termios tty;
    memset (&tty, 0, sizeof tty);
    if (tcgetattr (fd, &tty) != 0) {
        fprintf (stderr, "error %d from tcgetattr", errno);
        return -1;
    }

    cfsetospeed (&tty, speed);
    cfsetispeed (&tty, speed);

    tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS8;     // 8-bit chars
    // disable IGNBRK for mismatched speed tests; otherwise receive break
    // as \000 chars
    tty.c_iflag &= ~IGNBRK;         // ignore break signal
    tty.c_lflag = 0;                // no signaling chars, no echo,
                                    // no canonical processing
    tty.c_oflag = 0;                // no remapping, no delays
    tty.c_cc[VMIN]  = 1;            // blocking read
    tty.c_cc[VTIME] = 0;            // 0.0 second timeout

    tty.c_iflag &= ~(IXON | IXOFF | IXANY); // shut off xon/xoff ctrl

    tty.c_cflag |= (CLOCAL | CREAD);// ignore modem controls,
                                    // enable reading
    tty.c_cflag &= ~(PARENB | PARODD);      // shut off parity
    tty.c_cflag |= parity;
    tty.c_cflag &= ~CSTOPB;
    tty.c_cflag &= ~CRTSCTS;

    if (tcsetattr (fd, TCSANOW, &tty) != 0) {
        fprintf (stderr, "error %d from tcsetattr", errno);
        return -1;
    }
    return 0;
}

size_t
serial_sizeof_struct_termios() {
    return sizeof(struct termios);
}

int
serial_open(char *portname, struct termios *original_port_attributes)
{
    int success = 0;

    // receive latency ( in microseconds )
    unsigned long mics = 1;

    int fd = open(portname, O_RDWR | O_NOCTTY | O_NONBLOCK);
    if (fd < 1) {
        return fd;
    }

    // Save original serial port attributes
    tcgetattr(fd, original_port_attributes);

    // Exclusive access to serial device
    success = ioctl(fd, TIOCEXCL);

    if (success != -1) {    
        if (success != -1) {
            success = ioctl(fd, IOSSDATALAT, &mics);
        }
    }

    if (success == -1) {
        close(fd);
        return -1;
    }

    return fd;
}

int
serial_close(int fd, struct termios *original_port_attributes) {
    tcflush(fd, TCIOFLUSH);
    tcsetattr(fd, TCSANOW, original_port_attributes);
    close(fd);
    return 0;
}

static int write_max_idle_secs = 0;
static int write_max_idle_usecs = 100000; // 100 ms

#define IO_RETRY  0
#define IO_ERROR -1

int
serial_wait_for_input(int fd, int timeout_s, int timeout_us) {
    struct timeval timeout = { 0, 0 };
    struct fd_set set;
    FD_ZERO(&set);
    FD_SET(fd, &set);
    timeout.tv_sec = timeout_s;
    timeout.tv_usec = timeout_us;
    ssize_t i = select(fd+1, &set, NULL, NULL, &timeout);
    if (i < 0 && errno == EINTR) {
        return IO_RETRY;
    }
    return i;
}

int
serial_read(int fd, void *buf, size_t nbyte) {
    ssize_t i = read(fd, buf, nbyte);
    if (i < 0) {
        if (errno == EINTR || errno == EAGAIN) {
            return IO_RETRY;
        } else {
            // some real error; log/die/whatever and close() socket
            return IO_ERROR;
        }
    } else if (i == 0) {
        // the connection has been closed by your peer; clean-up and close()
        return IO_ERROR;
    }
    return i;
}

int
serial_write(int fd, void *buf, size_t nbyte) {
    // see http://developerweb.net/viewtopic.php?pid=31380
    struct fd_set set;
    struct timeval timeout;
    ssize_t i = 0;
    ssize_t written_so_far = 0;

    for (;;) {
        i = write(fd, buf, nbyte);
        if (i < 0) {
            if (errno == EINTR) {
                continue; // TODO: check for infinite loops.
            } else if (errno == EAGAIN) {
                FD_ZERO(&set);
                FD_SET(fd, &set);
                timeout.tv_sec = write_max_idle_secs;
                timeout.tv_usec = write_max_idle_usecs;
                i = select(fd+1, &set, NULL, NULL, &timeout);
                if (i < 0) {
                    /* error; log/die/whatever and close() socket */
                    if (errno == EINTR) {
                        continue; // TODO: check for infinite loops.
                    } else {
                        return IO_ERROR;
                    }
                } else if (i == 0) {
                    /* timed out without sending any data; log/die/whatever and close() */
                    return 0;
                }
                /* else, socket is now writable, so loop back up and do the write() again */
            } else {
                /* some real error; log/die/whatever and close() socket */
                return IO_ERROR;
            }
        } else if (i == 0) {
            /* the connection has been closed by your peer; clean-up and close() */
            return IO_ERROR;
        } else {
            if (i <= nbyte) {
                nbyte -= i;
                buf += i;
                written_so_far += i;
                /* now loopback to finish writing the rest */
                // TODO: check for infinite loops.
            }
            if (i >= nbyte) {
                return written_so_far;
            }
        }
    }

    // Should never go there
    printf("Unexpected error in serial_write\n");
    return IO_ERROR;
}

typedef void serial_enumerate_device_names_callback(const char *name);

#include <IOKit/IOKitLib.h>
#include <IOKit/serial/IOSerialKeys.h>
#include <IOKit/IOBSD.h>
#include <stdlib.h>

void
serial_enumerate_device_names (serial_enumerate_device_names_callback *callback) {
    io_object_t serialPort;
    io_iterator_t serialPortIterator;

// ask for all the serial ports
    IOServiceGetMatchingServices(kIOMasterPortDefault,
       IOServiceMatching(kIOSerialBSDServiceValue),
       &serialPortIterator);

// loop through all the serial ports and add them to the array
    while ((serialPort = IOIteratorNext(serialPortIterator))) {
        CFStringRef string = IORegistryEntryCreateCFProperty(serialPort, CFSTR(kIOCalloutDeviceKey),  kCFAllocatorDefault, 0);
        CFIndex length = CFStringGetLength(string);
        CFIndex maxSize = CFStringGetMaximumSizeForEncoding(length, kCFStringEncodingUTF8);
        char *buffer = (char *)malloc(maxSize);
        if (CFStringGetCString(string, buffer, maxSize, kCFStringEncodingUTF8)) {
            callback(buffer);
        }
        free(buffer);
        CFRelease(string);
        IOObjectRelease(serialPort);
    }

    IOObjectRelease(serialPortIterator);
}
