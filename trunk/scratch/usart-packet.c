#include <stdio.h>

void ur_begin(void);
void ur_error(void);
void ur_keep_sending(void);
void ur_byte(static unsigned char byte);
void ur_packet_finished(void);

/* There should be one of these packet structures for each packet you
   wish to send or receive. That is, if you wish to send and receive
   on the USART, that requires two packets. If you also want to
   receive via SPI, that requires another packet. */
struct packet {
  // Used for dispatching to handlers
  unsigned int id;
  // Should be between 0 and 31 inclusive
  unsigned char length;
  // The data. It's a little on the large side.
  unsigned char data[32];
  // The CRC-16 checksum
  unsigned int crc;
};

// ur_packet == usart-receive-packet
struct packet ur_packet;

struct {
  unsigned length_counter:8;
  unsigned have_idh:1;
  unsigned have_idl:1;
  unsigned have_length:1;
  unsigned have_check:1;
  unsigned have_data:1;
  unsigned have_crch:1;
  unsigned crc_okay:1;
  unsigned :2;
} ur_state;

// Clear state and get ready to receive packet
void ur_begin(void) {
  *(unsigned int *)&ur_state = 0;
  ur_packet.crc = 0xFFFF;
}

// Called when the packet is corrupted. Asks sender to resend the
// packet.
void ur_error(void) {
  ur_begin();
  printf((const rom far char *)"NAK\n");
}

// Tells sender that the previous block has been received successfully
void ur_keep_sending(void) {
  printf((const rom far char *)"ACK\n");
}

// Extract high and low bytes of an unsigned int
#define HIGH(uint) ((uint) >> 8)
#define LOW(uint) ((uint) & 0xFF)

// Calculate the check field of the packet from length and id
#define CALCULATE_CHECK(length, id) (((length) ^ HIGH(id)) ^ LOW(id))

// Handle a single incoming byte on the USART. This uses a nasty
// finite-state machine to do its nasty business.
void ur_byte(static unsigned char byte) {
  if (!ur_state.have_idh) {
    ur_packet.id = (unsigned int)byte << 8;
    ur_state.have_idh = 1;
  } else if (!ur_state.have_idl) {
	*(unsigned char *)(&ur_packet.id) = byte;
    ur_state.have_idl = 1;
  } else if (!ur_state.have_length) {
    // Make sure that length is 31 or less
    ur_packet.length = byte & 31;
    ur_state.have_length = 1;
  } else if (!ur_state.have_check) {
    // If check isn't what it ought to be, signal an error
    if (byte != CALCULATE_CHECK(ur_packet.length, ur_packet.id))
      ur_error();
    else {
      ur_keep_sending();
      ur_state.have_check = 1;
    }
  } else if (!ur_state.have_data) {
    ur_packet.data[ur_state.length_counter++] = byte;
    CRC16_UPDATE(ur_packet.crc, byte);

    if (ur_state.length_counter >= ur_packet.length)
      ur_state.have_data = 1;
  } else if (!ur_state.have_crch) {
    if (HIGH(ur_packet.crc) == byte) ur_state.crc_okay = 1;
    ur_state.have_crch = 1;
  } else {
    if (!ur_state.crc_okay || (LOW(ur_packet.crc) != byte))
      ur_error();
    else ur_packet_finished();
  }
}

void ur_packet_finished(void) {
  unsigned char i;

  printf((const rom far char *)"Packet received:\n  id:     %x\n  length: %hi\n  data:   ", ur_packet.id, ur_packet.length);
  for (i = 0; i < ur_packet.length; i++)
    printf((const rom far char *)"%x ", ur_packet.data[i]);
  printf((const rom far char *)"\n  CRC16:  %x\n\n", ur_packet.crc);
  ur_keep_sending();
}

void main(void) {
  static unsigned int crc = 0xFFFF;
  static unsigned char crc8 = 0xFF;
  
  OpenUSART(USART_TX_INT_OFF  &
	    USART_RX_INT_OFF  &
	    USART_ASYNCH_MODE &
	    USART_EIGHT_BIT   &
	    USART_CONT_RX     &
	    USART_BRGH_HIGH, 25);
  
  ur_begin();
  // id
  ur_byte(0x12); ur_byte(0x34);
  // length
  ur_byte(3);
  // check
  ur_byte(CALCULATE_CHECK(3, 0x1234));
  // data
  ur_byte(0x42); ur_byte(0x69); ur_byte(0xad);
  // crc
  CRC16_UPDATE(crc, 0x42);
  CRC16_UPDATE(crc, 0x69);
  CRC16_UPDATE(crc, 0xad);
  ur_byte(HIGH(crc));
  ur_byte(LOW(crc));

  while (1);
}
