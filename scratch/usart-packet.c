#include <stdio.h>
#include "usart-packet.h"

#ifdef PIC
#define PRINTF_TYPECASE (const rom far char *)
#define USART_OUT stdout
#define USART_IN stdin
#else
#define PRINTF_TYPECASE
#define USART_OUT stdout
#define USART_IN stdin
#endif

#define CRC16_UPDATE(crc, new_byte) {            \
  crc  = (unsigned char)(crc >> 8) | (crc << 8); \
  crc ^= new_byte;                               \
  crc ^= (unsigned char)(crc & 0xff) >> 4;       \
  crc ^= (crc << 8) << 4;                        \
  crc ^= ((crc & 0xff) << 4) << 1;               \
}

// ur_packet == usart-receive-packet
struct packet ur_packet;

// Having a single state means that we can only receive packets on one
// input channel at a time.
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
  fprintf(USART_OUT, PRINTF_TYPECASE"NAK\n");
}

// Tells sender that the previous block has been received successfully
void ur_keep_sending(void) {
  fprintf(USART_OUT, PRINTF_TYPECASE"ACK\n");
}

// Extract high and low bytes of an unsigned int
#define HIGH(uint) ((uint) >> 8)
#define LOW(uint) ((uint) & 0xFF)

// Calculate the check field of the packet from length and id
#define CALCULATE_CHECK(length, id) (((length) ^ HIGH(id)) ^ LOW(id))

// Handle a single incoming byte on the USART. This uses a nasty
// finite-state machine to do its nasty business.
void ur_byte(unsigned char byte) {
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

  printf(PRINTF_TYPECASE"Packet received:\n  id:     %x\n  length: %hi\n  data:   ", ur_packet.id, ur_packet.length);
  for (i = 0; i < ur_packet.length; i++)
    printf(PRINTF_TYPECASE"%x ", ur_packet.data[i]);
  printf(PRINTF_TYPECASE"\n  CRC16:  %x\n\n", ur_packet.crc);
  ur_keep_sending();
}


// OUTPUT (sending)

void usart_send_byte(unsigned char byte) {
  fputc(byte, USART_OUT);
  
}

unsigned char keep_sending_p() {
  char response[3];

  fgets(response, 3, USART_IN);
  if (strncmp(response, "ACK", 3)) return 1;
  else return 0;
}

void usart_send_packet(struct packet *p) {
  unsigned int crc; unsigned char i, byte;
  
 resend_packet:
  fprintf(stderr, "Sending packet %i\n", p->id);
  crc = 0xFFFF;
  
  // Send id, length, check
  usart_send_byte(HIGH(p->id));
  usart_send_byte(LOW(p->id));
  usart_send_byte(p->length);
  usart_send_byte(p->length ^ HIGH(p->id) ^ LOW(p->id));

  if (!keep_sending_p()) goto resend_packet;
  
  // Send data
  for (i=0; i < p->length; i++) {
    byte = p->data[i];
    usart_send_byte(byte);
    CRC16_UPDATE(crc, byte);
  }

  // Send CRC
  usart_send_byte(HIGH(crc));
  usart_send_byte(LOW(crc));
  if (!keep_sending_p()) goto resend_packet;
}
    

int main(void) {
  static unsigned int crc = 0xFFFF;
  static unsigned char crc8 = 0xFF;
  unsigned char byte, i;
  
  /* OpenUSART(USART_TX_INT_OFF  & */
/* 	    USART_RX_INT_OFF  & */
/* 	    USART_ASYNCH_MODE & */
/* 	    USART_EIGHT_BIT   & */
/* 	    USART_CONT_RX     & */
/* 	    USART_BRGH_HIGH, 25); */
  
  #ifdef RECEIVE
  fprintf(stderr, "Beginning recv\n");
  ur_begin();
  while (1) {
    byte = fgetc(USART_IN);
    fprintf(stderr, "Received byte %x\n", byte);
    ur_byte(byte);
  }
  #else
  fprintf(stderr, "Beginning send\n");
  for (i=0; i < 5; i++) {
    ur_packet.id = i;
    ur_packet.length = 4;
    ur_packet.data[0] = i;
    ur_packet.data[1] = i+1;
    ur_packet.data[2] = i+2;
    ur_packet.data[3] = i+3;
    usart_send_packet(&ur_packet);
    sleep(1);
  }
  #endif
    
  // id
 /*  ur_byte(0x12); ur_byte(0x34); */
/*   // length */
/*   ur_byte(3); */
/*   // check */
/*   ur_byte(CALCULATE_CHECK(3, 0x1234)); */
/*   // data */
/*   ur_byte(0x42); ur_byte(0x69); ur_byte(0xad); */
/*   // crc */
/*   CRC16_UPDATE(crc, 0x42); */
/*   CRC16_UPDATE(crc, 0x69); */
/*   CRC16_UPDATE(crc, 0xad); */
/*   ur_byte(HIGH(crc)); */
/*   ur_byte(LOW(crc)); */

  //  while (1);
}
