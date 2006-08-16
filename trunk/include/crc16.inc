// -*-C-*-

/* This code implements CRC-16-CCITT, using X^16 + X^12 + X^5 + 1 as
   the polynomial. It takes 90 clock cycles per byte. The initial CRC
   value should be 0xFFFF if you want to get ordinary return values,
   but this shouldn't make much difference to the strength of the
   CRC. */

// Update the variable crc with a new byte called new_byte
#define CRC16_UPDATE(crc, new_byte) {  	       	 \
  crc  = (unsigned char)(crc >> 8) | (crc << 8); \
  crc ^= new_byte;				 \
  crc ^= (unsigned char)(crc & 0xff) >> 4;	 \
  crc ^= (crc << 8) << 4;			 \
  crc ^= ((crc & 0xff) << 4) << 1;		 \
}
