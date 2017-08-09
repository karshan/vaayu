#include <mios32.h>
#include "app.h"

#include <FreeRTOS.h>
#include <task.h>
#include <queue.h>

#define PinSet MIOS32_BOARD_J10_PinSet
#define PinGet MIOS32_BOARD_J10_PinGet

// Pins
#define LED1_PIN 0
#define LED2_PIN 1

// 74595
#define PO_SER 2
#define PO_OE_ 3
#define PO_RCLK 4
#define PO_SRCLK 5
#define PO_SRCLR_ 6

// 74165
#define PI_SHLD 7
#define PI_CLK 8
#define PI_Q 9

#define NUM_LEDS 128

// Buttons
#define NUM_ROWS 16 // Row 0 is bottom
#define NUM_COLS 8  // Col 0 is left

// forward decls
void write_leds(u8 state[NUM_LEDS][3]);
s32 led_col_row_to_number(s32 col, s32 row);
void read_buttons(u8 ret[NUM_COLS][NUM_ROWS/8]);
u8 get_button_val(s32 col, s32 row, u8 button_state[NUM_COLS][NUM_ROWS/8]);

// global state
u8 led_state[NUM_LEDS][3];
u8 button_state[NUM_COLS][NUM_ROWS/8];

typedef struct {
    int val;
    int saved_val;
    int debounce_t;
    int mode;
} enc_state_t;

enc_state_t encoder_state[32];

void init_led_state() {
  int i;
  for (i = 0; i < NUM_LEDS; i++) {
    led_state[i][0] = 0;
    led_state[i][1] = 0;
    led_state[i][2] = 0;
  }
}

void init_encoder_state() {
  int i;
  for (i = 0; i < 32; i++) {
    encoder_state[i].val = 0;
    encoder_state[i].saved_val = 0;
    encoder_state[i].debounce_t = 0;
    encoder_state[i].mode = 0;
  }
}

void APP_Init(void)
{
  // initialize all LEDs
  MIOS32_BOARD_LED_Init(0xffffffff);

  // Pinout:
  // J10: LED1 (top half) LED2 (bottom half)
  MIOS32_BOARD_J10_PinInit(LED1_PIN, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  MIOS32_BOARD_J10_PinInit(LED2_PIN, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);

  //      595-SER OE_bar RCLK SRCLK SRCLR_bar
  MIOS32_BOARD_J10_PinInit(PO_SER, MIOS32_BOARD_PIN_MODE_OUTPUT_OD);
  MIOS32_BOARD_J10_PinInit(PO_OE_, MIOS32_BOARD_PIN_MODE_OUTPUT_OD);
  MIOS32_BOARD_J10_PinInit(PO_RCLK, MIOS32_BOARD_PIN_MODE_OUTPUT_OD);
  MIOS32_BOARD_J10_PinInit(PO_SRCLK, MIOS32_BOARD_PIN_MODE_OUTPUT_OD);
  MIOS32_BOARD_J10_PinInit(PO_SRCLR_, MIOS32_BOARD_PIN_MODE_OUTPUT_OD);

  //      165-SH/LD_bar CLK Q
  MIOS32_BOARD_J10_PinInit(PI_SHLD, MIOS32_BOARD_PIN_MODE_OUTPUT_OD);
  MIOS32_BOARD_J10_PinInit(PI_CLK, MIOS32_BOARD_PIN_MODE_OUTPUT_OD);
  MIOS32_BOARD_J10_PinInit(PI_Q, MIOS32_BOARD_PIN_MODE_INPUT_PD);

  init_led_state();
  init_encoder_state();
}


void APP_Background(void)
{
}

void APP_MIDI_NotifyPackage(mios32_midi_port_t port, mios32_midi_package_t midi_package)
{
}


int encoder_transition(u8 n1, u8 n2, u8 o1, u8 o2) {
  //           n1n2
  // clockwise: 00 -> 10 -> 11 -> 01 -> 00
  // ccw      : 00 -> 01 -> 11 -> 10 -> 00
  u8 cw[4];
  u8 ccw[4];

  cw[0] = 2;
  cw[1] = 0;
  cw[2] = 3;
  cw[3] = 1;

  ccw[0] = 1;
  ccw[1] = 3;
  ccw[2] = 0;
  ccw[3] = 2;

  u8 n = n2 | (n1 << 1);
  u8 o = o2 | (o1 << 1);
  if (cw[o] == n) {
    return 1;
  } else if (ccw[o] == n) {
    return -1;
  } else {
    return 0;
  }
}

void color_wheel(u8 *rgb, int i) {
  if (i == 0) {
      rgb[0] = 16;
      rgb[1] = 0;
      rgb[2] = 0;
  } else if (i == 1) {
      rgb[0] = 32;
      rgb[1] = 0;
      rgb[2] = 0;
  } else if (i == 2) {
      rgb[0] = 32;
      rgb[1] = 16;
      rgb[2] = 0;
  } else if (i == 3) {
      rgb[0] = 16;
      rgb[1] = 16;
      rgb[2] = 0;
  } else if (i == 4) {
      rgb[0] = 0;
      rgb[1] = 16;
      rgb[2] = 0;
  } else if (i == 5) {
      rgb[0] = 0;
      rgb[1] = 32;
      rgb[2] = 0;
  } else if (i == 6) {
      rgb[0] = 0;
      rgb[1] = 32;
      rgb[2] = 16;
  } else { //if (i == 7) {
      rgb[0] = 0;
      rgb[1] = 16;
      rgb[2] = 16;
  } /*else if (i == 8) {
      rgb[0] = 0;
      rgb[1] = 0;
      rgb[2] = 16;
  } else if (i == 9) {
      rgb[0] = 0;
      rgb[1] = 0;
      rgb[2] = 32;
  } else if (i == 10) {
      rgb[0] = 16;
      rgb[1] = 0;
      rgb[2] = 32;
  } else if (i == 11) {
      rgb[0] = 16;
      rgb[1] = 0;
      rgb[2] = 16;
  } else {
      rgb[0] = 16;
      rgb[1] = 0;
      rgb[2] = 0;
  }*/
}

void encoder_color_1(u8 *rgb, int enc) {
  color_wheel(rgb, (enc + 256)/(512/8));
}

void encoder_color_2(u8 *rgb, int enc) {
  color_wheel(rgb, ((enc + 256)/(512/64)) % 8);
}

unsigned int _time = 0;
void APP_Tick()
{
  u8 new_buttons[NUM_COLS][NUM_ROWS/8];
  u8 led_flush = 0;

  if (1) {
    _time++;

    int i, j, enc_i;

    read_buttons(new_buttons);

    init_led_state();

    // buttons
    for (i = 0; i < NUM_COLS; i++) {
      for (j = 0; j < 8; j++) {
        u8 new_val = get_button_val(i, j, new_buttons);
        u8 old_val = get_button_val(i, j, button_state);

        if (j % 2 == 1) {
          enc_i = (j/2)*8 + i;
          if (encoder_state[enc_i].mode == 0) {
            led_state[led_col_row_to_number(i, j)][0] = 0;
            led_state[led_col_row_to_number(i, j)][1] = 0;
            led_state[led_col_row_to_number(i, j)][2] = 64;
            if (new_val) {
              encoder_state[enc_i].mode = 1;
              encoder_state[enc_i].debounce_t = _time;
            }
          } else if (encoder_state[enc_i].mode == 1) {
            led_state[led_col_row_to_number(i, j)][0] = 64;
            led_state[led_col_row_to_number(i, j)][1] = 64;
            led_state[led_col_row_to_number(i, j)][2] = 0;
            if (new_val == 0 && 
                  (_time > encoder_state[enc_i].debounce_t + 10 || _time < encoder_state[enc_i].debounce_t)) {
              encoder_state[enc_i].saved_val = encoder_state[enc_i].val;
              encoder_state[enc_i].mode = 2;
            }
          } else if (encoder_state[enc_i].mode == 2) {
            led_state[led_col_row_to_number(i, j)][0] = 0;
            led_state[led_col_row_to_number(i, j)][1] = 64;
            led_state[led_col_row_to_number(i, j)][2] = 0;
            if (new_val) {
              encoder_state[enc_i].mode = 3;
              encoder_state[enc_i].debounce_t = _time;
            }
          } else if (encoder_state[enc_i].mode == 3) {
            led_state[led_col_row_to_number(i, j)][0] = 64;
            led_state[led_col_row_to_number(i, j)][1] = 64;
            led_state[led_col_row_to_number(i, j)][2] = 0;
            if (new_val == 0 &&
                  (_time > encoder_state[enc_i].debounce_t + 10 || _time < encoder_state[enc_i].debounce_t)) {
              encoder_state[enc_i].mode = 4;
            }
          } else if (encoder_state[enc_i].mode == 4) {
            int step = 4;
            led_state[led_col_row_to_number(i, j)][0] = 64;
            led_state[led_col_row_to_number(i, j)][1] = 0;
            led_state[led_col_row_to_number(i, j)][2] = 0;
            if (encoder_state[enc_i].saved_val == encoder_state[enc_i].val) {
              encoder_state[enc_i].mode = 0;
            } else if (encoder_state[enc_i].saved_val > encoder_state[enc_i].val) {
              if (encoder_state[enc_i].saved_val - encoder_state[enc_i].val <= step) {
                encoder_state[enc_i].val = encoder_state[enc_i].saved_val;
              } else {
                encoder_state[enc_i].val += step;
              }
              MIOS32_MIDI_SendCC(DEFAULT, Chn1, enc_i, (encoder_state[enc_i].val/4) + 63);
            } else {
              if (encoder_state[enc_i].val - encoder_state[enc_i].saved_val <= step) {
                encoder_state[enc_i].val = encoder_state[enc_i].saved_val;
              } else {
                encoder_state[enc_i].val -= step;
              }
              MIOS32_MIDI_SendCC(DEFAULT, Chn1, enc_i, (encoder_state[enc_i].val/4) + 63);
            }
          }
        } else {
          if (new_val != old_val) {
            MIOS32_MIDI_SendNoteOn(DEFAULT, Chn1, j*NUM_COLS + i, new_val ? 0x7f : 0);
          }
          led_state[led_col_row_to_number(i, j)][0] = new_val * 16;
        }

      }
    }

    // encoders
    for (i = 0; i < NUM_COLS; i++) {
      for (j = 8; j < NUM_ROWS; j += 2) {
        enc_i = ((j - 8)/2)*8 + i;
        u8 new_val_1 = get_button_val(i, j, new_buttons);
        u8 new_val_2 = get_button_val(i, j + 1, new_buttons);

        u8 old_val_1 = get_button_val(i, j, button_state);
        u8 old_val_2 = get_button_val(i, j + 1, button_state);

        if (new_val_1 != old_val_1 || new_val_2 != old_val_2) {
          if (get_button_val(0, 0, button_state)) {
            encoder_state[enc_i].val += encoder_transition(new_val_1, new_val_2, old_val_1, old_val_2);
          } else {
            encoder_state[enc_i].val += encoder_transition(new_val_1, new_val_2, old_val_1, old_val_2) * 4;
          }

          if (encoder_state[enc_i].val > 256) {
            encoder_state[enc_i].val = 256;
          } else if (encoder_state[enc_i].val < -252) {
            encoder_state[enc_i].val = -252;
          }
          MIOS32_MIDI_SendCC(DEFAULT, Chn1, enc_i, (encoder_state[enc_i].val/4) + 63);
        }

        encoder_color_1(led_state[led_col_row_to_number(i, j)], encoder_state[enc_i].val);
        encoder_color_2(led_state[led_col_row_to_number(i, j + 1)], encoder_state[enc_i].val);
      }
    }


    for (i = 0; i < NUM_COLS; i++) {
      for (j = 0; j < NUM_ROWS/8; j++) {
        button_state[i][j] = new_buttons[i][j];
      }
    }

    write_leds(led_state);
  }
}

inline void button_delay() {
}

// Button Section
u8 get_button_val(s32 col, s32 row, u8 button_state[NUM_COLS][NUM_ROWS/8]) {
  return (button_state[col][row/8] & (1 << (row % 8))) ? 1 : 0;
}

void po_clk() {
  PinSet(PO_SRCLK, 1);
  button_delay();
  PinSet(PO_SRCLK, 0);
  button_delay();

  PinSet(PO_RCLK, 1);
  button_delay();
  PinSet(PO_RCLK, 0);
  button_delay();
}

void read_current_col(u8 ret[NUM_ROWS/8]);
void read_buttons(u8 ret[NUM_COLS][NUM_ROWS/8]) {
  int i;
  PinSet(PO_OE_, 0);
  PinSet(PO_SRCLK, 0);
  PinSet(PO_RCLK, 0);

  PinSet(PO_SRCLR_, 0);
  button_delay();
  PinSet(PO_SRCLR_, 1);

  PinSet(PO_SER, 1);
  button_delay();
  po_clk();

  for (i = 0; i < NUM_COLS; i++) {
    read_current_col(ret[i]);

    PinSet(PO_SER, 0);
    button_delay();
    po_clk();
    //MIOS32_DELAY_Wait_uS(50000);
  }
}

// XXX By PCB design assume NUM_ROWS % 8 == 0
void read_current_col(u8 ret[NUM_ROWS/8]) {
  int i, j;
  PinSet(PI_CLK, 0);

  PinSet(PI_SHLD, 0);
  button_delay();
  PinSet(PI_SHLD, 1);
  button_delay();
 
  for (j = 0; j < NUM_ROWS/8; j++) {
    ret[j] = 0;
    for (i = 0; i < 8; i++) {
      ret[j] |= (PinGet(PI_Q) ? 1 : 0) << i;

      PinSet(PI_CLK, 1);
      button_delay();
      PinSet(PI_CLK, 0);
      button_delay();
    }
  }
}

// LED Section
s32 led_col_row_to_number(s32 col, s32 row) {
  // invert row because row 0 is bottom
  row = 15 - row;

  s32 half_offset = (row >= 8) ? 64 : 0;
  s32 col_offset = (col/2) * 16;
  s32 row_offset = (row % 8) * 2;
  return half_offset + col_offset + row_offset + (col % 2);
}

inline void ws2812_low(u8 pin) {
  PinSet(pin, 1);
asm volatile("nop\n\tnop\n\t");

  PinSet(pin, 0);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

}

inline void ws2812_high(u8 pin) {
  PinSet(pin, 1);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

  PinSet(pin, 0);
asm volatile("nop\n\tnop\n\t");
}

inline void ws2812_shiftOut(u8 a, u8 pin) {
  int i;
  for (i = 7; i >= 0; i--) {
    if (a & (1 << i)) {
      ws2812_high(pin);
    } else {
      ws2812_low(pin);
    }
  }
}

void write_leds(u8 state[NUM_LEDS][3]) {
  int i;
  __disable_irq();
  for (i = 0; i < NUM_LEDS/2; i++) {
    ws2812_shiftOut(state[i][0], 0);
    ws2812_shiftOut(state[i][1], 0);
    ws2812_shiftOut(state[i][2], 0);
  }
  MIOS32_BOARD_J10_PinSet(0, 0);

  for (i = NUM_LEDS/2; i < NUM_LEDS; i++) {
    ws2812_shiftOut(state[i][0], 1);
    ws2812_shiftOut(state[i][1], 1);
    ws2812_shiftOut(state[i][2], 1);
  }
  MIOS32_BOARD_J10_PinSet(1, 0);
  __enable_irq();
}
