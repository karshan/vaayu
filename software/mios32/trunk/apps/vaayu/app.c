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
void button_test();

// global state
u8 led_state[NUM_LEDS][3];
u8 button_state[NUM_COLS][NUM_ROWS/8];

void init_led_state() {
  int i;
  for (i = 0; i < NUM_LEDS; i++) {
    led_state[i][0] = 0;
    led_state[i][1] = 0;
    led_state[i][2] = 0;
  }
}

void APP_Init(void)
{
  // initialize all LEDs
  MIOS32_BOARD_LED_Init(0xffffffff);

  // Pinout:
  // J10: LED1 (top half) LED2 (bottom half)
  MIOS32_BOARD_J10_PinInit(0, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  MIOS32_BOARD_J10_PinInit(1, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);

  //      595-SER OE_bar RCLK SRCLK SRCLR_bar
  MIOS32_BOARD_J10_PinInit(2, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  MIOS32_BOARD_J10_PinInit(3, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  MIOS32_BOARD_J10_PinInit(4, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  MIOS32_BOARD_J10_PinInit(5, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  MIOS32_BOARD_J10_PinInit(6, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);

  //      165-SH/LD_bar CLK Q
  MIOS32_BOARD_J10_PinInit(7, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  MIOS32_BOARD_J10_PinInit(8, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  MIOS32_BOARD_J10_PinInit(9, MIOS32_BOARD_PIN_MODE_INPUT_PD);

  init_led_state();
  button_test();
}


void APP_Background(void)
{
}

void APP_MIDI_NotifyPackage(mios32_midi_port_t port, mios32_midi_package_t midi_package)
{
}

int _time = 0;
void APP_Tick()
{

  // TODO only write leds on change
  if (_time++ % 1000 == 0) {
#if 0
    int i, j;
    read_buttons(button_state);

    init_led_state();
    for (i = 0; i < NUM_COLS; i++) {
      for (j = 0; j < NUM_ROWS; j++) {
        led_state[led_col_row_to_number(i, j)][0] = get_button_val(i, j, button_state) * 16;
      }
    }
#endif

    write_leds(led_state);
  }
}

inline void button_delay() {
//asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");
MIOS32_DELAY_Wait_uS(100);
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

  PinSet(PO_OE_, 0);
  button_delay();
}

void read_current_col(u8 ret[NUM_ROWS/8]);
void read_buttons(u8 ret[NUM_COLS][NUM_ROWS/8]) {
  int i;
  PinSet(PO_OE_, 1);
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
    PinSet(PO_OE_, 1);

    PinSet(PO_SER, 0);
    button_delay();
    po_clk();
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

void button_test() {
  PinSet(PO_OE_, 1);
  PinSet(PO_SRCLK, 0);
  PinSet(PO_RCLK, 0);

  PinSet(PO_SRCLR_, 0);
  button_delay();
  PinSet(PO_SRCLR_, 1);

  PinSet(PO_SER, 1);
  button_delay();
  po_clk();

  PinSet(PO_OE_, 1);
  PinSet(PO_SER, 0);
  button_delay();
  po_clk();

  PinSet(PO_OE_, 1);
  PinSet(PO_SER, 0);
  button_delay();
  po_clk();

}

// LED Section
s32 led_col_row_to_number(s32 col, s32 row) {
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
