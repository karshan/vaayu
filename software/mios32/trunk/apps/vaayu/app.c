// $Id: app.c 1109 2010-10-11 23:14:49Z tk $
/*
 * Example for a "fastscan button matrix"
 *
 * ==========================================================================
 *
 *  Copyright (C) 2010 Thorsten Klose(tk@midibox.org)
 *  Licensed for personal non-commercial use only.
 *  All other rights reserved.
 * 
 * ==========================================================================
 */

/////////////////////////////////////////////////////////////////////////////
// Include files
/////////////////////////////////////////////////////////////////////////////

#include <mios32.h>
#include "app.h"

#include <FreeRTOS.h>
#include <task.h>
#include <queue.h>

/////////////////////////////////////////////////////////////////////////////
// for optional debugging messages
/////////////////////////////////////////////////////////////////////////////

// level >= 1: print warnings (recommented default value)
// level >= 2: print debug messages for Robin's Fatar Keyboard
// level >= 3: print row/column messages in addition for initial testing of matrix scan for other usecases
#define DEBUG_VERBOSE_LEVEL 1
#define DEBUG_MSG MIOS32_MIDI_SendDebugMessage

/////////////////////////////////////////////////////////////////////////////
// Local definitions
/////////////////////////////////////////////////////////////////////////////

#define PRIORITY_TASK_LED	( tskIDLE_PRIORITY + 2 )
static void TASK_LED(void *p);

/////////////////////////////////////////////////////////////////////////////
// Prototypes
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// Local Variables
/////////////////////////////////////////////////////////////////////////////

#define NUM_ENCODERS 8
#define BUTTON_COLS 2
#define BUTTON_ROWS 16
// rows 0 - 7 are encoders, 8 - 15 are buttons
#define LED_PIN (BUTTON_ROWS - (16 - BUTTON_COLS))
#define NUM_LEDS 32
int encoder_state[NUM_ENCODERS];
u8 button_state[BUTTON_ROWS][BUTTON_COLS]; // if more than 8 rows u8 will need to become u16
u8 led_state[NUM_LEDS][3]; // GRB

void reset_encoders() {
    int i;
    for (i = 0; i < NUM_ENCODERS; i++) {
        encoder_state[i] = 64;
    }
}

/////////////////////////////////////////////////////////////////////////////
// This hook is called after startup to initialize the application
/////////////////////////////////////////////////////////////////////////////
void APP_Init(void)
{
  int i, pin, row, col;

  // initialize all LEDs
  MIOS32_BOARD_LED_Init(0xffffffff);

  for (pin = 0; pin < BUTTON_COLS; pin++) {
    MIOS32_BOARD_J10_PinInit(pin, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  }

  for (pin = BUTTON_COLS; pin < 16; pin++) {
    MIOS32_BOARD_J10_PinInit(pin, MIOS32_BOARD_PIN_MODE_INPUT_PD);
  }

  for (pin = 0; pin < BUTTON_ROWS - (16 - BUTTON_COLS); pin++) {
    MIOS32_BOARD_J5_PinInit(pin, MIOS32_BOARD_PIN_MODE_INPUT_PD);
  }

  MIOS32_BOARD_J5_PinInit(LED_PIN, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);

  for (row = 0; row < BUTTON_ROWS; row++) {
      for (col = 0; col < BUTTON_COLS; col++) {
          button_state[row][col] = 0;
      }
  }

  reset_encoders();  

  for (i = 0; i < NUM_LEDS; i++) {
    led_state[i][0] = 0; 
    led_state[i][1] = 0;
    led_state[i][2] = 0;
  }
}


void APP_Background(void)
{
}

void APP_MIDI_NotifyPackage(mios32_midi_port_t port, mios32_midi_package_t midi_package)
{
}

inline void ws2812_low() {
  MIOS32_BOARD_J5_PinSet(LED_PIN, 1);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

  MIOS32_BOARD_J5_PinSet(LED_PIN, 0);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

}

inline void ws2812_high() {
  MIOS32_BOARD_J5_PinSet(LED_PIN, 1);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

  MIOS32_BOARD_J5_PinSet(LED_PIN, 0);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");
}

inline void ws2812_shiftOut(u8 a) {
  int i;
  for (i = 7; i >= 0; i--) {
    if (a & (1 << i)) {
      ws2812_high();
    } else {
      ws2812_low();
    }
  }
}

void TASK_LED(void *p) {
  int i;
  for (i = 0; i < NUM_LEDS; i++) {
    ws2812_shiftOut(led_state[i][0]);
    ws2812_shiftOut(led_state[i][1]);
    ws2812_shiftOut(led_state[i][2]);
  }
  MIOS32_BOARD_J10_PinSet(LED_PIN, 0);
}

void update_encoder_state(u8 encoder, u8 ov, u8 nv) {
    u8 cw[] = { 2, 0, 3, 1 };  // 0 -> 2, 1 -> 3, 2 -> 3, 3 -> 1 sequence: 0 2 3 1
    u8 ccw[] = { 1, 3, 0, 2 }; // 0 -> 1, 1 -> 3, 2 -> 0, 3 -> 2 sequence: 0 1 3 2
    if (cw[ov] == nv) {
        encoder_state[encoder]++;
    } else if (ccw[ov] == nv) {
        encoder_state[encoder]--;
    } else {
        MIOS32_MIDI_SendDebugMessage("encode: %d: ov = %x, nv = %x\n", encoder, ov, nv);
    }
    if (encoder_state[encoder] >= 127) encoder_state[encoder] = 127;
    if (encoder_state[encoder] < 0) encoder_state[encoder] = 0;
}

void notify_button(int row, int col, int old_val, int new_val) {
    if (old_val ^ new_val) {
        MIOS32_MIDI_SendDebugMessage("press on (%d, %d): %d\n", row, col, new_val);
        MIOS32_MIDI_SendNoteOn(DEFAULT, Chn1, (col * BUTTON_ROWS + row + 24) & 0x7f, new_val ? 0x7f : 0x0);
    }
}

void notify_encoder(int encoder, int old_val, int new_val)
{
    if (old_val ^ new_val) {
        update_encoder_state(encoder, old_val, new_val);
        MIOS32_MIDI_SendCC(DEFAULT, Chn1, 0x10 + encoder, encoder_state[encoder]);
    }
}

void setColumns(u8 value) {
  int i;
  for (i = 0; i < BUTTON_COLS; i++) {
    MIOS32_BOARD_J10_PinSet(i, (value & (1 << i)) ? 1 : 0);
  }
}

void getRows(u8 *out) {
    int pin;
    int out_index = 0;
    for (pin = BUTTON_COLS; pin < 16; pin++) {
        out[out_index++] = MIOS32_BOARD_J10_PinGet(pin);
    }
    for (pin = 0; pin < BUTTON_ROWS - (16 - BUTTON_COLS); pin++) {
        out[out_index++] = MIOS32_BOARD_J5_PinGet(pin);
    }
}

void interpolate(u8 *out, u8 *a, u8 *b, int s) {
    int i;
    for (i = 0; i < 3; i++) {
        out[i] = (a[i] * (128 - s) + b[i] * s)/128;
    }
}

void set_color_by_encoder_value(u8 *c, int v) {
    u8 blue[] = { 0, 0, 64 };
    u8 white[] = { 32, 32, 32 };
    u8 red[] = { 0, 128, 0 };
    if (v <= 64) {
        interpolate(c, blue, white, v * 2);
    } else {
        interpolate(c, white, red, (v - 64) * 2);
    }
}

void color_leds_by_encoder_state() {
    int i, x;
    for (i = 0; i < NUM_ENCODERS; i++) {
        // 0 -> 0 % n, n
        // 1 -> n*2 * 1, n + 1
        // 2 -> n*2 * 2,
        // 3 -> n*2 * 3, 
        if (i < 4) {
            x = (BUTTON_COLS * 2) * i;
            set_color_by_encoder_value(led_state[x], encoder_state[i]);
            set_color_by_encoder_value(led_state[x + BUTTON_COLS], encoder_state[i]);
        } else {
            x = ((BUTTON_COLS * 2) * (i - 4)) + 1;
            set_color_by_encoder_value(led_state[x], encoder_state[i]);
            set_color_by_encoder_value(led_state[x + BUTTON_COLS], encoder_state[i]);
        }
    }
}

u8 encoder_bits(u8 a, u8 b) {
    return (a ? 2 : 0) | (b ? 1 : 0);
}

int time_ = 0;
void APP_Tick()
{
    int col, row;
    u8 rows_out[BUTTON_ROWS];

    if (time_ % 10 == 0) {
        TASK_LED(NULL);
    }

	for (col = 0; col < BUTTON_COLS; col++) {
        setColumns(1 << col);

        MIOS32_DELAY_Wait_uS(100); //TODO fine tune to save time

        getRows(rows_out);

        for (row = 0; row < 8; row += 2) { // encoders
            u8 old_val = encoder_bits(button_state[row + 1][col], button_state[row][col]);
            u8 new_val = encoder_bits(rows_out[row + 1], rows_out[row]);

            notify_encoder(col * 4 + (row / 2), old_val, new_val);

            button_state[row][col] = rows_out[row];
            button_state[row + 1][col] = rows_out[row + 1];
        }
        for (row = 8; row < 16; row++) { // buttons
            u8 old_val = button_state[row][col];
            u8 new_val = rows_out[row];

            notify_button(row, col, old_val, new_val);

            button_state[row][col] = new_val;
        }
    }

    color_leds_by_encoder_state();
    time_++;
}
