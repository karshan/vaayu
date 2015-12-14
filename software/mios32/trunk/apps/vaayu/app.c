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
#define BUTTON_ROWS 8
#define LED_PIN (BUTTON_COLS + BUTTON_ROWS)
#define NUM_LEDS 16
int encoder_state[NUM_ENCODERS];
u8 button_state[BUTTON_COLS]; // if more than 8 rows u8 will need to become u16
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
  int i, pin, column;

  // initialize all LEDs
  MIOS32_BOARD_LED_Init(0xffffffff);

  for (pin = 0; pin < BUTTON_COLS; pin++) {
    MIOS32_BOARD_J10_PinInit(pin, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  }

  for (pin = BUTTON_COLS; pin < BUTTON_COLS + BUTTON_ROWS; pin++) {
    MIOS32_BOARD_J10_PinInit(pin, MIOS32_BOARD_PIN_MODE_INPUT_PD);
  }

  MIOS32_BOARD_J10_PinInit(LED_PIN, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);

  for (column = 0; column < BUTTON_COLS; column++) {
    button_state[column] = 0;
  }

  reset_encoders();  

  for (i = 0; i < NUM_LEDS; i++) {
    led_state[i][0] = 0x10; 
    led_state[i][1] = 0x10;
    led_state[i][2] = 0x10;
  }
}


void APP_Background(void)
{
}

void APP_MIDI_NotifyPackage(mios32_midi_port_t port, mios32_midi_package_t midi_package)
{
}

inline void ws2812_low() {
  MIOS32_BOARD_J10_PinSet(LED_PIN, 1);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

  MIOS32_BOARD_J10_PinSet(LED_PIN, 0);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

}

inline void ws2812_high() {
  MIOS32_BOARD_J10_PinSet(LED_PIN, 1);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

  MIOS32_BOARD_J10_PinSet(LED_PIN, 0);
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

void BUTTON_NotifyToggle(u8 old_value, u8 new_value, u8 col)
{
    // ENCODERS_PER_COL = 4
    int i;
    for (i = 0; i < 4; i++) {
        u8 encoder = col * 4 + i;
        u8 ov = (old_value & (3 << (i * 2))) >> (i * 2);
        u8 nv = (new_value & (3 << (i * 2))) >> (i * 2);
        if (ov ^ nv) {
            update_encoder_state(encoder, ov, nv);
            MIOS32_MIDI_SendCC(DEFAULT, Chn1, 0x10 + encoder, encoder_state[encoder]);
        }
    }
}

void setColumns(u8 value) {
  int i;
  for (i = 0; i < BUTTON_COLS; i++) {
    MIOS32_BOARD_J10_PinSet(i, (value & (1 << i)) ? 1 : 0);
  }
}

u8 getRows() {
  return (u8)((MIOS32_BOARD_J10_Get() & (((1 << BUTTON_ROWS) - 1) << BUTTON_COLS)) >> BUTTON_COLS);
}

void interpolate(u8 *out, u8 *a, u8 *b, int s) {
    int i;
    for (i = 0; i < 3; i++) {
        out[i] = (a[i] * (128 - s) + b[i] * s)/128;
    }
}

void set_color_by_encoder(u8 *c, int v) {
    u8 blue[] = { 32, 0, 32 };
    u8 white[] = { 0, 0, 64 };
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
            set_color_by_encoder(led_state[x], encoder_state[i]);
            set_color_by_encoder(led_state[x + BUTTON_COLS], encoder_state[i]);
        } else {
            x = ((BUTTON_COLS * 2) * (i - 4)) + 1;
            set_color_by_encoder(led_state[x], encoder_state[i]);
            set_color_by_encoder(led_state[x + BUTTON_COLS], encoder_state[i]);
        }
    }
}

int time_ = 0;
void APP_Tick()
{
    u8 column;
    if (time_ % 10 == 0) {
        TASK_LED(NULL);
    }

	for(column = 0; column < BUTTON_COLS; column++) {
        u8 row_value;
        setColumns(1 << column);
        MIOS32_DELAY_Wait_uS(100); //TODO fine tune to save time
        row_value = getRows();
        if (row_value ^ button_state[column]) {
            BUTTON_NotifyToggle(button_state[column], row_value, column);
        }
        button_state[column] = row_value;
    }

    color_leds_by_encoder_state();
    time_++;
}
