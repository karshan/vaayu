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
#define NUM_LEDS 8
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

  for (i = 0; i < 24; i++) {
    led_state[i][i % 3] = 0x10; 
    led_state[i][(i + 1) % 3] = 0x10;
    led_state[i][(i + 2) % 3] = 0x10;
  }
  xTaskCreate(TASK_LED, (signed portCHAR *)"LED", configMINIMAL_STACK_SIZE, NULL, PRIORITY_TASK_LED, NULL);
}


/////////////////////////////////////////////////////////////////////////////
// This task is running endless in background
/////////////////////////////////////////////////////////////////////////////
void APP_Background(void)
{
}

/////////////////////////////////////////////////////////////////////////////
// This hook is called when a MIDI package has been received
/////////////////////////////////////////////////////////////////////////////
void APP_MIDI_NotifyPackage(mios32_midi_port_t port, mios32_midi_package_t midi_package)
{
}


/////////////////////////////////////////////////////////////////////////////
// This hook is called before the shift register chain is scanned
/////////////////////////////////////////////////////////////////////////////
void APP_SRIO_ServicePrepare(void)
{
}


/////////////////////////////////////////////////////////////////////////////
// This hook is called after the shift register chain has been scanned
/////////////////////////////////////////////////////////////////////////////
void APP_SRIO_ServiceFinish(void)
{
}


/////////////////////////////////////////////////////////////////////////////
// This hook is called when a button has been toggled
// pin_value is 1 when button released, and 0 when button pressed
/////////////////////////////////////////////////////////////////////////////
void APP_DIN_NotifyToggle(u32 pin, u32 pin_value)
{
}


/////////////////////////////////////////////////////////////////////////////
// This hook is called when an encoder has been moved
// incrementer is positive when encoder has been turned clockwise, else
// it is negative
/////////////////////////////////////////////////////////////////////////////
void APP_ENC_NotifyChange(u32 encoder, s32 incrementer)
{
}


/////////////////////////////////////////////////////////////////////////////
// This hook is called when a pot has been moved
/////////////////////////////////////////////////////////////////////////////
void APP_AIN_NotifyChange(u32 pin, u32 pin_value)
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
        MIOS32_MIDI_SendDebugMessage("ov = %x, nv = %x\n", ov, nv);
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

void APP_Tick()
{
    u8 column;

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
}
