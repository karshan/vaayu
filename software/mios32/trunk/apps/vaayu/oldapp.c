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

#define PRIORITY_TASK_MATRIX_SCAN	( tskIDLE_PRIORITY + 2 )

#define KEYBOARD_MIDI_PORT USB0
#define KEYBOARD_MIDI_CHN Chn1

/////////////////////////////////////////////////////////////////////////////
// Prototypes
/////////////////////////////////////////////////////////////////////////////
static void TASK_MatrixScan(void *pvParameters);


/////////////////////////////////////////////////////////////////////////////
// Local Variables
/////////////////////////////////////////////////////////////////////////////

u8 button_state[4]; // if more than 8 rows u8 will need to become u16
u8 led_state[24][3]; // GRB

/////////////////////////////////////////////////////////////////////////////
// This hook is called after startup to initialize the application
/////////////////////////////////////////////////////////////////////////////
void APP_Init(void)
{
  // initialize all LEDs
  MIOS32_BOARD_LED_Init(0xffffffff);

  int pin;
  for (pin = 0; pin < 4; pin++) {
    MIOS32_BOARD_J10_PinInit(pin, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  }
  for (pin = 4; pin < 8; pin++) {
    MIOS32_BOARD_J10_PinInit(pin, MIOS32_BOARD_PIN_MODE_INPUT_PD);
  }
  MIOS32_BOARD_J10_PinInit(8, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);

  int column;
  for (column = 0; column < 4; column++) {
    button_state[column] = 0;
  }

  int i;
  for (i = 0; i < 24; i++) {
    led_state[i][i % 3] = 0x7f; 
    led_state[i][(i + 1) % 3] = 0x7f;
    led_state[i][(i + 2) % 3] = 0x7f;
  }

  MIOS32_STOPWATCH_Init(1);

  // start matrix scan task
  xTaskCreate(TASK_MatrixScan, (signed portCHAR *)"MatrixScan", configMINIMAL_STACK_SIZE, NULL, PRIORITY_TASK_MATRIX_SCAN, NULL);
}


/////////////////////////////////////////////////////////////////////////////
// This task is running endless in background
/////////////////////////////////////////////////////////////////////////////
void APP_Background(void)
{
  #if 0
  // endless loop
  while( 1 ) {
    // toggle the state of all LEDs (allows to measure the execution speed with a scope)
    MIOS32_BOARD_LED_Set(0xffffffff, ~MIOS32_BOARD_LED_Get());

  }
  #endif
}

u32 shiftOut_time;
/////////////////////////////////////////////////////////////////////////////
// This hook is called when a MIDI package has been received
/////////////////////////////////////////////////////////////////////////////
void APP_MIDI_NotifyPackage(mios32_midi_port_t port, mios32_midi_package_t midi_package)
{
  #if 0
  if (midi_package.velocity != 0) {
    if (midi_package.note == 0x18) { // C0
      multiplier = 2;
      offset = 0;
      timer_period = 25;
    } else if (midi_package.note == 0x19) {
      multiplier++;
      timer_period++;
    } else if (midi_package.note == 0x1a) {
      multiplier--;
      timer_period--;
    } else if (midi_package.note == 0x1b) {
      offset++;
    } else if (midi_package.note == 0x1c) {
      offset--;
    } else if (midi_package.note == 0x1d) {
      diff++;
    } else if (midi_package.note == 0x1e) {
      diff--;
    }
  }
  //MIOS32_MIDI_SendDebugMessage("multiplier = %d, offset = %d, diff = %d, note = %x, velocity = %x\n", multiplier, offset, diff, midi_package.note, midi_package.velocity);
  //MIOS32_MIDI_SendDebugMessage("timer_period = %d, init_return = %d, deinit_return = %d\n", timer_period, init_return, deinit_return);
  #endif
  MIOS32_MIDI_SendDebugMessage("shiftOut_time = %d", shiftOut_time);
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

inline void delay_20th_us(u32 a) {
    asm volatile(" mov r0, %[a] \n\t"
             "1: subs r0, #1 \n\t"
             " bhi 1b \n\t"
             :
             : [a] "r" (a)
             : "r0");
}

inline void ws2812_low() {
  MIOS32_BOARD_J10_PinSet(8, 1);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

  MIOS32_BOARD_J10_PinSet(8, 0);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

}

inline void ws2812_high() {
  MIOS32_BOARD_J10_PinSet(8, 1);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

  MIOS32_BOARD_J10_PinSet(8, 0);
asm volatile("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t");

}

inline void ws2812_shiftOut(u8 a) {
  int i;
  MIOS32_STOPWATCH_Reset();
  u32 start = MIOS32_STOPWATCH_ValueGet();
  for (i = 7; i >= 0; i--) {
    if (a & (1 << i)) {
      ws2812_high();
    } else {
      ws2812_low();
    }
  }
  shiftOut_time = MIOS32_STOPWATCH_ValueGet() - start;
}

u8 sin_table[1000] = { 64,64,65,65,66,66,66,67,67,68,68,68,69,69,70,70,70,71,71,72,72,72,73,73,74,74,74,75,75,76,76,76,77,77,78,78,78,79,79,80,80,80,81,81,81,82,82,83,83,83,84,84,85,85,85,86,86,86,87,87,88,88,88,89,89,89,90,90,91,91,91,92,92,92,93,93,93,94,94,94,95,95,96,96,96,97,97,97,98,98,98,99,99,99,100,100,100,101,101,101,102,102,102,103,103,103,104,104,104,104,105,105,105,106,106,106,107,107,107,108,108,108,108,109,109,109,110,110,110,110,111,111,111,111,112,112,112,113,113,113,113,114,114,114,114,115,115,115,115,116,116,116,116,116,117,117,117,117,118,118,118,118,118,119,119,119,119,119,120,120,120,120,120,121,121,121,121,121,122,122,122,122,122,122,123,123,123,123,123,123,124,124,124,124,124,124,124,124,125,125,125,125,125,125,125,125,126,126,126,126,126,126,126,126,126,126,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,126,126,126,126,126,126,126,126,126,126,125,125,125,125,125,125,125,125,124,124,124,124,124,124,124,124,123,123,123,123,123,123,122,122,122,122,122,122,121,121,121,121,121,120,120,120,120,120,119,119,119,119,119,118,118,118,118,118,117,117,117,117,116,116,116,116,116,115,115,115,115,114,114,114,114,113,113,113,113,112,112,112,111,111,111,111,110,110,110,110,109,109,109,108,108,108,108,107,107,107,106,106,106,105,105,105,104,104,104,104,103,103,103,102,102,102,101,101,101,100,100,100,99,99,99,98,98,98,97,97,97,96,96,96,95,95,94,94,94,93,93,93,92,92,92,91,91,91,90,90,89,89,89,88,88,88,87,87,86,86,86,85,85,85,84,84,83,83,83,82,82,81,81,81,80,80,80,79,79,78,78,78,77,77,76,76,76,75,75,74,74,74,73,73,72,72,72,71,71,70,70,70,69,69,68,68,68,67,67,66,66,66,65,65,64,64,64,63,63,62,62,62,61,61,60,60,60,59,59,58,58,58,57,57,56,56,56,55,55,54,54,54,53,53,52,52,52,51,51,50,50,50,49,49,48,48,48,47,47,47,46,46,45,45,45,44,44,43,43,43,42,42,42,41,41,40,40,40,39,39,39,38,38,37,37,37,36,36,36,35,35,35,34,34,34,33,33,32,32,32,31,31,31,30,30,30,29,29,29,28,28,28,27,27,27,26,26,26,25,25,25,24,24,24,24,23,23,23,22,22,22,21,21,21,20,20,20,20,19,19,19,18,18,18,18,17,17,17,17,16,16,16,15,15,15,15,14,14,14,14,13,13,13,13,12,12,12,12,12,11,11,11,11,10,10,10,10,10,9,9,9,9,9,8,8,8,8,8,7,7,7,7,7,6,6,6,6,6,6,5,5,5,5,5,5,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,8,8,8,8,8,9,9,9,9,9,10,10,10,10,10,11,11,11,11,12,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,17,17,17,17,18,18,18,18,19,19,19,20,20,20,20,21,21,21,22,22,22,23,23,23,24,24,24,24,25,25,25,26,26,26,27,27,27,28,28,28,29,29,29,30,30,30,31,31,31,32,32,32,33,33,34,34,34,35,35,35,36,36,36,37,37,37,38,38,39,39,39,40,40,40,41,41,42,42,42,43,43,43,44,44,45,45,45,46,46,47,47,47,48,48,48,49,49,50,50,50,51,51,52,52,52,53,53,54,54,54,55,55,56,56,56,57,57,58,58,58,59,59,60,60,60,61,61,62,62,62,63,63,64 };

u32 ctr = 0;
void ledTask() {
  int i, j;
  ctr++;
  for (i = 0; i < 24; i++) {
    u8 val = sin_table[ctr % 1000];
    ws2812_shiftOut(val);
    ws2812_shiftOut(0);
    ws2812_shiftOut(val/2);
  }
  MIOS32_BOARD_J10_PinSet(8, 0);
  //MIOS32_BOARD_LED_Set(0xffffffff, ~MIOS32_BOARD_LED_Get());
}

/////////////////////////////////////////////////////////////////////////////
// This task is called each mS to scan the button matrix
/////////////////////////////////////////////////////////////////////////////

// will be called on button pin changes (see TASK_BLM_Check)
void BUTTON_NotifyToggle(u8 row, u8 column, u8 pin_value)
{
  int note_number = 0x15 + (4 * column) + row;

  if( pin_value ) {
    // determine velocity depending on delay
    int velocity = 127;

    MIOS32_MIDI_SendNoteOn(KEYBOARD_MIDI_PORT, KEYBOARD_MIDI_CHN, note_number, velocity);
  } else {
    // send Note On with velocity 0
    MIOS32_MIDI_SendNoteOn(KEYBOARD_MIDI_PORT, KEYBOARD_MIDI_CHN, note_number, 0x00);
  }
}


void setColumns(u8 value) {
  int i;
  for (i = 0; i < 4; i++) {
    MIOS32_BOARD_J10_PinSet(i, (value & (1 << i)) ? 1 : 0);
  }
}

u8 getRows() {
  return (u8)((MIOS32_BOARD_J10_Get() & 0x00f0) >> 4);
}

#if 0
u32 ws2812_led = 0;
u8 ws2812_color = 0;
s8 ws2812_bit = 7;
u8 ws2812_state = 0;
void ws2812_irq(void) {
  if (ws2812_state == 0) {
    MIOS32_BOARD_J10_PinSet(8, 1);
  } else if(ws2812_state == 32) {
    if (ws2812_color == 1) { //led_state[ws2812_led][ws2812_color] & (1 << ws2812_bit)) {
      //MIOS32_BOARD_J10_PinSet(8, 1);
    } else {
      MIOS32_BOARD_J10_PinSet(8, 0);
    }
  } else if (ws2812_state == 67) {
    MIOS32_BOARD_J10_PinSet(8, 0);
  }

  if (++ws2812_state >= 105) {
    ws2812_state = 0;
    ws2812_bit--;
  }
  if (ws2812_bit < 0) {
    ws2812_bit = 7;
    ws2812_color++;
  }
  if (ws2812_color >= 3) {
    ws2812_color = 0;
    ws2812_led++;
  }
  if (ws2812_led >= 24) {
    ws2812_led = 0;
    MIOS32_BOARD_J10_PinSet(8, 0);
    deinit_return = MIOS32_TIMER_DeInit(0);
  }
}
#endif

void TASK_MatrixScan(void *pvParameters)
{
  while( 1 ) {
    // wait for next timesplice (1 mS)
    vTaskDelay(1 / portTICK_RATE_MS);
    ledTask();
#if 0
    ws2812_bit = 7;
    ws2812_state = 0;
    ws2812_led = 0;
    ws2812_color = 0;
    init_return = MIOS32_TIMER_Init_Raw(0, timer_period, ws2812_irq, MIOS32_IRQ_PRIO_HIGHEST);
#endif

    u8 row;
    u8 column = 0;
    u8 value;
	for(column = 0; column < 4; column++) {
      setColumns(1 << column);
	  MIOS32_DELAY_Wait_uS(100); //TODO fine tune to save time
      u8 row_value = getRows();
      u8 changed = row_value ^ button_state[column];
      if (changed) {
        for (row = 0; row < 4; row++) {
          if (changed & (1 << row)) {
            value = (row_value & (1 << row)) ? 1 : 0;
            BUTTON_NotifyToggle(row, column, value);
          }
        }
      }
      button_state[column] = row_value;
    }
  }
}
