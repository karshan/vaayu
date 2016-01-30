#include <mios32.h>
#include "app.h"

#include <FreeRTOS.h>
#include <task.h>
#include <queue.h>

#define BUTTON_COLS 4
#define BUTTON_ROWS 8
#define LED_PIN (BUTTON_ROWS + BUTTON_COLS)
#define NUM_LEDS 32
u8 button_state[BUTTON_ROWS][BUTTON_COLS]; // if more than 8 rows u8 will need to become u16
u8 led_state[NUM_LEDS][3]; // GRB

enum {
  WAITING_FOR_START,
  WAITING_FOR_END
} sysex_parser_state = WAITING_FOR_START;

int sysex_parser_ctr;

s32 SYSEX_Parser(mios32_midi_port_t port, u8 midi_in) {
  if (sysex_parser_state == WAITING_FOR_START && midi_in == 0xf0) {
    sysex_parser_state = WAITING_FOR_END;
    sysex_parser_ctr = 0;
  } else if (sysex_parser_state == WAITING_FOR_END) {
    if (midi_in == 0xf7) {
      sysex_parser_state = WAITING_FOR_START;
    } else {
      led_state[sysex_parser_ctr/3 % NUM_LEDS][sysex_parser_ctr % 3] = midi_in;
      sysex_parser_ctr++;
    }
  }
  return 1; //Don't call APP_MIDI_NOTIFY
}

// This hook is called after startup to initialize the application
void APP_Init(void)
{
  int i, pin, row, col;

  // initialize all LEDs
  MIOS32_BOARD_LED_Init(0xffffffff);

  for (pin = 0; pin < BUTTON_COLS; pin++) {
    MIOS32_BOARD_J10_PinInit(pin, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);
  }

  for (pin = BUTTON_COLS; pin < BUTTON_COLS + BUTTON_ROWS; pin++) {
    MIOS32_BOARD_J10_PinInit(pin, MIOS32_BOARD_PIN_MODE_INPUT_PD);
  }

  MIOS32_BOARD_J10_PinInit(LED_PIN, MIOS32_BOARD_PIN_MODE_OUTPUT_PP);

  for (row = 0; row < BUTTON_ROWS; row++) {
      for (col = 0; col < BUTTON_COLS; col++) {
          button_state[row][col] = 0;
      }
  }

  for (i = 0; i < NUM_LEDS; i++) {
    led_state[i][0] = 0; 
    led_state[i][1] = 0;
    led_state[i][2] = 0;
  }
  MIOS32_MIDI_SysExCallback_Init(SYSEX_Parser);
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

void notify_button(int row, int col, int old_val, int new_val) {
    if (old_val ^ new_val) {
        MIOS32_MIDI_SendNoteOn(DEFAULT, Chn1, (col * BUTTON_ROWS + row + 24) & 0x7f, new_val ? 0x7f : 0x0);
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
    for (pin = BUTTON_COLS; pin < BUTTON_COLS + BUTTON_ROWS; pin++) {
        out[out_index++] = MIOS32_BOARD_J10_PinGet(pin);
    }
}

void color_leds_by_button_state() {
    int row, col;
    for (row = 8; row < BUTTON_ROWS; row++) {
        for (col = 0; col < BUTTON_COLS; col++) {
            led_state[row * BUTTON_COLS + col][0] = 0;
            led_state[row * BUTTON_COLS + col][1] = button_state[row][col] ? 64 : 0;
            led_state[row * BUTTON_COLS + col][2] = button_state[row][col] ? 64 : 0;
        }
    }
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

        for (row = 0; row < BUTTON_ROWS; row++) { // buttons
            u8 old_val = button_state[row][col];
            u8 new_val = rows_out[row];

            notify_button(row, col, old_val, new_val);

            button_state[row][col] = new_val;
        }
    }

    color_leds_by_button_state();
    time_++;
}
