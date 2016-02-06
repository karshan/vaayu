loadAPI (1);

host.defineController ("Vaayu", "Vaayu", "1.0", "C8E78BB2-C6EA-11E5-BAE6-7F1B2B8CFB91", "Karshan Sharma");
host.defineMidiPorts (1, 1);
host.addDeviceNameBasedDiscoveryPair(["MIOS32"], ["MIOS32"]);

var globals = {
  clip: null,
  leds: null
};

function onMidi(status, data1, data2) {
  globals.clip.isLoopEnabled().set(true);
  globals.clip.getLoopStart().setRaw(0);
  globals.clip.getLoopLength().setRaw(2);
  host.getMidiOutPort(0).sendSysex(ledSysexStr(globals.leds));

  if (data2 == 127) {
    if (data1 <= 39 && data1 >= 32) {
      globals.clip.toggleStep(7 - (data1 - 32), 24, 127);
    }
  }
}

function ledSysexStr(leds) {
  var x = 7;
  var y = 3;
  var out = "";
  for (i = 0; i < 32; i++) {
    out += leds[x][y];
    if (x == 0 && y == 2) {
      x = 7; y = 1;
    } else if (x == 0 && y == 0) {
      break; // unnecessary since we have i = 0 -> 32 but what the heck
    } else if (y == 3) {
      y = 2;
    } else if (y == 1) {
      y = 0;
    } else if (y == 2){
      x--;
      y = 3;
    } else if (y == 0) {
      x--;
      y = 1;
    }
  }
  return "F0" + out + "F7";
}

function playingStepObserver(playingStepIdx) {
  globals.prevPlayingStepIdx = globals.playingStepIdx;
  globals.playingStepIdx = playingStepIdx;
}

function stepDataObserver(x, y, isEnabled) {
  y = y - 24;
  if (x >= 0 && x < 8 && y >= 0 && y < 2) {
    println("stepDataObserver(" + x + ", " + y + ")");

    globals.stepData[x][y + 2] = isEnabled;
  }
}

function init ()
{
    println ("Initialized.");
    host.getMidiInPort(0).setMidiCallback(onMidi);
    globals.clip = host.createCursorClip(8, 100);
    globals.clip.addPlayingStepObserver(playingStepObserver);
    globals.clip.addStepDataObserver(stepDataObserver);
    globals.prevPlayingStepIdx = -1;

    globals.leds = new Array(8);
    for (var x = 0; x < 8; x++) {
      globals.leds[x] = new Array(4);
    }

    globals.stepData = new Array(8);
    for (var x = 0; x < 8; x++) {
      globals.stepData[x] = new Array(4);
    }
}

function exit ()
{
}

function flushLeds() {
  if (globals.leds != null) {
    for (x = 0; x < 8; x++) {
      for (y = 0; y < 4; y++) {
        if (globals.stepData[x][y] == true) {
          globals.leds[x][y] = "000030";
        } else {
          globals.leds[x][y] = "000000";
        }
      }
    }
    if (globals.playingStepIdx >= 0 && globals.playingStepIdx <= 7) {
      if (globals.prevPlayingStepIdx >= 0 && globals.prevPlayingStepIdx <= 7) {
        if (globals.stepData[globals.prevPlayingStepIdx][3] == true) {
          globals.leds[globals.prevPlayingStepIdx][3] = "000030";
        } else {
          globals.leds[globals.prevPlayingStepIdx][3] = "000000";
        }
        if (globals.stepData[globals.prevPlayingStepIdx][2] == true) {
          globals.leds[globals.prevPlayingStepIdx][2] = "000030";
        } else {
          globals.leds[globals.prevPlayingStepIdx][2] = "000000";
        }
      }
      globals.leds[globals.playingStepIdx][3] = "100000";
      globals.leds[globals.playingStepIdx][2] = "100000";
      globals.prevPlayingStepIdx = globals.playingStepIdx;
    }
    var ledStr = ledSysexStr(globals.leds);
    if (globals.lastLedStr != ledStr) {
      host.getMidiOutPort(0).sendSysex(ledStr);
      globals.lastLedStr = ledStr;
    }
  }
}

function flush ()
{
  flushLeds();
}
