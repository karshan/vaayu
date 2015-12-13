// $Id: mios32_config.h 502 2009-05-09 14:20:30Z tk $
/*
 * Local MIOS32 configuration file
 *
 * this file allows to disable (or re-configure) default functions of MIOS32
 * available switches are listed in $MIOS32_PATH/modules/mios32/MIOS32_CONFIG.txt
 *
 */

#ifndef _MIOS32_CONFIG_H
#define _MIOS32_CONFIG_H

// The boot message which is print during startup and returned on a SysEx query
#define MIOS32_LCD_BOOT_MSG_LINE1 "MIDIbox C4 v0.03 (2010-05-01)"
#define MIOS32_LCD_BOOT_MSG_LINE2 "(C) 2010 ilmenator"

// FatFs configuration: support long filenames
#define FATFS_USE_LFN 1
#define FATFS_MAX_LFN 255

#endif /* _MIOS32_CONFIG_H */
