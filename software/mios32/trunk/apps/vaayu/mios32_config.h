// $Id: mios32_config.h 1086 2010-10-03 17:37:29Z tk $
/*
 * Local MIOS32 configuration file
 *
 * this file allows to disable (or re-configure) default functions of MIOS32
 * available switches are listed in $MIOS32_PATH/modules/mios32/MIOS32_CONFIG.txt
 *
 */

#ifndef _MIOS32_CONFIG_H
#define _MIOS32_CONFIG_H

#define MIOS32_DONT_USE_LCD 1
#define MIOS32_DONT_USE_SPI 1
#define MIOS32_DONT_USE_SRIO 1
#define MIOS32_DONT_USE_DIN 1
#define MIOS32_DONT_USE_DOUT 1
#define MIOS32_DONT_USE_ENC 1
#define MIOS32_DONT_USE_MF 1
#define MIOS32_DONT_USE_AIN 1
#define MIOS32_DONT_USE_IIC_BS 1
#define MIOS32_DONT_USE_COM 1

#endif /* _MIOS32_CONFIG_H */
