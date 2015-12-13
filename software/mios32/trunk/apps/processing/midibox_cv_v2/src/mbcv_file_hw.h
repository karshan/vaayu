// $Id: mbcv_file_hw.h 1912 2014-01-03 23:15:07Z tk $
/*
 * Header for file functions
 *
 * ==========================================================================
 *
 *  Copyright (C) 2013 Thorsten Klose (tk@midibox.org)
 *  Licensed for personal non-commercial use only.
 *  All other rights reserved.
 * 
 * ==========================================================================
 */

#ifndef _MBCV_FILE_HW_H
#define _MBCV_FILE_HW_H


/////////////////////////////////////////////////////////////////////////////
// Global definitions
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// Global Types
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// Prototypes
/////////////////////////////////////////////////////////////////////////////

extern s32 MBCV_FILE_HW_Init(u32 mode);
extern s32 MBCV_FILE_HW_Load(void);
extern s32 MBCV_FILE_HW_Unload(void);

extern s32 MBCV_FILE_HW_Valid(void);
extern s32 MBCV_FILE_HW_ConfigLocked(void);

extern s32 MBCV_FILE_HW_LockConfig(void);

extern s32 MBCV_FILE_HW_Read(void);


/////////////////////////////////////////////////////////////////////////////
// Export global variables
/////////////////////////////////////////////////////////////////////////////


#endif /* _MBCV_FILE_HW_H */
