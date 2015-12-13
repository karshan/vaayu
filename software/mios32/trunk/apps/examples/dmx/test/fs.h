// $Id: mid_file.h 771 2009-11-10 21:10:43Z tk $
/*
 * Header for MIDI file access routines
 *
 * ==========================================================================
 *
 *  Copyright (C) 2008 Thorsten Klose (tk@midibox.org)
 *  Licensed for personal non-commercial use only.
 *  All other rights reserved.
 * 
 * ==========================================================================
 */

#ifndef _FS_H
#define _FS_H

/////////////////////////////////////////////////////////////////////////////
// Global definitions
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// Global Types
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// Prototypes
/////////////////////////////////////////////////////////////////////////////

extern s32 FS_Init(u32 mode);

extern char *FS_UI_NameGet(void);
extern char *FS_FindNext(char *filename);

extern s32 FS_mount_fs(void);
extern s32 FS_open(char *filename);
extern u32 FS_read(void *buffer, u32 len);
extern s32 FS_eof(void);
extern s32 FS_seek(u32 pos);


/////////////////////////////////////////////////////////////////////////////
// Export global variables
/////////////////////////////////////////////////////////////////////////////

#endif /* _FS_H */
