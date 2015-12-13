// $Id: seq.h 687 2009-08-03 20:48:54Z tk $
/*
 * Header file for sequencer routines
 *
 * ==========================================================================
 *
 *  Copyright (C) 2008 Thorsten Klose (tk@midibox.org)
 *  Licensed for personal non-commercial use only.
 *  All other rights reserved.
 * 
 * ==========================================================================
 */

#ifndef _SEQ_H
#define _SEQ_H

#include <notestack.h>

/////////////////////////////////////////////////////////////////////////////
// Global definitions
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// Global Types
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// Prototypes
/////////////////////////////////////////////////////////////////////////////

extern s32 SEQ_Init(u32 mode);

extern s32 SEQ_Reset(void);

extern s32 SEQ_Handler(void);

extern s32 SEQ_NotifyNoteOn(u8 note, u8 velocity);
extern s32 SEQ_NotifyNoteOff(u8 note);


/////////////////////////////////////////////////////////////////////////////
// Export global variables
/////////////////////////////////////////////////////////////////////////////

static u8 seq_pause;


#endif /* _SEQ_H */
