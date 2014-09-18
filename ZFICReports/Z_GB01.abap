*Using program RGUGBR00, regenerate the programs for the substitution in
* the current client. Parameter: "FI" application area, select the
*"Generate substitution export routines" checkbox (next to last
*checkbox).

REPORT Z_GB01.
PARAMETER: P_ACTUAL type C.
CHECK P_ACTUAL = 'X'.

TABLES: GB01.
UPDATE GB01 SET BEXCLUDE = ' '
              WHERE BOOLCLASS = '009'
                AND CLASSTYPE = 'S'
                AND BCLTAB    = 'BSEG'
                AND BCLFIELD  = 'VBUND'.

*DELETE FROM GB01
*              WHERE BOOLCLASS = '015'
*                AND CLASSTYPE = 'S'
*                AND BCLTAB    = 'BSEG'
*                AND BCLFIELD  = 'VBUND'.
*
*DELETE FROM GB01
*              WHERE BOOLCLASS = '015'
*                AND CLASSTYPE = 'S'
*                AND BCLTAB    = 'BSEG'
*                AND BCLFIELD  = '*'.

*GB01-BOOLCLASS = '015'.  GB01-CLASSTYPE = 'S'.
*GB01-BCLTAB    = 'BSEG'. GB01-BCLFIELD  = '*'.
*GB01-BEXCLUDE  = 'X'.
*MODIFY GB01.

GB01-BOOLCLASS = '009'.  GB01-CLASSTYPE = 'S'.
GB01-BCLTAB    = 'BSEG'. GB01-BCLFIELD  = '*'.
GB01-BEXCLUDE  = ' '.

GB01-BCLTAB    = 'BSEG'. GB01-BCLFIELD  = 'ZLSPR'.
MODIFY GB01.

*GB01-BCLTAB    = 'BSEG'. GB01-BCLFIELD  = 'SGTXT'.
*MODIFY GB01.
*
*GB01-BCLTAB    = 'BSEG'. GB01-BCLFIELD  = 'XREF3'.
*MODIFY GB01.
*
*GB01-BCLTAB    = 'BKPF'. GB01-BCLFIELD  = 'XBLNR'.
*MODIFY GB01.
*
*GB01-BCLTAB    = 'BSEG'. GB01-BCLFIELD  = 'XREF1'.
*MODIFY GB01.
*
*GB01-BCLTAB    = 'BSEG'. GB01-BCLFIELD  = 'XREF2'.
*MODIFY GB01.

write:/ '*** Notice ***'.
write:/ 'GB01 is changed'.
write:/ 'Please run RGUGBR00 with FI parameter'.
