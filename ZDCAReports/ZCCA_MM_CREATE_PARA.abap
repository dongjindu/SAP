*----------------------------------------------------------------------*
*   INCLUDE ZCCA_MM_CREATE_PARA                                        *
*----------------------------------------------------------------------*
*#### Selection Screen ####
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME  TITLE text-t01.
PARAMETERS : p_file   LIKE rlgrap-filename   OBLIGATORY.
*SELECTION-SCREEN ULINE.
*PARAMETERS : P_TEST  TYPE BAPIFLAG DEFAULT 'X' AS LISTBOX
*                                             VISIBLE LENGTH 14.

SELECTION-SCREEN ULINE.
PARAMETERS : p_mode   TYPE tb_bdcmode DEFAULT 'N' AS LISTBOX
                                         VISIBLE LENGTH 25.
SELECTION-SCREEN END OF BLOCK blk .
