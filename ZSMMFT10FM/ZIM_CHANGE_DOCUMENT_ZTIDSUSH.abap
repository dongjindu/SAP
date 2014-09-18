FUNCTION ZIM_CHANGE_DOCUMENT_ZTIDSUSH.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTIDSUSH) LIKE  ZTIDSUSH STRUCTURE  ZTIDSUSH
*"     REFERENCE(O_ZTIDSUSH) LIKE  ZTIDSUSH STRUCTURE  ZTIDSUSH
*"----------------------------------------------------------------------
   DATA : OBJECID      LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR : IT_CDTXT.
   OBJECID = N_ZTIDSUSH+3(18).

   CALL FUNCTION 'ZTIDSUSH_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    OBJECID
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            PLANNED_CHANGE_NUMBER   = ''
            OBJECT_CHANGE_INDICATOR = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            N_ZTIDSUSH         =    N_ZTIDSUSH
            O_ZTIDSUSH         =    O_ZTIDSUSH
            UPD_ZTIDSUSH       =    UPD_CHNGIND.

ENDFUNCTION.
