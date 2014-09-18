FUNCTION ZIM_CHANGE_DOCUMENT_ZTIDSUSD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTIDSUSD) LIKE  ZTIDSUSD STRUCTURE  ZTIDSUSD
*"     REFERENCE(O_ZTIDSUSD) LIKE  ZTIDSUSD STRUCTURE  ZTIDSUSD
*"----------------------------------------------------------------------
   DATA : OBJECID      LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR : IT_CDTXT.
   OBJECID = N_ZTIDSUSD+3(21).

   CALL FUNCTION 'ZTIDSUSD_WRITE_DOCUMENT'
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
            N_ZTIDSUSD         =    N_ZTIDSUSD
            O_ZTIDSUSD         =    O_ZTIDSUSD
            UPD_ZTIDSUSD       =    UPD_CHNGIND.

ENDFUNCTION.
