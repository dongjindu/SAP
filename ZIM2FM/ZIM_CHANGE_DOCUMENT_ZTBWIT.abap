FUNCTION ZIM_CHANGE_DOCUMENT_ZTBWIT .
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"             REFERENCE(N_ZTBWIT) LIKE  ZTBWIT STRUCTURE  ZTBWIT
*"             REFERENCE(O_ZTBWIT) LIKE  ZTBWIT STRUCTURE  ZTBWIT
*"----------------------------------------------------------------------
  DATA : OBJECID  LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR: IT_CDTXT.
   OBJECID = N_ZTBWIT+3(18).

   CALL FUNCTION 'ZTBWIT_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBWIT           =    N_ZTBWIT
            O_ZTBWIT           =    O_ZTBWIT
            UPD_ZTBWIT         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = UPD_CHNGIND
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTBWIT       = ''
      tables
            ICDTXT_ZTBWIT           =    IT_CDTXT.

ENDFUNCTION.
