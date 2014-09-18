FUNCTION ZIM_CHANGE_DOCUMENT_ZTCCIT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTCCIT) LIKE  ZTCCIT STRUCTURE  ZTCCIT
*"     REFERENCE(O_ZTCCIT) LIKE  ZTCCIT STRUCTURE  ZTCCIT
*"----------------------------------------------------------------------
   DATA : OBJECID  LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR: IT_CDTXT.
   OBJECID = N_ZTCCIT+3(15).

   CALL FUNCTION 'ZTCCIT_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTCCIT           =    N_ZTCCIT
            O_ZTCCIT           =    O_ZTCCIT
            UPD_ZTCCIT         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTCCIT       = ''
      tables
            ICDTXT_ZTCCIT           =    IT_CDTXT.

ENDFUNCTION.
