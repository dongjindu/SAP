FUNCTION ZIM_CHANGE_DOCUMENT_CCIT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTIVIT) LIKE  ZTIVIT STRUCTURE  ZTIVIT
*"     REFERENCE(O_ZTIVIT) LIKE  ZTIVIT STRUCTURE  ZTIVIT
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : IT_CDTXT.
   clear : IT_CDTXT.
   objecid = N_ZTIVIT+3(15).

   CALL FUNCTION 'ZICCIT_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTIVIT           =    N_ZTIVIT
            O_ZTIVIT           =    O_ZTIVIT
            UPD_ZTIVIT         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZICCIT       = ''
      tables
            ICDTXT_ZICCIT           =    IT_CDTXT.

ENDFUNCTION.
