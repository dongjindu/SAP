FUNCTION ZIM_CHANGE_DOCUMENT_ZTIDSHSD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTIDSHSD) LIKE  ZTIDSHSD STRUCTURE  ZTIDSHSD
*"     REFERENCE(O_ZTIDSHSD) LIKE  ZTIDSHSD STRUCTURE  ZTIDSHSD
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : IT_CDTXT.
   clear : IT_CDTXT.
   objecid = N_ZTIDSHSD+3(21).

   CALL FUNCTION 'ZTIDSHSD_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTIDSHSD         =    N_ZTIDSHSD
            O_ZTIDSHSD         =    O_ZTIDSHSD
            UPD_ZTIDSHSD       =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTIDSHSD     = ''
      tables
            ICDTXT_ZTIDSHSD         =    it_CDTXT.

ENDFUNCTION.
