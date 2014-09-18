FUNCTION ZIM_CHANGE_DOCUMENT_BLCST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBLCST) LIKE  ZTBLCST STRUCTURE  ZTBLCST
*"     REFERENCE(O_ZTBLCST) LIKE  ZTBLCST STRUCTURE  ZTBLCST
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : IT_CDTXT.
   clear : IT_CDTXT.
   objecid = N_ZTBLCST+3(15).

   CALL FUNCTION 'ZIBLCST_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBLCST           =    N_ZTBLCST
            O_ZTBLCST           =    O_ZTBLCST
            UPD_ZTBLCST         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIBLCST      = ''
      tables
            ICDTXT_ZIBLCST           =    it_CDTXT.

ENDFUNCTION.
