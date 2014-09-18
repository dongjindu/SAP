FUNCTION ZIM_CHANGE_DOCUMENT_ZTIDSHS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTIDSHS) LIKE  ZTIDSHS STRUCTURE  ZTIDSHS
*"     REFERENCE(O_ZTIDSHS) LIKE  ZTIDSHS STRUCTURE  ZTIDSHS
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : IT_CDTXT.
   clear : IT_CDTXT.
   objecid = N_ZTIDSHS+3(18).

   CALL FUNCTION 'ZTIDSHS_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTIDSHS          =    N_ZTIDSHS
            O_ZTIDSHS          =    O_ZTIDSHS
            UPD_ZTIDSHS        =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTIDSHS      = ''
      tables
            ICDTXT_ZTIDSHS          =    it_CDTXT.

ENDFUNCTION.
