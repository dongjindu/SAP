FUNCTION ZIM_CHANGE_DOCUMENT_OFF.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTOFF) LIKE  ZTOFF STRUCTURE  ZTOFF
*"     REFERENCE(O_ZTOFF) LIKE  ZTOFF STRUCTURE  ZTOFF
*"     REFERENCE(N_ZTOFFFTX) LIKE  ZTOFFFTX STRUCTURE  ZTOFFFTX
*"       OPTIONAL
*"     REFERENCE(O_ZTOFFFTX) LIKE  ZTOFFFTX STRUCTURE  ZTOFFFTX
*"       OPTIONAL
*"----------------------------------------------------------------------
   DATA : OBJECID     LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR : IT_CDTXT.
   OBJECID = N_ZTOFF+3(10).

   CALL FUNCTION 'ZIOFDOC_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    OBJECID
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTOFF            =    N_ZTOFF
            O_ZTOFF            =    O_ZTOFF
            UPD_ZTOFF          =    UPD_CHNGIND
            N_ZTOFFFTX         =    N_ZTOFFFTX
            O_ZTOFFFTX         =    O_ZTOFFFTX
            UPD_ZTOFFFTX       =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIOFDOC      = ''
      tables
            ICDTXT_ZIOFDOC     =    it_CdtXT.

ENDFUNCTION.
