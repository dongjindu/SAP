FORM CD_CALL_ZTBSEG                        .
   IF   ( UPD_ZTBSEG                         NE SPACE )
     OR ( UPD_ICDTXT_ZTBSEG          NE SPACE )
   .
     CALL FUNCTION 'ZTBSEG_WRITE_DOCUMENT         ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTBSEG
                      = *ZTBSEG
                  N_ZTBSEG
                      = ZTBSEG
                  UPD_ZTBSEG
                      = UPD_ZTBSEG
                  UPD_ICDTXT_ZTBSEG
                      = UPD_ICDTXT_ZTBSEG
          TABLES  ICDTXT_ZTBSEG
                      = ICDTXT_ZTBSEG
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
