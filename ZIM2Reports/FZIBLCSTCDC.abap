FORM CD_CALL_ZIBLCST                       .
   IF   ( UPD_ZTBLCST                        NE SPACE )
     OR ( UPD_ICDTXT_ZIBLCST         NE SPACE )
   .
     CALL FUNCTION 'ZIBLCST_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTBLCST
                      = *ZTBLCST
                  N_ZTBLCST
                      = ZTBLCST
                  UPD_ZTBLCST
                      = UPD_ZTBLCST
                  UPD_ICDTXT_ZIBLCST
                      = UPD_ICDTXT_ZIBLCST
          TABLES  ICDTXT_ZIBLCST
                      = ICDTXT_ZIBLCST
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
