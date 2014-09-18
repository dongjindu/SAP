FORM CD_CALL_ZICIVHD                       .
   IF   ( UPD_ZTCIVHD                        NE SPACE )
     OR ( UPD_ICDTXT_ZICIVHD         NE SPACE )
   .
     CALL FUNCTION 'ZICIVHD_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTCIVHD
                      = *ZTCIVHD
                  N_ZTCIVHD
                      = ZTCIVHD
                  UPD_ZTCIVHD
                      = UPD_ZTCIVHD
                  UPD_ICDTXT_ZICIVHD
                      = UPD_ICDTXT_ZICIVHD
          TABLES  ICDTXT_ZICIVHD
                      = ICDTXT_ZICIVHD
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
