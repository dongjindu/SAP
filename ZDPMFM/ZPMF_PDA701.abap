FUNCTION ZPMF_PDA701 .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SY-SUBRC
*"  TABLES
*"      T701 STRUCTURE  ZSPM_PDA701 OPTIONAL
*"      T701R STRUCTURE  ZSPM_PDA701R OPTIONAL
*"----------------------------------------------------------------------
  DATA : IT_T357 LIKE TABLE OF T357 WITH HEADER LINE,
         IT_T499S LIKE TABLE OF T499S WITH HEADER LINE,
         IT_T001L LIKE TABLE OF T001L WITH HEADER LINE.
  DATA: L_TABIX LIKE SY-TABIX.

  LOOP AT T701.
    CASE T701-TYPE.
      WHEN 'A'.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_T357
               FROM T357
              WHERE WERKS EQ T701-WERKS.
        LOOP AT IT_T357.
          T701R-TYPE = T701-TYPE.
          T701R-CODE = IT_T357-BEBER.
          T701R-TEXT = IT_T357-FING.
          APPEND T701R. CLEAR T701R.
        ENDLOOP.
      WHEN 'B'.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_T499S
               FROM T499S
              WHERE WERKS EQ T701-WERKS.

** Changed on 04/11/11

*      LOOP AT IT_T499S.
*        T701R-TYPE = T701-TYPE.
*        T701R-CODE = IT_T499S-STAND.
*        T701R-TEXT = IT_T499S-KTEXT.
*        APPEND T701R. CLEAR T701R.
*      ENDLOOP.

        LOOP AT IT_T499S.
          CASE T701-BEBER.
            WHEN '100'.
              IF IT_T499S-KTEXT+0(1) = 'S'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T499S-STAND.
                T701R-TEXT = IT_T499S-KTEXT.
                APPEND T701R.
              ENDIF.
            WHEN '200'.
              IF IT_T499S-KTEXT+0(1) = 'B'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T499S-STAND.
                T701R-TEXT = IT_T499S-KTEXT.
                APPEND T701R.
              ENDIF.
            WHEN '300'.
              IF IT_T499S-KTEXT+0(1) = 'P'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T499S-STAND.
                T701R-TEXT = IT_T499S-KTEXT.
                APPEND T701R.
              ENDIF.
            WHEN '400'.
              IF IT_T499S-KTEXT+0(1) = 'T'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T499S-STAND.
                T701R-TEXT = IT_T499S-KTEXT.
                APPEND T701R.
              ENDIF.
            WHEN '500' OR '502'.
              IF IT_T499S-KTEXT+0(1) = 'E'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T499S-STAND.
                T701R-TEXT = IT_T499S-KTEXT.
                APPEND T701R.
              ENDIF.
            WHEN '600'.
              IF IT_T499S-KTEXT+0(1) = 'U'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T499S-STAND.
                T701R-TEXT = IT_T499S-KTEXT.
                APPEND T701R.
              ENDIF.
            WHEN '700'.
              IF IT_T499S-KTEXT+0(1) = 'Q'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T499S-STAND.
                T701R-TEXT = IT_T499S-KTEXT.
                APPEND T701R.
              ENDIF.

          ENDCASE.
          CLEAR T701R.
        ENDLOOP.

** End

      WHEN 'C'.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_T001L
               FROM T001L
              WHERE WERKS EQ T701-WERKS.
        LOOP AT IT_T001L.

** Changed on 04/11/11
*          T701R-TYPE = T701-TYPE.
*          T701R-CODE = IT_T001L-LGORT.
*          T701R-TEXT = IT_T001L-LGOBE.
*          APPEND T701R. CLEAR T701R.
          L_TABIX = SY-TABIX.
          CASE IT_T001L-WERKS.
            WHEN 'P001'.
              IF IT_T001L-LGORT BETWEEN 'P600' AND 'P699'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T001L-LGORT.
                T701R-TEXT = IT_T001L-LGOBE.
                APPEND T701R.

              ELSE.
                DELETE IT_T001L INDEX L_TABIX.
              ENDIF.
            WHEN 'E001'.
              IF IT_T001L-LGORT BETWEEN 'E600' AND 'E699'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T001L-LGORT.
                T701R-TEXT = IT_T001L-LGOBE.
                APPEND T701R.

              ELSE.
                DELETE IT_T001L INDEX L_TABIX.
              ENDIF.
** Changed on 12/13/11
            WHEN 'E002'.
** FOR E002
            IF IT_T001L-LGORT BETWEEN 'N600' AND 'N699'.
                T701R-TYPE = T701-TYPE.
                T701R-CODE = IT_T001L-LGORT.
                T701R-TEXT = IT_T001L-LGOBE.
                APPEND T701R.

              ELSE.
                DELETE IT_T001L INDEX L_TABIX.
              ENDIF.
** END FOR E002
            WHEN OTHERS.
              DELETE IT_T001L INDEX L_TABIX.
          ENDCASE.
          CLEAR T701R.
** End of change
        ENDLOOP.
    ENDCASE.
  ENDLOOP.

  DESCRIBE TABLE T701R LINES SY-INDEX.
  IF SY-INDEX = 0.
    SUBRC = 4.
  ELSE.
    SUBRC = 0.
  ENDIF.

  SORT T701R.
ENDFUNCTION.
