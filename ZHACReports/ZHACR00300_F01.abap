*----------------------------------------------------------------------*
***INCLUDE ZHACR20040_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FIND_TABLE_COLUMN_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_TABLE_COLUMN_RTN .

  REFRESH: GT_TABLE, GT_FIELDNAME.
  CLEAR:   GT_TABLE, GT_FIELDNAME.
  CLEAR:   G_WORD,   G_BWORD.

  LOOP AT GT_ABAP1 INTO GS_ABAP.

    TRANSLATE GS_ABAP TO UPPER CASE .

*   PGM Source# SPLIT ##.
    INCLUDE ZHACR20040_F02.

*   # ### ## ### 75###
    DO 75 TIMES.
      MOVE SY-INDEX TO G_NUMC2.

      CONCATENATE 'GS_STR-FLD' G_NUMC2 INTO G_FIELD.
      ASSIGN (G_FIELD) TO  <FS_FIELD1>.

      MOVE  <FS_FIELD1> TO G_VALUE.  "## ### ## ####

*     ##### SQL# ###### ##, ### ## ### #####.
      IF G_VALUE+0(1) EQ '"'.
        EXIT.
      ENDIF.

*      CHECK G_VALUE+0(1) NE '"'.

*     Period# ## ## # ### ####
      CLEAR: GS_STR2.
      SPLIT G_VALUE AT '.' INTO: G_VALUE  GS_STR2-FLD02  GS_STR2-FLD03.

*     ## ### #### ####..
      IF G_VALUE IS INITIAL. CONTINUE.  ENDIF.

*     SQL# ## ### ## # ## ### ###.
      IF <FS_FIELD1> EQ 'SELECT' OR
         <FS_FIELD1> EQ 'INTO'   OR
         <FS_FIELD1> EQ 'FROM'   OR
         <FS_FIELD1> EQ 'FOR'    OR
         <FS_FIELD1> EQ 'WHERE'  OR
         <FS_FIELD1> EQ 'INNER'  OR
         <FS_FIELD1> EQ 'OUTTER' OR
         <FS_FIELD1> EQ 'JOIN'   OR
         <FS_FIELD1> EQ 'ON'     OR
         <FS_FIELD1> EQ 'AS'     OR
         <FS_FIELD1> EQ 'ORDER'  OR
         <FS_FIELD1> EQ 'SORT'.

        MOVE G_WORD      TO G_BWORD. "## ##### ## ## ## ##.
        MOVE <FS_FIELD1> TO G_WORD.  "### ## ### ## ####.
        CONTINUE.
      ENDIF.

*     SQL# ## ## ### itab# ###.
      CASE G_WORD.
        WHEN 'SELECT' OR 'WHERE' OR 'ON' OR 'ORDER' OR 'SORT'.

*         ### ### ### #, alias# ##### ####.
          INCLUDE ZHACR20040_F03.

          IF GS_STR2-FLD02 IS NOT INITIAL.
            MOVE GS_STR2-FLD01 TO GT_FIELDNAME-ALIAS.
            MOVE GS_STR2-FLD02 TO GT_FIELDNAME-FIELDNAME.
            MOVE G_WORD        TO GT_FIELDNAME-AREA.
          ELSE.
            MOVE GS_STR2-FLD01 TO GT_FIELDNAME-FIELDNAME.
            MOVE G_WORD        TO GT_FIELDNAME-AREA.
          ENDIF.

          APPEND GT_FIELDNAME.

        WHEN 'AS'.
          CASE G_BWORD.
*           Select #### AS# ## ##..
*           ### ## SELECT##### ## ## # #.
            WHEN 'SELECT'.
              MOVE G_BWORD TO G_WORD.

*           ### ## AS# ## GT_TABLE# Alias# ### ##.
            WHEN 'FROM' OR 'JOIN'.
              DESCRIBE TABLE GT_TABLE LINES SY-TABIX.
              MOVE G_VALUE TO GT_TABLE-ALIAS.
              MODIFY GT_TABLE INDEX SY-TABIX TRANSPORTING ALIAS.
          ENDCASE.

        WHEN 'FROM' OR 'JOIN'.
          MOVE G_VALUE TO GT_TABLE-TABNAME.
          APPEND GT_TABLE.

*       ### ## ##.
        WHEN 'INTO' OR 'FOR'.

      ENDCASE.
    ENDDO.
  ENDLOOP.

  CLEAR: GT_TABLE, GT_FIELDNAME.

ENDFORM.                    " FIND_TABLE_COLUMN_RTN

*&---------------------------------------------------------------------*
*&      Form  UPDATE_TO_TABLE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TO_TABLE_RTN .

  REFRESH: GT_ASTB1, GT_ASTB2.
  CLEAR:   GS_ASTB1, GS_ASTB2.
  CLEAR:   GT_TABLE, GT_FIELDNAME.


  LOOP AT GT_TABLE WHERE ALIAS EQ SPACE.
    MOVE GT_TABLE-TABNAME TO GT_TABLE-ALIAS.
    MODIFY GT_TABLE.
  ENDLOOP.

  LOOP AT GT_TABLE.
*   #### alias# ###### alias# ##
*    IF GT_TABLE-ALIAS IS NOT INITIAL.
    LOOP AT GT_FIELDNAME WHERE ALIAS = GT_TABLE-ALIAS.
      INCLUDE ZHACR20040_F04.
    ENDLOOP.
*    ELSE.
    LOOP AT GT_FIELDNAME WHERE ALIAS = SPACE.
      INCLUDE ZHACR20040_F04.
    ENDLOOP.
*    ENDIF.
  ENDLOOP.

  MODIFY ZACTASTB1 FROM TABLE GT_ASTB1.
  MODIFY ZACTASTB2 FROM TABLE GT_ASTB2.
  MODIFY ZACTASTB3 FROM TABLE GT_ASTB3.

  COMMIT WORK.

  DATA : L_LINES  LIKE  SY-TABIX .
  DESCRIBE TABLE GT_ASTB1 LINES L_LINES .
  MESSAGE S011(ZAC01) WITH L_LINES .

ENDFORM.                    " UPDATE_TO_TABLE_RTN
