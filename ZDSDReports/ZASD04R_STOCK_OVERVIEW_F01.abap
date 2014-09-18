*----------------------------------------------------------------------*
*   INCLUDE ZASD04R_STOCK_OVERVIEW_F01                                 *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.

DATA: BEGIN OF L_T_E1CUVAL OCCURS 0.
        INCLUDE STRUCTURE E1CUVAL.
DATA: END OF L_T_E1CUVAL.

DATA: BEGIN OF L_T_E1CUCFG  OCCURS 0.
        INCLUDE STRUCTURE E1CUCFG.
DATA: END OF L_T_E1CUCFG.

DATA: BEGIN OF L_T_E1CUINS  OCCURS 0.
        INCLUDE STRUCTURE E1CUINS.
DATA: END OF L_T_E1CUINS.

DATA: BEGIN OF L_T_E1CUCOM  OCCURS 0.
        INCLUDE STRUCTURE E1CUCOM.
DATA: END OF L_T_E1CUCOM.

DATA: L_TABIX LIKE SY-TABIX.

  SELECT A~LGORT A~MATNR A~VBELN A~KALAB C~CUOBJ
  INTO   CORRESPONDING FIELDS OF TABLE LTAB
  FROM   MSKA AS A
  INNER  JOIN MARA AS B
  ON     A~MATNR EQ B~MATNR

  INNER  JOIN VBAP AS C
  ON     A~VBELN EQ C~VBELN

  WHERE  A~WERKS = P_WERKS
  AND    A~LGORT IN S_LGORT
  AND    A~KALAB >= 1
  AND    B~MTART EQ 'FERT'
  AND    C~POSNR EQ '10'.


  CLEAR: LTAB.
  LOOP AT LTAB.
     L_TABIX = SY-TABIX.
     CLEAR: L_T_E1CUVAL.
     REFRESH: L_T_E1CUVAL.

     CALL FUNCTION 'CUXM_GET_CONFIGURATION'
       EXPORTING
         INSTANCE                          = LTAB-CUOBJ
         WERKS                             = P_WERKS
*      IMPORTING
*        ET_RETURN                          =
       TABLES
         T_E1CUCFG                          = L_T_E1CUCFG
         T_E1CUINS                          = L_T_E1CUINS
         T_E1CUVAL                          = L_T_E1CUVAL
         T_E1CUCOM                          = L_T_E1CUCOM
*      EXCEPTIONS
*        INSTANCE_NOT_FOUND                 = 1
*        INTERNAL_ERROR                     = 2
*        INSTANCE_IS_A_CLASSIFICATION       = 3
*        OTHERS                             = 4
               .
     IF SY-SUBRC = 0.
        READ TABLE L_T_E1CUVAL WITH KEY CHARC = 'COLOREXT'.
        IF SY-SUBRC = 0.
           LTAB-EXTC = L_T_E1CUVAL-VALUE.
        ENDIF.

        READ TABLE L_T_E1CUVAL WITH KEY CHARC = 'COLORINT'.
        IF SY-SUBRC = 0.
           LTAB-INTC = L_T_E1CUVAL-VALUE.
        ENDIF.
     ENDIF.

*     MOVE-CORRESPONDING LTAB TO T_ITAB2.
*     APPEND T_ITAB2 TO ITAB2.

     MOVE-CORRESPONDING LTAB TO T_ITAB1.
     COLLECT T_ITAB1 INTO ITAB1.

     MODIFY LTAB INDEX L_TABIX.
     CLEAR: LTAB, T_ITAB2, T_ITAB1, L_TABIX.
  ENDLOOP.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_DATA.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_SCREEN.

  SORT ITAB1.
  DESCRIBE TABLE ITAB1 LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
  ELSE.
    CALL SCREEN 9000.
  ENDIF.

ENDFORM.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZATION.

ENDFORM.                    " INITIALIZATION
