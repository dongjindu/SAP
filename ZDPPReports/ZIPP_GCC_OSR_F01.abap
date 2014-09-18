*---------------------------------------------------------------------*
*       FORM P2000_GET_DATA                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM P2000_GET_DATA.
  DATA : LT_WOSUM LIKE TABLE OF ZTPP_WOSUM WITH HEADER LINE.
  DATA : LV_OBJEK LIKE AUSP-OBJEK .
*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_WOSUM
    FROM ZTPP_WOSUM
    WHERE WOMODDATE IN P_DATUM
      AND WO_SER    IN P_WOSER
      AND NATION    IN P_NATN
      AND DEALER    IN P_DEAL
      AND MODQTY    NE 0
      AND SALES NE ''.



  LOOP AT LT_WOSUM.

    CHECK LT_WOSUM-INITQTY NE LT_WOSUM-MODQTY .
    CHECK LT_WOSUM-DEALER+0(1) EQ 'A' OR
          LT_WOSUM-DEALER+0(1) EQ 'B'.

    CONCATENATE '20' LT_WOSUM-WO_SER+1(4) INTO
    GT_ITEM-PACK .
    GT_ITEM-GRDE = ''.
    GT_ITEM-EXCL = LT_WOSUM-EXTC.
    GT_ITEM-INCL = LT_WOSUM-INTC.
    CONCATENATE LT_WOSUM-WO_SER
                LT_WOSUM-NATION
                LT_WOSUM-DEALER
                INTO GT_ITEM-WKNO.

    GT_ITEM-PLNT = '5N'."C_PLNT.
*    CONCATENATE LT_WOSUM-WO_SER+0(5)
*                LT_WOSUM-NATION
*                LT_WOSUM-DEALER
*    INTO  GT_ITEM-REFE.

    LV_OBJEK = GT_ITEM-WKNO.
**#2 GET AUSP-ATWRT DATA
    PERFORM P2100_GETSINGLE_ATWRT USING :
            LV_OBJEK GT_ITEM-DIST 'P_DESTINATION_CODE',
            LV_OBJEK GT_ITEM-MCCD 'P_MI',
            LV_OBJEK GT_ITEM-OCCN 'P_OCN',
            LV_OBJEK GT_ITEM-REFE 'P_LC_NO'.


    GT_ITEM-VQTY = LT_WOSUM-INITQTY.
    GT_ITEM-CQTY = LT_WOSUM-MODQTY.

    GT_ITEM-OCHD = LT_WOSUM-WOMODDATE.
    GT_ITEM-CRDT = SY-DATUM.
    GT_ITEM-SNDT = SY-DATUM.

    APPEND GT_ITEM.

  ENDLOOP.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM P2100_GETSINGLE_ATWRT                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_OBJEK                                                       *
*  -->  P_ATWRT                                                       *
*  -->  P_ATNAM                                                       *
*---------------------------------------------------------------------*
FORM P2100_GETSINGLE_ATWRT USING P_OBJEK P_ATWRT P_ATNAM.

  DATA : LV_ATINN LIKE AUSP-ATINN.
  PERFORM P4000_CONVERSION_ATINN USING P_ATNAM LV_ATINN.

  SELECT SINGLE ATWRT INTO P_ATWRT
  FROM AUSP
  WHERE KLART = '001'
    AND ATINN = LV_ATINN
    AND OBJEK = P_OBJEK
    AND MAFID EQ 'O'.

ENDFORM.                    " P2210_GETSINGLE_ATWRT


*---------------------------------------------------------------------*
*       FORM P4000_CONVERSION_ATINN                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*  -->  P_ATINN                                                       *
*---------------------------------------------------------------------*
FORM P4000_CONVERSION_ATINN USING P_VALUE P_ATINN .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = P_VALUE
       IMPORTING
            OUTPUT = P_ATINN.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P3000_SEND_EAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_SEND_EAI.
  DATA:   LV_RESULT(1),
          LV_MSGTXT(100).

  CALL FUNCTION 'Z_FPP_GCC_OSR'
    DESTINATION C_DEST
    IMPORTING
      FLAG                  = LV_RESULT
    TABLES
      INPUT                 = GT_ITEM
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE LV_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE LV_MSGTXT.

  IF SY-SUBRC = 0.
    IF LV_RESULT = 'S'.
      LV_MSGTXT = 'Data successfully sent out'.
      MESSAGE S009 WITH LV_MSGTXT.
    ELSE.
      LV_MSGTXT =  'Data unsuccessfully sent out'.
      MESSAGE E009 WITH LV_MSGTXT.
    ENDIF.
  ELSE.
    MESSAGE S009 WITH LV_MSGTXT.
  ENDIF.


ENDFORM.                    " P3000_SEND_EAI
