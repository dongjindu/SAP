*----------------------------------------------------------------------*
*   INCLUDE MZAPP272_HPC_ORDER_PLANF01                                 *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX
*&---------------------------------------------------------------------*
FORM SET_LISTBOX_APP272.
  PERFORM LIST_BOX_P_MAKER_APP272.
  PERFORM VRM_SET_VALUES_APP272.
  PERFORM LIST_BOX_P_PLNT_APP272.
  PERFORM VRM_SET_VALUES_APP272.
  PERFORM LIST_BOX_P_CARX_APP272.
  PERFORM VRM_SET_VALUES_APP272.
  PERFORM LIST_BOX_P_GUBN_APP272.
  PERFORM VRM_SET_VALUES_APP272.
*  PERFORM LIST_BOX_P_HPCC_APP272.


ENDFORM.                    " SET_LISTBOX
*&---------------------------------------------------------------------*
*&      Form  VRM_SET_VALUES
*&---------------------------------------------------------------------*
FORM VRM_SET_VALUES_APP272.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = WA_NAME_APP272
            VALUES = IT_LIST_APP272.

ENDFORM.                    " VRM_SET_VALUES
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_P_MAKER_APP272
*&---------------------------------------------------------------------*
FORM LIST_BOX_P_MAKER_APP272.
  CLEAR : IT_LIST_APP272. REFRESH : IT_LIST_APP272.
  WA_VALUE_APP272-KEY  = 'HMC'.
  WA_VALUE_APP272-TEXT = 'Hyundai Motor Manufacturing Alabama'.
  APPEND WA_VALUE_APP272 TO IT_LIST_APP272.
  WA_NAME_APP272 = 'P_MAKER_APP272'.

ENDFORM.                    " LIST_BOX_P_MAKER_APP272
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_P_PLNT_APP272
*&---------------------------------------------------------------------*
FORM LIST_BOX_P_PLNT_APP272.
  DATA: L_INDEX(1) TYPE N.
  CLEAR : IT_LIST_APP272. REFRESH : IT_LIST_APP272.
  DO 7 TIMES.
    L_INDEX = SY-INDEX.
    WA_VALUE_APP272-KEY = L_INDEX.
*  APPEND LT_PLNT. CLEAR LT_PLNT.
    CONCATENATE L_INDEX ' PLANT' INTO WA_VALUE_APP272-TEXT.
    APPEND WA_VALUE_APP272 TO IT_LIST_APP272.
  ENDDO.
  WA_NAME_APP272 = 'ZTBM_ABXPLIDT-PLNT'.
  DELETE IT_LIST_APP272 WHERE KEY IS INITIAL.
ENDFORM.                    " LIST_BOX_P_PLNT_APP272
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_P_CARX_APP272
*&---------------------------------------------------------------------*
FORM LIST_BOX_P_CARX_APP272.
  CLEAR : IT_LIST_APP272. REFRESH : IT_LIST_APP272.
  WA_VALUE_APP272-KEY = 'DA'.
  WA_VALUE_APP272-TEXT = 'EF-SONATA'.
  APPEND WA_VALUE_APP272 TO IT_LIST_APP272.
  WA_VALUE_APP272-KEY = 'EM'.
  WA_VALUE_APP272-TEXT = 'SANTAFE'.
  APPEND WA_VALUE_APP272 TO IT_LIST_APP272.
  WA_NAME_APP272 = 'ZTBM_ABXPLIDT-CARX'.
  DELETE IT_LIST_APP272 WHERE KEY IS INITIAL.
ENDFORM.                    " LIST_BOX_P_CARX_APP272
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_P_GUBN_APP272
*&---------------------------------------------------------------------*
FORM LIST_BOX_P_GUBN_APP272.
  CLEAR : IT_LIST_APP272. REFRESH : IT_LIST_APP272.
  WA_VALUE_APP272-KEY = 'P'.
  WA_VALUE_APP272-TEXT = 'P TABLE'.
  APPEND WA_VALUE_APP272 TO IT_LIST_APP272.
  WA_VALUE_APP272-KEY = 'B'.
  WA_VALUE_APP272-TEXT = 'B TABLE'.
  APPEND WA_VALUE_APP272 TO IT_LIST_APP272.
  WA_VALUE_APP272-KEY = 'Q'.
  WA_VALUE_APP272-TEXT = 'Q TABLE'.
  APPEND WA_VALUE_APP272 TO IT_LIST_APP272.
  WA_NAME_APP272 = 'ZTBM_ABXPLIDT-GUBN'.
  DELETE IT_LIST_APP272 WHERE KEY IS INITIAL.
ENDFORM.                    " LIST_BOX_P_GUBN_APP272
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
FORM DISPLAY_APP272.
  MOVE-CORRESPONDING IT_PCLD_APP272 TO ZTBM_ABXPCLDT.
ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_ABXPCLDT
*&---------------------------------------------------------------------*
FORM READ_ZTBM_ABXPCLDT_APP272.
  REFRESH IT_PCLD_APP272. CLEAR IT_PCLD_APP272.

  SELECT CARX GUBN HPCC UPGN HEAD SAYA ALOC
         TORQ TORN TORX MARK SECU LPRT OPRT
         DEPO LOCA COLM OPT1 OPT2 OPT3 OPT4
         ZPP_USER
       FROM  ZTBM_ABXPCLDT
       INTO TABLE IT_PCLD_APP272
       WHERE CARX EQ ZTBM_ABXPLIDT-CARX
       AND   GUBN EQ ZTBM_ABXPLIDT-GUBN.

ENDFORM.                    " READ_ZTBM_ABXPCLDT
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS_APP272.
  CASE ZTBM_ABXPLIDT-GUBN.
    WHEN 'P'.
      PERFORM READ_ZTBM_ABXPCLDT_APP272.
    WHEN 'B'.
      PERFORM READ_ZTBM_ABXPCLDT_APP272.
    WHEN 'Q'.
      PERFORM READ_ZTBM_ABXPCLDT_APP272.
  ENDCASE.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_9002
*&---------------------------------------------------------------------*
FORM DISPLAY_APP272_02.

ENDFORM.                    " DISPLAY_9002
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_9003
*&---------------------------------------------------------------------*
FORM display_APP272_03.

ENDFORM.                    " DISPLAY_9003
