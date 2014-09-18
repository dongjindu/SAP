************************************************************************
* Program Name      : ZACO53U_MIPCC
* Author            : Eun Hwa , Jung
* Creation Date     : 2004.03.31
* Specifications By : Bong-Doo , Moon
* Pattern           : Report 1-1
* Development Request No : UD
* Addl Documentation:
* Description       : Create MIP PCC

* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT zaco53u_mipcc MESSAGE-ID zmco.

TABLES : mbew, t001w, keko.
DATA: BEGIN OF it_mara_m OCCURS 0,
       matnr TYPE mara-matnr,
       werks TYPE marc-werks,
     END OF it_mara_m.

DATA: BEGIN OF it_process_post OCCURS 0,
       matnr TYPE mara-matnr,
       werks TYPE marc-werks,
     END OF it_process_post.

DATA: BEGIN OF it_post OCCURS 0,
       matnr TYPE mara-matnr,
       werks TYPE marc-werks,
     END OF it_post.

DATA : it_l_e_vkks0	LIKE TABLE OF vkks0
                          WITH HEADER LINE.

DATA it_result TYPE  ckml_t_process.

*- BDC TABLE
DATA  BEGIN OF bdc_tab OCCURS 0.     " BDCDATA TABLE.
        INCLUDE STRUCTURE bdcdata.
DATA  END OF bdc_tab.

DATA  BEGIN OF messtab OCCURS 0.     " BDC MESSAGE TABLE.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA  END OF messtab.

DATA : message(100),
           bdc_mode VALUE 'N'.    " 'A'.

DATA : BEGIN OF it_mess OCCURS 0,
        message(100),
       END OF it_mess.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS :
             mark(1) DEFAULT 'Y'.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK bl1.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Read Information from MARA+MARC
  PERFORM read_mara_marc.
* Creation order ( case 1,2 )
  PERFORM call_transaction.

*&---------------------------------------------------------------------*
*&      Form  READ_MARA_MARC
*&---------------------------------------------------------------------*
*       Read Information from MARA+MARC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mara_marc.

  IF mark = 'X'.
  ELSE.
    MESSAGE e000(zmco) WITH ' input value error '.
  ENDIF.


  CLEAR : it_mara_m, it_mara_m[].

  SELECT  marc~matnr marc~werks
          INTO CORRESPONDING FIELDS OF TABLE it_mara_m
          FROM mara INNER JOIN marc
            ON mara~matnr = marc~matnr
          WHERE mtart EQ 'HALB'
            AND NOT matkl IN ('BIP','BIW')
*// Mod. by Hyung Jin Youn 2004.04.07
* Except UPGVC ( No Cost = 'X' : Costing 1 View )
            AND marc~ncost NE 'X'
*// End. Of Mod.
**// Mod. by Hyung Jin Youn 2004.04.07
** Except Engine(in Plant 'P001')
**               ( Procurement Type         = 'F'
**                 Special Procurement type = '40'
**                 Plant = 'P001' )
*            AND NOT (
*                        MARC~BESKZ = 'F'
*                    AND MARC~SOBSL = '40'
*                    AND MARC~WERKS = 'P001'
*                    ).
**// End. Of Mod.
*// Mod. by eun-hwa 2004.05.14
* Except         Procurement Type         = 'F'
            AND NOT marc~beskz = 'F'.
*// End. Of Mod.

  CLEAR it_mara_m.

*// Mod. by Hyung Jin Youn 2004.04.07
* If standard price <= 0.00, Do not Create PCC order
  DATA: l_idx LIKE sy-index.
  LOOP AT it_mara_m.
    l_idx = sy-tabix.

    CLEAR t001w.
    SELECT SINGLE * FROM  t001w
                   WHERE  werks = it_mara_m-werks.


    CLEAR mbew.
    SELECT SINGLE
          * FROM mbew
           WHERE matnr = it_mara_m-matnr
             AND bwkey = t001w-bwkey
             AND stprs EQ space.
    IF sy-subrc = 0.
      WRITE : / 'No standard price is found'
               , it_mara_m-matnr
               , it_mara_m-werks.
      DELETE it_mara_m INDEX l_idx.
    ELSE.

* check cost est.
* Product Costing - Header Data
      CLEAR keko.
      SELECT SINGLE * FROM keko
              WHERE matnr = it_mara_m-matnr
                AND bwkey = t001w-bwkey
                AND freig = 'X'.  "released

      IF sy-subrc <> 0.
        WRITE : / 'Standard cost is not released'
                 , it_mara_m-matnr
                 , it_mara_m-werks.
        DELETE it_mara_m INDEX l_idx.
      ENDIF.

    ENDIF.

    CLEAR it_mara_m.
  ENDLOOP.

*// End. Of Mod.




  CLEAR : it_post, it_post[], it_process_post, it_process_post[].
* Check order
  LOOP AT it_mara_m.

    CLEAR : it_l_e_vkks0, it_l_e_vkks0[].
    CALL FUNCTION 'KK_F_PKOSA_FIND'
     EXPORTING
       i_matnr                     = it_mara_m-matnr
       i_werks                     = it_mara_m-werks
       i_pwerk                     = it_mara_m-werks
     TABLES
       e_vkks0                     = it_l_e_vkks0
*   E_PKOSA                     =
     EXCEPTIONS
       none_found                  = 1
       wrong_input                 = 2
       none_picked                 = 3
       wrong_rule                  = 4
       rsh_not_valid               = 5
       wrong_characteristics       = 6
       no_rule                     = 7
       version_not_valid           = 8
       OTHERS                      = 9
              .
    IF sy-subrc = 0.      " if order created....
      DELETE it_mara_m.
      CONTINUE.

    ELSE.

      CLEAR : it_result, it_result[].
      CALL FUNCTION 'CKML_MGV_PROCESSES_READ'
        EXPORTING
          iv_werks                    = it_mara_m-werks
          iv_matnr                    = it_mara_m-matnr
*         IV_BTYP                     = 'BF'
       IMPORTING
         et_process_tbl              =  it_result.
*      IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.

*     case : new-creation order
      IF it_result[] IS INITIAL.
        MOVE-CORRESPONDING it_mara_m TO it_post.
        APPEND it_post.
        CLEAR  it_post.
*     case : re-creation order deleted
      ELSE.
        MOVE-CORRESPONDING it_mara_m TO it_process_post.
        APPEND it_process_post.
        CLEAR  it_process_post.
      ENDIF.

    ENDIF.

  ENDLOOP.
  CLEAR it_mara_m.



ENDFORM.                    " READ_MARA_MARC
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_transaction.

  IF ( it_process_post[] IS INITIAL )  AND ( it_post[] IS INITIAL ) .
    WRITE : / ' Create MIP PCC data not found '.
    STOP.
  ENDIF.

* case 1 : creation  order
  LOOP AT it_post.
    CLEAR : bdc_tab, bdc_tab[].
    PERFORM dynpro USING :
            'X' 'SAPMKOSA'       '0101',
            ' ' 'PKOSA-MATNR'    it_post-matnr,
            ' ' 'PKOSA-WERKS'    it_post-werks,
            ' ' 'PKOSA-AUART'    'RM01',
            ' ' 'BDC_OKCODE'     '/00'.

    PERFORM dynpro USING :
            'X' 'SAPMSSY0'      '0120',
            ' ' 'BDC_CURSOR'    '06/16',
            ' ' 'BDC_OKCODE'    '=&NT1'.

    PERFORM dynpro USING :
             'X' 'SAPMKOSA'      '0111',
            ' ' 'BDC_OKCODE'    '=SAVE',
            ' ' 'PKOSA-KTEXT'   'Collective Pool'.

    CLEAR : messtab, messtab[].
    CALL TRANSACTION 'KKF6'  USING bdc_tab
                         MODE   bdc_mode
                         UPDATE 'S'
                         MESSAGES INTO messtab.

    WRITE / : .
    WRITE  : 1(11) 'Material:' , it_post-matnr.

    CLEAR : message.
    LOOP AT messtab.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = messtab-msgid
                msgnr               = messtab-msgnr
                msgv1               = messtab-msgv1
                msgv2               = messtab-msgv2
                msgv3               = messtab-msgv3
                msgv4               = messtab-msgv4
           IMPORTING
                message_text_output = message.
      WRITE / :  message.
    ENDLOOP.

  ENDLOOP.

* case 2 : re-creation  order deleted ( when you re-create order deleted
  LOOP AT it_process_post.
    CLEAR : bdc_tab, bdc_tab[].

    PERFORM dynpro USING :
            'X' 'SAPMKOSA'       '0101',
            ' ' 'PKOSA-MATNR'    it_process_post-matnr,
            ' ' 'PKOSA-WERKS'    it_process_post-werks,
            ' ' 'PKOSA-AUART'    'RM01',
            ' ' 'BDC_OKCODE'     '/00'.

    PERFORM dynpro USING :
            'X' 'SAPMSSY0'      '0120',
            ' ' 'BDC_CURSOR'    '04/17',
            ' ' 'BDC_OKCODE'    '=&NT1'.

    PERFORM dynpro USING :
             'X' 'SAPMKOSA'      '0111',
            ' ' 'BDC_OKCODE'    '=SAVE',
            ' ' 'PKOSA-KTEXT'   'Collective Pool'.

    CLEAR : messtab, messtab[].
    CALL TRANSACTION 'KKF6'  USING bdc_tab
                         MODE   bdc_mode
                         UPDATE 'S'
                         MESSAGES INTO messtab.

    WRITE / : .
    WRITE  : 1(11) 'Material:' , it_process_post-matnr.

    CLEAR : message.
    LOOP AT messtab.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = messtab-msgid
                msgnr               = messtab-msgnr
                msgv1               = messtab-msgv1
                msgv2               = messtab-msgv2
                msgv3               = messtab-msgv3
                msgv4               = messtab-msgv4
           IMPORTING
                message_text_output = message.
      WRITE / :  message.
    ENDLOOP.
  ENDLOOP.




*  LOOP AT IT_MARA_M.
*    CLEAR : BDC_TAB, BDC_TAB[].
*
**    IF SY-TABIX = 3.    " TEST
**      EXIT.
**    ELSE.
*    PERFORM DYNPRO USING :
*            'X' 'SAPMKOSA'       '0101',
*            ' ' 'PKOSA-MATNR'    IT_MARA_M-MATNR,
*            ' ' 'PKOSA-WERKS'    IT_MARA_M-WERKS,
*            ' ' 'PKOSA-AUART'    'RM01',
*            ' ' 'BDC_OKCODE'     '/00'.
*
*    PERFORM DYNPRO USING :
*            'X' 'SAPMSSY0'      '0120',
*            ' ' 'BDC_CURSOR'    '06/16',
*            ' ' 'BDC_OKCODE'    '=&NT1'.
*
*    PERFORM DYNPRO USING :
*             'X' 'SAPMKOSA'      '0111',
*            ' ' 'BDC_OKCODE'    '=SAVE'.
*
*    CLEAR : MESSTAB, MESSTAB[].
*    CALL TRANSACTION 'KKF6'  USING BDC_TAB
*                         MODE   BDC_MODE
*                         UPDATE 'S'
*                         MESSAGES INTO MESSTAB.
**    ENDIF.
*
*    WRITE / : .
*    WRITE  : 1(11) 'Material:' , IT_MARA_M-MATNR.
*
*    CLEAR : MESSAGE.
**    read table messtab index 1.
*    LOOP AT MESSTAB.
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                MSGID               = MESSTAB-MSGID
*                MSGNR               = MESSTAB-MSGNR
*                MSGV1               = MESSTAB-MSGV1
*                MSGV2               = MESSTAB-MSGV2
*                MSGV3               = MESSTAB-MSGV3
*                MSGV4               = MESSTAB-MSGV4
*           IMPORTING
*                MESSAGE_TEXT_OUTPUT = MESSAGE.
*      WRITE / :  MESSAGE.
*    ENDLOOP.
*
**  ENDIF.
*  ENDLOOP.
*


ENDFORM.                    " CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0203   text
*      -->P_0204   text
*      -->P_0205   text
*----------------------------------------------------------------------*
FORM dynpro USING     dynbegin name value.

  IF dynbegin  = 'X'.
    CLEAR bdc_tab.
    MOVE : name  TO bdc_tab-program,
           value TO bdc_tab-dynpro,
           'X'   TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR bdc_tab.
    MOVE : name  TO bdc_tab-fnam,
           value TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.

ENDFORM.                    " dynpro
