*----------------------------------------------------------------------*
*   INCLUDE MZEMMGM01E_6007I01                                         *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  IF sy-calld = 'X'. LEAVE PROGRAM. ENDIF.    "Call mode active (X)
  CASE sy-dynnr.
    WHEN 0100.
      CASE save_ok_code.
        WHEN 'EXIT'.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          LEAVE PROGRAM.
      ENDCASE.
    WHEN 0200.
      CASE save_ok_code.
        WHEN 'EXIT'.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          LEAVE TO TRANSACTION sy-tcode.
      ENDCASE.
    WHEN 9100.
      CASE save_ok_code.
        WHEN 'EXIT'.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          LEAVE PROGRAM.
      ENDCASE.
    WHEN 9200.
      CASE save_ok_code.
        WHEN 'EXIT'.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          LEAVE TO TRANSACTION sy-tcode.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  back  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CHECK save_ok_code = 'BACK'.
  IF sy-calld = 'X'. LEAVE PROGRAM. ENDIF.    "Call mode active (X)
  CASE sy-dynnr.
    WHEN 0100.
      LEAVE PROGRAM.
    WHEN 0200.
      LEAVE TO TRANSACTION sy-tcode.
    WHEN 9100.
      LEAVE PROGRAM.
    WHEN 9200.
      LEAVE TO TRANSACTION sy-tcode.
  ENDCASE.
ENDMODULE.                 " back  INPUT
*&---------------------------------------------------------------------*
*&      Module  ente  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ente INPUT.
  CHECK save_ok_code = 'ENTE'.
  CASE sy-dynnr.
    WHEN 0100.
      CHECK NOT ( ztmm_6007_01-subpart IS INITIAL OR
                  ztmm_6007_01-werks IS INITIAL ).
      SET PARAMETER ID 'ZPI_SUBPART' FIELD ztmm_6007_01-subpart.
      CASE sy-tcode.
        WHEN 'ZMME81'.   "Create
        WHEN 'ZMME82'.   "Change
          MOVE-CORRESPONDING gs_ztmm_6007_01 TO ztmm_6007_01.
        WHEN 'ZMME83'.   "Display
          MOVE-CORRESPONDING gs_ztmm_6007_01 TO ztmm_6007_01.
      ENDCASE.
      SET SCREEN 0200.
    WHEN 0200.
    WHEN 9100.
      CHECK NOT ( ztmm_6007_02-endpart IS INITIAL OR
                  ztmm_6007_02-werks IS INITIAL ).
      SET PARAMETER ID 'ZPI_ENDPART' FIELD ztmm_6007_02-endpart.
      CASE sy-tcode.
        WHEN 'ZMME84'.   "Create
          " First time we get data from zvmm_6007_05
          SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_zvmm_6007_05
            FROM zvmm_6007_05      "EMMGM01 BOM List
            WHERE matnr = ztmm_6007_02-endpart AND
                  werks = ztmm_6007_02-werks.

          IF sy-subrc = 0.
            LOOP AT gt_zvmm_6007_05 INTO gs_zvmm_6007_05.
              MOVE-CORRESPONDING gs_zvmm_6007_05 TO wa_zsmm_6007_02.
              wa_zsmm_6007_02-endpart = gs_zvmm_6007_05-matnr.
              wa_zsmm_6007_02-subpart = gs_zvmm_6007_05-idnrk.
              APPEND wa_zsmm_6007_02 TO it_zsmm_6007_02.
            ENDLOOP.
          ENDIF.

        WHEN 'ZMME85'.   "Change
          SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zsmm_6007_02
            FROM ztmm_6007_02
            WHERE endpart = ztmm_6007_02-endpart AND
                  werks   = ztmm_6007_02-werks.
          IF sy-subrc <> 0.
          ENDIF.
        WHEN 'ZMME86'.   "Display
          SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zsmm_6007_02
            FROM ztmm_6007_02
            WHERE endpart = ztmm_6007_02-endpart AND
                  werks   = ztmm_6007_02-werks.
          IF sy-subrc <> 0.

          ENDIF.
      ENDCASE.
      SET SCREEN 9200.
  ENDCASE.
ENDMODULE.                 " ente  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_key  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_key INPUT.
  CASE sy-dynnr.
    WHEN 0100.
*Existence Check
      CLEAR: gs_ztmm_6007_01.
      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF gs_ztmm_6007_01
        FROM ztmm_6007_01
        WHERE subpart = ztmm_6007_01-subpart AND
              werks   = ztmm_6007_01-werks.

      CASE sy-tcode.
        WHEN 'ZMME81'.
          IF sy-subrc = 0.
            MESSAGE e999(zmmm) WITH 'Already Data'
                                    ztmm_6007_01-subpart
                                    ztmm_6007_01-werks
                                    'exists !'.
          ELSE.
            SELECT SINGLE *
              INTO CORRESPONDING FIELDS OF gs_ztmm_6007_01
              FROM zvmm_6007_01
              WHERE subpart = ztmm_6007_01-subpart AND
                    werks   = ztmm_6007_01-werks.
            IF sy-subrc <> 0.
              MESSAGE e999(zmmm) WITH 'Invalid Key Value !'.
            ENDIF.
          ENDIF.
        WHEN 'ZMME82'.
          IF sy-subrc = 0.
          ELSE.
            MESSAGE e999(zmmm) WITH 'Data'
                                    ztmm_6007_01-subpart
                                    ztmm_6007_01-werks
                                    'does not exist !'.
          ENDIF.
        WHEN 'ZMME83'.
          IF sy-subrc = 0.
          ELSE.
            MESSAGE e999(zmmm) WITH 'Data'
                                    ztmm_6007_01-subpart
                                    ztmm_6007_01-werks
                                    'does not exist !'.
          ENDIF.
      ENDCASE.

    WHEN 9100.
*Existence Check
      CLEAR: gs_ztmm_6007_02.
      SELECT SINGLE endpart werks
        INTO CORRESPONDING FIELDS OF gs_ztmm_6007_02
        FROM ztmm_6007_02
        WHERE endpart = ztmm_6007_02-endpart AND
              werks   = ztmm_6007_02-werks.

      CASE sy-tcode.
        WHEN 'ZMME84'.    "Create
          IF sy-subrc = 0.
            MESSAGE e999(zmmm) WITH 'Already Data'
                                    ztmm_6007_01-subpart
                                    ztmm_6007_01-werks
                                    'exists !'.
          ELSE.
            SELECT SINGLE *
              INTO CORRESPONDING FIELDS OF gs_ztmm_6007_02
              FROM zvmm_6007_03        "(For End Part)
              WHERE endpart = ztmm_6007_02-endpart AND
                    werks   = ztmm_6007_02-werks.
            IF sy-subrc <> 0.
              MESSAGE e999(zmmm) WITH 'Invalid Key Value !'.
            ENDIF.
          ENDIF.

        WHEN 'ZMME85'.    "Change.
          IF sy-subrc = 0.
          ELSE.
            MESSAGE e999(zmmm) WITH 'Data'
                                    ztmm_6007_01-subpart
                                    ztmm_6007_01-werks
                                    'does not exist !'.
          ENDIF.
        WHEN 'ZMME86'.   "Display
          IF sy-subrc = 0.
          ELSE.
            MESSAGE e999(zmmm) WITH 'Data'
                                    ztmm_6007_02-endpart
                                    ztmm_6007_02-werks
                                    'does not exist !'.
          ENDIF.
      ENDCASE.

  ENDCASE.

ENDMODULE.                 " check_key  INPUT
*&---------------------------------------------------------------------*
*&      Module  save  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE save INPUT.
  CHECK save_ok_code = 'SAVE'.
  CASE sy-dynnr.
    WHEN 0200.
*CHECK EXISTENCE CHECK
      SELECT SINGLE subpart werks
        INTO CORRESPONDING FIELDS OF gs_ztmm_6007_01
        FROM ztmm_6007_01
        WHERE subpart = ztmm_6007_01-subpart AND
              werks   = ztmm_6007_01-werks.
      CALL FUNCTION 'Z_FMM_6001_01_TIME_STAMP'
           CHANGING
                ch_erdat = ztmm_6007_01-erdat
                ch_erzet = ztmm_6007_01-erzet
                ch_ernam = ztmm_6007_01-ernam
                ch_aedat = ztmm_6007_01-aedat
                ch_aezet = ztmm_6007_01-aezet
                ch_aenam = ztmm_6007_01-aenam.

      IF sy-subrc = 0.
        UPDATE ztmm_6007_01.
      ELSE.
        INSERT ztmm_6007_01.
      ENDIF.
    WHEN 9200.
      gt_zsmm_6007_02 = it_zsmm_6007_02.
      DELETE gt_zsmm_6007_02 WHERE subpart = space.

      IF gt_zsmm_6007_02 IS INITIAL.
        MESSAGE i999(zmmm) WITH
          'There is no data. Not Saved !'.
        EXIT.
      ENDIF.
      CHECK NOT gt_zsmm_6007_02 IS INITIAL.

      CLEAR ix_check.
      PERFORM check_dup_subpart TABLES gt_zsmm_6007_02
                                CHANGING ix_check.

      CHECK ix_check IS INITIAL.
      FIELD-SYMBOLS: <fs_zsmm_6007_02> LIKE zsmm_6007_02.

      LOOP AT gt_zsmm_6007_02 ASSIGNING <fs_zsmm_6007_02>.
        PERFORM get_unit_of_matnr USING    <fs_zsmm_6007_02>-subpart
                                  CHANGING <fs_zsmm_6007_02>-meins.
        SELECT SINGLE erdat erzet ernam
          INTO (<fs_zsmm_6007_02>-erdat, <fs_zsmm_6007_02>-erzet,
                <fs_zsmm_6007_02>-ernam)
          FROM ztmm_6007_02
          WHERE endpart = <fs_zsmm_6007_02>-endpart AND
                werks   = <fs_zsmm_6007_02>-werks   AND
                subpart = <fs_zsmm_6007_02>-subpart.

        CALL FUNCTION 'Z_FMM_6001_01_TIME_STAMP'
             CHANGING
                  ch_erdat = <fs_zsmm_6007_02>-erdat
                  ch_erzet = <fs_zsmm_6007_02>-erzet
                  ch_ernam = <fs_zsmm_6007_02>-ernam
                  ch_aedat = <fs_zsmm_6007_02>-aedat
                  ch_aezet = <fs_zsmm_6007_02>-aezet
                  ch_aenam = <fs_zsmm_6007_02>-aenam.
      ENDLOOP.
      MODIFY ztmm_6007_02 FROM TABLE gt_zsmm_6007_02.
  ENDCASE.
ENDMODULE.                 " save  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_it_zsmm_6007_02  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_it_zsmm_6007_02 INPUT.
  MOVE-CORRESPONDING zsmm_6007_02 TO wa_zsmm_6007_02.
  MODIFY it_zsmm_6007_02 FROM  wa_zsmm_6007_02
                         INDEX tc_9200-current_line.
ENDMODULE.                 " modify_it_zsmm_6007_02  INPUT
