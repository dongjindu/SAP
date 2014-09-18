report  zfebka10.                                .

type-pools: slis.

data: h_bseg type bseg,
      l_bseg type standard table of bseg,
      l_cleared_bseg type standard table of bseg,
      h_febep type febep,
      l_febep type standard table of febep,
      h_febko type febko,
      l_febko type standard table of febko,
      h_fields type zfebka10_fieldcatalogue,
      l_fields type standard table of zfebka10_fieldcatalogue,
      l_fields_update_1 type standard table of zfebka10_fieldcatalogue,
      l_fields_update_2 type standard table of zfebka10_fieldcatalogue,
      l_layout type slis_layout_alv.

selection-screen begin of block 1 with frame title text-010.
parameters: l_hkont like h_bseg-hkont.
selection-screen end of block 1.

selection-screen begin of block 2 with frame title text-020.
select-options s_kukey for h_febep-kukey.
selection-screen end of block 2.

parameters: l_update type z_aabgl_eb.

at selection-screen.
  if not l_hkont is initial and not s_kukey is initial.
    message i150(new_feba).
  endif.


start-of-selection.

  if not l_hkont is initial.

    select * from bseg into table l_bseg
      where augbl = space
        and hkont = l_hkont.

    select * from febep into table l_febep
      where vb2ok = ' '.

    sort l_febep by kukey esnum.

*1st color: choose all febeps whose BELNR corresponds to open items
*leftovers must either been cleared or have other accounts
    loop at l_febep into h_febep.
      at new kukey.
        select * from febko appending table l_febko
          where kukey = h_febep-kukey.
      endat.
      read table l_bseg with key belnr = h_febep-belnr into h_bseg.
      if sy-subrc = 0.
        clear h_fields.
        delete l_bseg index sy-tabix.
        read table l_febko with key kukey = h_febep-kukey
          into h_febko.
        move-corresponding h_febko to h_fields.
        move-corresponding h_bseg to h_fields.
        move-corresponding h_febep to h_fields.
        move h_febko-hbkid to h_fields-hbkid.
        delete l_febep.
        h_fields-color = 'C10'.
        append h_fields to l_fields.
      else.
*delete those febeps whose BELNR just means other accounts
        select single * from bseg into h_bseg
          where belnr = h_febep-belnr
            and hkont = l_hkont.
        if sy-subrc <> 0.
          delete l_febep.
        endif.
      endif.
    endloop.

    sort l_fields by hbkid valut.

    loop at l_febep into h_febep.
      clear h_fields.
      read table l_febko with key kukey = h_febep-kukey
        into h_febko.
      move-corresponding h_febko to h_fields.
      move-corresponding h_febep to h_fields.
      clear h_fields-hkont.         "no open item on interim account
      h_fields-color = 'C20'.
*find out the document which was cleared against posting area 1 document
      clear h_fields-belnr.
      select single * from bseg into h_bseg
        where belnr = h_febep-belnr
          and augbl ne space.
      if sy-subrc = 0.
        select * from bseg into table l_cleared_bseg
          where hkont = l_hkont
            and augbl = h_bseg-augbl.
        if sy-dbcnt = 2.
*2 lines: the document number is unique
          read table l_cleared_bseg
            with key belnr = h_febep-belnr
            into h_bseg.
          if sy-subrc = 0.
            if sy-tabix = 1.
              read table l_cleared_bseg index 2
                into h_bseg.
            else.
              read table l_cleared_bseg index 1
                into h_bseg.
            endif.
            h_fields-belnr = h_bseg-belnr.
            if l_update = 'X'.
              move 'X' to h_fields-updte.
              append h_fields to l_fields_update_2.
            endif.       "automatic update?
          endif.         "double-check: original item was cleared
        endif.           "document number unique
      endif.             "there is a clearing document
      append h_fields to l_fields.
    endloop.

    loop at l_bseg into h_bseg.
      clear h_fields.
      move-corresponding h_bseg to h_fields.
      h_fields-color = 'C30'.
      append h_fields to l_fields.
    endloop.

    if l_update = 'X'.

      sort l_fields_update_2 by kukey esnum.
      loop at l_fields_update_2 into h_fields.
        update febep set vb2ok = 'X' nbbln = h_fields-belnr
          where kukey = h_fields-kukey
            and esnum = h_fields-esnum.
        at end of kukey.
          call function 'UPDATE_FEBKO_STATUS'
               EXPORTING
                    i_kukey            = h_fields-kukey
               EXCEPTIONS
                    kukey_not_in_febko = 1
                    others             = 2.
          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          endif.
        endat.
      endloop.

    endif.

  else.

    select * from febep into table l_febep
      where ( ( vb2ok = ' ' and not akbln = space )
        or    ( vb1ok = ' ' and not ak1bl = space ) )
        and kukey in s_kukey.

    sort l_febep by kukey esnum.

    loop at l_febep into h_febep.
      at new kukey.
        select * from febko appending table l_febko
          where kukey = h_febep-kukey.
        read table l_febko with key kukey = h_febep-kukey
          into h_febko.
      endat.
      if not h_febep-ak1bl = space.
        select single * from bseg into h_bseg
          where bukrs = h_febko-bukrs
            and belnr = h_febep-ak1bl
            and gjahr = h_febep-gjahr
            and not augbl = space.
        if sy-subrc = 0.
          clear h_fields.
          move-corresponding h_febko to h_fields.
          move-corresponding h_febep to h_fields.
          move-corresponding h_bseg to h_fields.
          move h_febko-hbkid to h_fields-hbkid.
          move h_bseg-augbl to h_fields-belnr.
          h_fields-color = 'C10'.
          if l_update = 'X'.
            move 'X' to h_fields-updte.
            append h_fields to l_fields_update_1.
          endif.
          append h_fields to l_fields.
          continue.
        endif.
      endif.
      if not h_febep-akbln = space.
        select single * from bseg into h_bseg
          where bukrs = h_febko-bukrs
            and belnr = h_febep-akbln
            and gjahr = h_febep-gjahr
            and not augbl = space.
        if sy-subrc = 0.
          clear h_fields.
          move-corresponding h_febko to h_fields.
          move-corresponding h_febep to h_fields.
          move-corresponding h_bseg to h_fields.
          move h_febko-hbkid to h_fields-hbkid.
          move h_bseg-augbl to h_fields-belnr.
          h_fields-color = 'C20'.
          if l_update = 'X'.
            move 'X' to h_fields-updte.
            append h_fields to l_fields_update_2.
          endif.
          append h_fields to l_fields.
          continue.
        endif.
      endif.
    endloop.

    if l_update = 'X'.

*now, update the febep and febko table.
*1st: posting area 1
      sort l_fields_update_1 by kukey esnum.
      loop at l_fields_update_1 into h_fields.
        update febep set vb1ok = 'X' belnr = h_fields-belnr
          where kukey = h_fields-kukey
            and esnum = h_fields-esnum.
        at end of kukey.
          call function 'UPDATE_FEBKO_STATUS'
               EXPORTING
                    i_kukey            = h_fields-kukey
               EXCEPTIONS
                    kukey_not_in_febko = 1
                    others             = 2.
          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          endif.
        endat.
      endloop.

*2nd: posting area 2
      sort l_fields_update_2 by kukey esnum.
      loop at l_fields_update_2 into h_fields.
        update febep set vb2ok = 'X' nbbln = h_fields-belnr
          where kukey = h_fields-kukey
            and esnum = h_fields-esnum.
        at end of kukey.
          call function 'UPDATE_FEBKO_STATUS'
               EXPORTING
                    i_kukey            = h_fields-kukey
               EXCEPTIONS
                    kukey_not_in_febko = 1
                    others             = 2.
          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          endif.
        endat.
      endloop.

    endif.

  endif.

  l_layout-info_fieldname = 'COLOR'.
  l_layout-f2code = 'PICK'.
  call function 'REUSE_ALV_LIST_DISPLAY'
   exporting
*   I_INTERFACE_CHECK              = ' '
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                = ' '
    i_callback_program             = 'ZFEBKA10'
*   I_CALLBACK_PF_STATUS_SET       = ' '
    i_callback_user_command        = 'USER_COMMAND'
    i_structure_name               = 'ZFEBKA10_FIELDCATALOGUE'
    is_layout                      = l_layout
*   it_fieldcat                    =
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
*   IT_SORT                        =
*   IT_FILTER                      =
*   IS_SEL_HIDE                    =
*   I_DEFAULT                      = 'X'
*   I_SAVE                         = ' '
*   IS_VARIANT                     =
*   IT_EVENTS                      =
*   IT_EVENT_EXIT                  =
*   IS_PRINT                       =
*   IS_REPREP_ID                   =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    tables
      t_outtab                       = l_fields
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
            .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


*---------------------------------------------------------------------*
*  FORM user_command
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  R_UCOMM
*  -->  RS_SELFIELD
*---------------------------------------------------------------------*
form user_command
  using r_ucomm like sy-ucomm
        rs_selfield type slis_selfield.
  tables: febep.
  case r_ucomm.
    when 'PICK'.
      read table l_fields index rs_selfield-tabindex
        into h_fields.
      if not h_fields-kukey is initial and
         not h_fields-esnum is initial.
      else.
        if not h_fields-belnr is initial.
          set parameter id 'BLN' field h_fields-belnr.
          set parameter id 'BUK' field h_fields-bukrs.
          set parameter id 'GJR' field h_fields-gjahr.
          call transaction 'FB03' and skip first screen.
        endif.
      endif.
  endcase.
endform.                    "user_command
