*****************************************************
***	Program: /VWK/MAIREPP224
***	Description: control_departure mass update
***	Author: Guilherme Ferrarezi	Date: 04/02/2025
***	Trijay
***--------------------------------------------------
REPORT /vwk/mairepp224.

TYPES:
  ty_t_worksheet_name TYPE STANDARD TABLE OF string,

  ty_t_maitpp209      TYPE STANDARD TABLE OF /vwk/maitpp209, " Controle de embarque - faturas

  BEGIN OF ty_t001w,
    werks TYPE t001w-werks,                                  " Centro
  END OF ty_t001w,

  ty_t_t001w TYPE SORTED TABLE OF ty_t001w WITH UNIQUE KEY werks,

  BEGIN OF ty_marc,
    matnr TYPE marc-matnr,                                   " Nº do material
    werks TYPE marc-werks,                                   " Centro
  END OF ty_marc,

  ty_t_marc TYPE SORTED TABLE OF ty_marc WITH UNIQUE KEY matnr werks,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,                                   " Nº conta do fornecedor
    name1 TYPE lfa1-name1,                                   " Nome 1
  END OF ty_lfa1,

  ty_t_lfa1 TYPE SORTED TABLE OF ty_lfa1 WITH UNIQUE KEY lifnr,

  ty_t_log  TYPE STANDARD TABLE OF /vwk/maispp082,           " Log de controle de embarques

  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,                                   " Nº do material
    maktx TYPE makt-maktx,                                   " Texto breve de material
  END OF ty_makt,

  ty_t_makt TYPE SORTED TABLE OF ty_makt WITH UNIQUE KEY matnr.

CONSTANTS:
  BEGIN OF c_structure,
    boarding_control TYPE dd02l-tabname VALUE '/VWK/MAISPP082', " Nome da tabela
  END OF c_structure.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.

PARAMETERS: p_file   TYPE string OBLIGATORY DEFAULT 'C:\.xls', " File local para Upload ou Download
            p_export TYPE rlgrap-filename OBLIGATORY. " File local para Upload ou Download

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM zf_search_import_archive.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_export.

  PERFORM zf_search_export_directory.

START-OF-SELECTION.

  PERFORM zf_control_departure.

FORM zf_search_import_archive.

  DATA: lt_filetable    TYPE filetable,
        lv_window_title TYPE string,
        lv_rc           TYPE i. " Rc of type Integers

  lv_window_title = TEXT-t02.

  FIELD-SYMBOLS:<fs_file> LIKE LINE OF lt_filetable.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title      = lv_window_title
      default_extension = space
      default_filename  = '*.xls'
    CHANGING
      file_table        = lt_filetable
      rc                = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).
  IF sy-subrc <> 0.
    MESSAGE e000(oo) WITH TEXT-e01. " & & & &
  ENDIF. " IF sy-subrc <> 0

  READ TABLE lt_filetable ASSIGNING <fs_file> INDEX 1.
  IF sy-subrc IS INITIAL.
    p_file = <fs_file>-filename.
  ENDIF. " IF sy-subrc IS INITIAL

ENDFORM.
FORM zf_search_export_directory.

  DATA: lv_window_title TYPE string,
        lv_folder       TYPE string.

  lv_window_title = TEXT-t03.

  cl_gui_frontend_services=>directory_browse(
  EXPORTING
    window_title = lv_window_title
    CHANGING
      selected_folder        = lv_folder
    EXCEPTIONS
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).
  IF sy-subrc <> 0.
    MESSAGE e000(oo) WITH TEXT-e01. " & & & &
  ENDIF. " IF sy-subrc <> 0

  p_export = lv_folder.

ENDFORM.
FORM zf_control_departure.

  DATA: lt_data_tab        TYPE solix_tab,
        lt_data_tab_del    TYPE solix_tab,
        lv_xdocument       TYPE xstring,
        lv_filelength      TYPE i,                            " Filelength of type Integers
        lv_excel_ref       TYPE REF TO cl_fdt_xl_spreadsheet, " Fdt_xl_spreadsheet class
        lt_worksheet_names TYPE ty_t_worksheet_name,
        lt_maitpp209       TYPE ty_t_maitpp209,
        lt_log             TYPE ty_t_log,
        lv_filename        TYPE string,
        flg_error          TYPE flag.                         " Flag geral

  lv_filename = p_file.

  PERFORM zf_read_archive_to_table USING lv_filename
                                         'BIN'
                                CHANGING lv_filelength
                                         lv_xdocument
                                         lt_data_tab.

  lt_data_tab_del = lt_data_tab.

  PERFORM zf_scms_binary_to_xstring USING lv_filelength
                                 CHANGING lv_xdocument
                                          lt_data_tab.

  PERFORM zf_create_excel_ref USING lv_filename
                                    lv_xdocument
                           CHANGING lv_excel_ref
                                    flg_error.

  IF flg_error IS NOT INITIAL.
    MESSAGE s000(oo) WITH TEXT-e01 DISPLAY LIKE 'E'. " & & & &
    RETURN.
  ENDIF. " IF flg_error IS NOT INITIAL

  CALL METHOD lv_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names
    IMPORTING
      worksheet_names = lt_worksheet_names.

  IF lt_worksheet_names IS INITIAL.
    MESSAGE s000(oo) WITH TEXT-e11 DISPLAY LIKE 'E'. " & & & &
    RETURN.
  ENDIF. " IF lt_worksheet_names IS INITIAL

  PERFORM zf_build_maitpp209_by_excel USING lv_excel_ref
                                            lt_worksheet_names
                                   CHANGING lt_maitpp209.

  PERFORM zf_validate_maitpp209_values CHANGING lt_maitpp209
                                                lt_log.

  MODIFY /vwk/maitpp209 FROM TABLE lt_maitpp209.

  PERFORM zf_move_archive USING 'BIN'
                                lt_data_tab_del.

  IF lt_log IS INITIAL.
    MESSAGE s000(oo) WITH TEXT-s01. " & & & &
  ELSE. " ELSE -> IF lt_log IS INITIAL
    PERFORM zf_show_log_alv USING lt_log.
  ENDIF. " IF lt_log IS INITIAL

ENDFORM.
FORM zf_read_archive_to_table USING pv_filename TYPE string
                                    pv_filetype TYPE char10
                           CHANGING pv_filelength TYPE i " Filelength of type Integers
                                    pv_header     TYPE xstring
                                    pt_data_tab  TYPE solix_tab.

  CLEAR: pt_data_tab, pv_header, pv_filelength.
  IF pv_filename IS INITIAL.
    RETURN.
  ENDIF. " IF pv_filename IS INITIAL

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = pv_filename
      filetype                = pv_filetype
    IMPORTING
      filelength              = pv_filelength
      header                  = pv_header
    CHANGING
      data_tab                = pt_data_tab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_build_maitpp209_by_excel USING pv_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet " Fdt_xl_spreadsheet class
                                       pt_worksheet_names TYPE ty_t_worksheet_name
                              CHANGING pt_maitpp209 TYPE ty_t_maitpp209.

  DATA: wa_maitpp209      LIKE LINE OF pt_maitpp209,
        lv_tabix          TYPE sy-tabix,         " Campo do sistema ABAP: índice de linhas das tabelas internas
        lv_excel_ref_itab TYPE REF TO data,      "  class
        lv_dummy          TYPE c,                " Dummy of type Character
        lt_datum          TYPE STANDARD TABLE OF string,
        lv_datum          LIKE LINE OF lt_datum. " Campo do sistema ABAP: contador de loops

  FIELD-SYMBOLS: <fs_worksheet_tab> TYPE STANDARD TABLE.

  CLEAR pt_maitpp209.

  LOOP AT pt_worksheet_names ASSIGNING FIELD-SYMBOL(<fs_worksheet_names>).

    CLEAR: wa_maitpp209,
           lv_datum.

    lv_excel_ref_itab = pv_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( <fs_worksheet_names> ).
    ASSIGN lv_excel_ref_itab->* TO <fs_worksheet_tab>.
    CHECK <fs_worksheet_tab> IS ASSIGNED.

    DELETE <fs_worksheet_tab> WHERE ('A = space').

    LOOP AT <fs_worksheet_tab> ASSIGNING FIELD-SYMBOL(<fs_worksheet_line>).
      lv_tabix = sy-tabix.

      IF lv_tabix EQ 2.
        ASSIGN COMPONENT 2 OF STRUCTURE <fs_worksheet_line> TO FIELD-SYMBOL(<fs_field>).
        IF sy-subrc IS INITIAL.
          wa_maitpp209-werks = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL

      ELSEIF lv_tabix EQ 4.
        ASSIGN COMPONENT 2 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-matnr = <fs_field>.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = wa_maitpp209-matnr
            IMPORTING
              output       = wa_maitpp209-matnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
          IF sy-subrc IS NOT INITIAL.
            CONTINUE.
          ENDIF. " IF sy-subrc IS NOT INITIAL
        ENDIF. " IF sy-subrc IS INITIAL

      ELSEIF lv_tabix EQ 7.
        ASSIGN COMPONENT 2 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-used_model = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL

      ELSEIF lv_tabix EQ 10.
        ASSIGN COMPONENT 2 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          SPLIT <fs_field> AT space INTO wa_maitpp209-transit_time lv_dummy.
        ENDIF. " IF sy-subrc IS INITIAL

      ELSEIF lv_tabix EQ 11.
        ASSIGN COMPONENT 2 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-lifnr = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL

      ELSEIF lv_tabix GT 13.
        ASSIGN COMPONENT 1 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-belnr = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL

        ASSIGN COMPONENT 2 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          SPLIT <fs_field> AT '-' INTO TABLE lt_datum.
          LOOP AT lt_datum ASSIGNING FIELD-SYMBOL(<fs_datum>).
            IF strlen( <fs_datum> ) LT 2.
              CONCATENATE '0' <fs_datum> INTO <fs_datum>.
            ENDIF. " IF strlen( <fs_datum> ) LT 2
            CONCATENATE lv_datum <fs_datum> INTO lv_datum.
          ENDLOOP. " LOOP AT lt_datum ASSIGNING FIELD-SYMBOL(<fs_datum>)
          IF lv_datum NA sy-abcde.
            wa_maitpp209-fkdat = lv_datum.
            CLEAR lv_datum.
          ENDIF. " IF lv_datum NA sy-abcde
        ENDIF. " IF sy-subrc IS INITIAL

        ASSIGN COMPONENT 3 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-dispatch = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL

        ASSIGN COMPONENT 4 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-menge = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL

        ASSIGN COMPONENT 7 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          SPLIT <fs_field> AT '-' INTO TABLE lt_datum.
          LOOP AT lt_datum ASSIGNING <fs_datum>.
            IF strlen( <fs_datum> ) LT 2.
              CONCATENATE '0' <fs_datum> INTO <fs_datum>.
            ENDIF. " IF strlen( <fs_datum> ) LT 2
            CONCATENATE lv_datum <fs_datum> INTO lv_datum.
          ENDLOOP. " LOOP AT lt_datum ASSIGNING <fs_datum>
          IF lv_datum NA sy-abcde.
            wa_maitpp209-etd = lv_datum.
            CLEAR lv_datum.
          ENDIF. " IF lv_datum NA sy-abcde
        ENDIF. " IF sy-subrc IS INITIAL

        ASSIGN COMPONENT 8 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          SPLIT <fs_field> AT '-' INTO TABLE lt_datum.
          LOOP AT lt_datum ASSIGNING <fs_datum>.
            IF strlen( <fs_datum> ) LT 2.
              CONCATENATE '0' <fs_datum> INTO <fs_datum>.
            ENDIF. " IF strlen( <fs_datum> ) LT 2
            CONCATENATE lv_datum <fs_datum> INTO lv_datum.
          ENDLOOP. " LOOP AT lt_datum ASSIGNING <fs_datum>
          IF lv_datum NA sy-abcde.
            wa_maitpp209-eta = lv_datum.
            CLEAR lv_datum.
          ENDIF. " IF lv_datum NA sy-abcde
        ENDIF. " IF sy-subrc IS INITIAL

        ASSIGN COMPONENT 9 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          SPLIT <fs_field> AT '-' INTO TABLE lt_datum.
          LOOP AT lt_datum ASSIGNING <fs_datum>.
            IF strlen( <fs_datum> ) LT 2.
              CONCATENATE '0' <fs_datum> INTO <fs_datum>.
            ENDIF. " IF strlen( <fs_datum> ) LT 2
            CONCATENATE lv_datum <fs_datum> INTO lv_datum.
          ENDLOOP. " LOOP AT lt_datum ASSIGNING <fs_datum>
          IF lv_datum NA sy-abcde.
            wa_maitpp209-inbound_date = lv_datum.
            CLEAR lv_datum.
          ENDIF. " IF lv_datum NA sy-abcde
        ENDIF. " IF sy-subrc IS INITIAL


        ASSIGN COMPONENT 10 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-trasnsport_type = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL


        ASSIGN COMPONENT 11 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-trasnsport_comp = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL


        ASSIGN COMPONENT 12 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-container = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL


        ASSIGN COMPONENT 13 OF STRUCTURE <fs_worksheet_line> TO <fs_field>.
        IF sy-subrc IS INITIAL.
          wa_maitpp209-observation = <fs_field>.
        ENDIF. " IF sy-subrc IS INITIAL

        APPEND wa_maitpp209 TO pt_maitpp209.
        CLEAR: wa_maitpp209-belnr, wa_maitpp209-fkdat, wa_maitpp209-dispatch,
               wa_maitpp209-menge, wa_maitpp209-etd, wa_maitpp209-eta,
               wa_maitpp209-inbound_date, wa_maitpp209-trasnsport_type, wa_maitpp209-container,
               wa_maitpp209-observation, wa_maitpp209-trasnsport_comp.

      ENDIF. " IF lv_tabix EQ 2

    ENDLOOP. " LOOP AT <fs_worksheet_tab> ASSIGNING FIELD-SYMBOL(<fs_worksheet_line>)

  ENDLOOP. " LOOP AT pt_worksheet_names ASSIGNING FIELD-SYMBOL(<fs_worksheet_names>)


ENDFORM.
FORM zf_validate_maitpp209_values CHANGING pt_maitpp209 TYPE ty_t_maitpp209
                                           pt_log TYPE ty_t_log.

  DATA: lt_t001w  TYPE ty_t_t001w,
        lt_marc   TYPE ty_t_marc,
        lt_lfa1   TYPE ty_t_lfa1,
        lt_makt   TYPE ty_t_makt,
        wa_log    LIKE LINE OF pt_log,
        flg_error TYPE flag. " Flag geral

  CLEAR pt_log.

  PERFORM zf_read_t001w_by_maitpp209 USING pt_maitpp209
                                  CHANGING lt_t001w.

  PERFORM zf_read_marc_by_maitpp209 USING pt_maitpp209
                                 CHANGING lt_marc.

  PERFORM zf_read_lfa1_by_maitpp209 USING pt_maitpp209
                                 CHANGING lt_lfa1.

  PERFORM zf_read_makt_by_maitpp209 USING pt_maitpp209
                                 CHANGING lt_makt.

  LOOP AT pt_maitpp209 ASSIGNING FIELD-SYMBOL(<fs_maitpp209>).

    CLEAR: wa_log, flg_error.

    READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<fs_makt>) WITH KEY matnr = <fs_maitpp209>-matnr.

    READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>) WITH KEY lifnr = <fs_maitpp209>-lifnr.
    IF sy-subrc IS NOT INITIAL.
      MOVE-CORRESPONDING <fs_maitpp209> TO wa_log.
      IF <fs_makt> IS ASSIGNED.
        wa_log-maktx = <fs_makt>-maktx.
      ENDIF. " IF <fs_makt> IS ASSIGNED
      wa_log-message = TEXT-e04.
      APPEND wa_log TO pt_log.
      CLEAR wa_log.
      flg_error = abap_true.
    ENDIF. " IF sy-subrc IS NOT INITIAL

    READ TABLE lt_t001w TRANSPORTING NO FIELDS WITH KEY werks = <fs_maitpp209>-werks.
    IF sy-subrc IS NOT INITIAL.
      MOVE-CORRESPONDING <fs_maitpp209> TO wa_log.
      IF <fs_lfa1> IS ASSIGNED.
        wa_log-name1 = <fs_lfa1>-name1.
      ENDIF. " IF <fs_lfa1> IS ASSIGNED
      IF <fs_makt> IS ASSIGNED.
        wa_log-maktx = <fs_makt>-maktx.
      ENDIF. " IF <fs_makt> IS ASSIGNED
      wa_log-message = TEXT-e02.
      APPEND wa_log TO pt_log.
      CLEAR wa_log.
      flg_error = abap_true.
    ENDIF. " IF sy-subrc IS NOT INITIAL

    READ TABLE lt_marc TRANSPORTING NO FIELDS WITH KEY matnr = <fs_maitpp209>-matnr
                                                       werks = <fs_maitpp209>-werks.
    IF  sy-subrc IS NOT INITIAL.
      MOVE-CORRESPONDING <fs_maitpp209> TO wa_log.
      IF <fs_lfa1> IS ASSIGNED.
        wa_log-name1 = <fs_lfa1>-name1.
      ENDIF. " IF <fs_lfa1> IS ASSIGNED
      wa_log-message = TEXT-e03.
      APPEND wa_log TO pt_log.
      CLEAR wa_log.
      flg_error = abap_true.
    ENDIF. " IF sy-subrc IS NOT INITIAL

    IF <fs_maitpp209>-fkdat IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = <fs_maitpp209>-fkdat
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.
      IF sy-subrc IS NOT INITIAL.
        MOVE-CORRESPONDING <fs_maitpp209> TO wa_log.
        IF <fs_lfa1> IS ASSIGNED.
          wa_log-name1 = <fs_lfa1>-name1.
        ENDIF. " IF <fs_lfa1> IS ASSIGNED
        IF <fs_makt> IS ASSIGNED.
          wa_log-maktx = <fs_makt>-maktx.
        ENDIF. " IF <fs_makt> IS ASSIGNED
        wa_log-message = TEXT-e07.
        APPEND wa_log TO pt_log.
        CLEAR wa_log.
        flg_error = abap_true.
      ENDIF. " IF sy-subrc IS NOT INITIAL
    ENDIF. " IF <fs_maitpp209>-fkdat IS NOT INITIAL

    IF <fs_maitpp209>-etd IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = <fs_maitpp209>-etd
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.
      IF sy-subrc IS NOT INITIAL.
        MOVE-CORRESPONDING <fs_maitpp209> TO wa_log.
        IF <fs_lfa1> IS ASSIGNED.
          wa_log-name1 = <fs_lfa1>-name1.
        ENDIF. " IF <fs_lfa1> IS ASSIGNED
        IF <fs_makt> IS ASSIGNED.
          wa_log-maktx = <fs_makt>-maktx.
        ENDIF. " IF <fs_makt> IS ASSIGNED
        wa_log-message = TEXT-e08.
        APPEND wa_log TO pt_log.
        CLEAR wa_log.
        flg_error = abap_true.
      ENDIF. " IF sy-subrc IS NOT INITIAL
    ENDIF. " IF <fs_maitpp209>-etd IS NOT INITIAL

    IF <fs_maitpp209>-eta IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = <fs_maitpp209>-eta
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.
      IF sy-subrc IS NOT INITIAL.
        MOVE-CORRESPONDING <fs_maitpp209> TO wa_log.
        IF <fs_lfa1> IS ASSIGNED.
          wa_log-name1 = <fs_lfa1>-name1.
        ENDIF. " IF <fs_lfa1> IS ASSIGNED
        IF <fs_makt> IS ASSIGNED.
          wa_log-maktx = <fs_makt>-maktx.
        ENDIF. " IF <fs_makt> IS ASSIGNED
        wa_log-message = TEXT-e09.
        APPEND wa_log TO pt_log.
        CLEAR wa_log.
        flg_error = abap_true.
      ENDIF. " IF sy-subrc IS NOT INITIAL
    ENDIF. " IF <fs_maitpp209>-eta IS NOT INITIAL

    IF <fs_maitpp209>-inbound_date IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = <fs_maitpp209>-inbound_date
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.
      IF sy-subrc IS NOT INITIAL.
        MOVE-CORRESPONDING <fs_maitpp209> TO wa_log.
        IF <fs_lfa1> IS ASSIGNED.
          wa_log-name1 = <fs_lfa1>-name1.
        ENDIF. " IF <fs_lfa1> IS ASSIGNED
        IF <fs_makt> IS ASSIGNED.
          wa_log-maktx = <fs_makt>-maktx.
        ENDIF. " IF <fs_makt> IS ASSIGNED
        wa_log-message = TEXT-e10.
        APPEND wa_log TO pt_log.
        CLEAR wa_log.
        flg_error = abap_true.
      ENDIF. " IF sy-subrc IS NOT INITIAL

      IF flg_error IS NOT INITIAL.
        DELETE pt_maitpp209.
      ENDIF. " IF flg_error IS NOT INITIAL
    ENDIF. " IF <fs_maitpp209>-inbound_date IS NOT INITIAL

  ENDLOOP. " LOOP AT pt_maitpp209 ASSIGNING FIELD-SYMBOL(<fs_maitpp209>)

ENDFORM.
FORM zf_read_t001w_by_maitpp209 USING pt_maitpp209 TYPE ty_t_maitpp209
                             CHANGING pt_t001w TYPE ty_t_t001w.

  DATA: lt_maitpp209 TYPE ty_t_maitpp209.

  CLEAR pt_t001w.

  lt_maitpp209 = pt_maitpp209.
  SORT lt_maitpp209 BY werks.
  DELETE ADJACENT DUPLICATES FROM lt_maitpp209 COMPARING werks.

  IF lt_maitpp209 IS INITIAL.
    RETURN.
  ENDIF. " IF lt_maitpp209 IS INITIAL

  SELECT werks " Centro
    FROM t001w " Centros/filiais
    INTO TABLE pt_t001w
    FOR ALL ENTRIES IN lt_maitpp209
    WHERE werks EQ lt_maitpp209-werks.

ENDFORM.
FORM zf_read_marc_by_maitpp209 USING pt_maitpp209 TYPE ty_t_maitpp209
                            CHANGING pt_marc TYPE ty_t_marc.

  DATA: lt_maitpp209 TYPE ty_t_maitpp209.

  CLEAR pt_marc.

  lt_maitpp209 = pt_maitpp209.
  SORT lt_maitpp209 BY matnr werks.
  DELETE ADJACENT DUPLICATES FROM lt_maitpp209 COMPARING matnr werks.

  IF lt_maitpp209 IS INITIAL.
    RETURN.
  ENDIF. " IF lt_maitpp209 IS INITIAL

  SELECT matnr werks
    FROM marc " Dados de centro para material
    INTO TABLE pt_marc
    FOR ALL ENTRIES IN lt_maitpp209
    WHERE matnr EQ lt_maitpp209-matnr
      AND werks EQ lt_maitpp209-werks.

ENDFORM.
FORM zf_read_lfa1_by_maitpp209 USING pt_maitpp209 TYPE ty_t_maitpp209
                            CHANGING pt_lfa1 TYPE ty_t_lfa1.

  DATA: lt_maitpp209 TYPE ty_t_maitpp209.

  CLEAR pt_lfa1.

  lt_maitpp209 = pt_maitpp209.
  SORT lt_maitpp209 BY lifnr.
  DELETE ADJACENT DUPLICATES FROM lt_maitpp209 COMPARING lifnr.

  IF lt_maitpp209 IS INITIAL.
    RETURN.
  ENDIF. " IF lt_maitpp209 IS INITIAL

  SELECT lifnr name1
    FROM lfa1 " Mestre de fornecedores (parte geral)
    INTO TABLE pt_lfa1
    FOR ALL ENTRIES IN lt_maitpp209
    WHERE lifnr EQ lt_maitpp209-lifnr.

ENDFORM.
FORM zf_read_makt_by_maitpp209 USING pt_maitpp209 TYPE ty_t_maitpp209
                            CHANGING pt_makt TYPE ty_t_makt.

  DATA: lt_maitpp209 TYPE ty_t_maitpp209.

  CLEAR pt_makt.

  lt_maitpp209 = pt_maitpp209.
  SORT lt_maitpp209 BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_maitpp209 COMPARING matnr.

  IF lt_maitpp209 IS INITIAL.
    RETURN.
  ENDIF. " IF lt_maitpp209 IS INITIAL

  SELECT matnr maktx
    FROM makt " Textos breves de material
    INTO TABLE pt_makt
    FOR ALL ENTRIES IN lt_maitpp209
    WHERE matnr EQ lt_maitpp209-matnr
      AND spras EQ sy-langu.

ENDFORM.
FORM zf_show_log_alv USING pt_log TYPE ty_t_log.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.

  PERFORM zf_build_fieldcat CHANGING lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title       = TEXT-e05
      i_selection   = abap_false
      i_zebra       = abap_true
      i_tabname     = 'PT_LOG'
      it_fieldcat   = lt_fieldcat
    TABLES
      t_outtab      = pt_log
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_build_fieldcat CHANGING pt_fieldcat TYPE slis_t_fieldcat_alv.

  CLEAR pt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = c_structure-boarding_control
    CHANGING
      ct_fieldcat            = pt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_move_archive USING pv_filetype TYPE char10
                           pt_data_tab TYPE solix_tab.

  DATA: lv_filename   TYPE string,
        lt_file       TYPE STANDARD TABLE OF string,
        lv_lines      TYPE i, " Lines of type Integers
        lv_exportname TYPE string,
        lv_rc         TYPE i. " Rc of type Integers

  lv_filename = p_file.

  SPLIT lv_filename AT '\' INTO TABLE lt_file.

  DESCRIBE TABLE lt_file LINES lv_lines.

  READ TABLE lt_file ASSIGNING FIELD-SYMBOL(<fs_file>) INDEX lv_lines.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE s000(oo) WITH TEXT-e06 DISPLAY LIKE 'E'. " & & & &
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  CONCATENATE p_export <fs_file> INTO lv_exportname SEPARATED BY '\'.

  IF lv_exportname EQ lv_filename.
    RETURN.
  ENDIF. " IF lv_exportname EQ lv_filename

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_exportname
      filetype                = pv_filetype
    TABLES
      data_tab                = pt_data_tab
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE s000(oo) WITH TEXT-e06 DISPLAY LIKE 'E'. " & & & &
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  cl_gui_frontend_services=>file_delete( EXPORTING filename = lv_filename
                                          CHANGING rc = lv_rc
                                        EXCEPTIONS file_delete_failed = 1
                                            OTHERS = 2 ).

  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_scms_binary_to_xstring USING pv_input_length TYPE i " Scms_binary_to_xstring of type Integers
                            CHANGING pv_buffer      TYPE xstring
                                     pt_binary_tab TYPE solix_tab.

  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = pv_input_length
    IMPORTING
      buffer       = pv_buffer
    TABLES
      binary_tab   = pt_binary_tab
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_create_excel_ref USING pv_filename TYPE string
                               pv_xdocument TYPE xstring
                      CHANGING pv_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet " Fdt_xl_spreadsheet class
                               pflg_error TYPE flag.                          " Flag geral

  CLEAR pflg_error.

  IF pv_excel_ref IS BOUND.
    RETURN.
  ENDIF. " IF pv_excel_ref IS BOUND

  TRY.
      CREATE OBJECT pv_excel_ref
        EXPORTING
          document_name = pv_filename
          xdocument     = pv_xdocument.

    CATCH cx_fdt_excel_core.
      pflg_error = abap_true.
      RETURN.
  ENDTRY.

ENDFORM.
