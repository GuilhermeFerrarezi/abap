*****************************************************
***	Program: /VWK/MAIREPP225
***	Description: unified realise inventory analysis
***	Author: Guilherme Ferrarezi	Date: 06/02/2025
***	Trijay
***--------------------------------------------------
REPORT /vwk/mairepp225.

TABLES sscrfields. " Campos em imagens de seleção

TYPES:
  BEGIN OF ty_t001w,
    werks TYPE t001w-werks,                                         " Centro
    name1 TYPE t001w-name1,                                         " Nome
  END OF ty_t001w,

  ty_t_t001w           TYPE SORTED TABLE OF ty_t001w WITH UNIQUE KEY werks,

  ty_t_mrp_ind_lines   TYPE STANDARD TABLE OF bapi_mrp_ind_lines,   " MRP: linhas individuais elementos MRP

  ty_t_mrp_total_lines TYPE STANDARD TABLE OF bapi_mrp_total_lines, " MRP: linhas de totais para elementos MRP

  BEGIN OF ty_mrp_total_lines.
    INCLUDE TYPE bapi_mrp_total_lines.
TYPES: werks TYPE /vwk/maitpp209-werks,                                      " Centro
       week  TYPE numc2,                                                     " Período ou segmento
       year  TYPE numc4,                                                     " Centro
       END OF ty_mrp_total_lines,

       ty_t_mrp_total_lines_werks TYPE STANDARD TABLE OF ty_mrp_total_lines, " MRP: linhas de totais para elementos MRP

       BEGIN OF ty_mrp_ind_lines_werks.
    INCLUDE TYPE bapi_mrp_ind_lines.
TYPES: werks TYPE /vwk/maitpp209-werks,                                       " Centro
       week  TYPE numc2,                                                      " Período ou segmento
       year  TYPE numc4,                                                      " Parâmetro de contagem
       END OF ty_mrp_ind_lines_werks,

       ty_t_mrp_ind_lines_werks TYPE STANDARD TABLE OF ty_mrp_ind_lines_werks,

       ty_t_stock_detail        TYPE STANDARD TABLE OF bapi_mrp_stock_detail, " MRP: campos de estatística para estoques

       BEGIN OF ty_maitpp209.
    INCLUDE TYPE /vwk/maitpp209.
TYPES: week TYPE numc2,                                                                  " Nº de dois dígitos
       year TYPE numc4,                                                                  " Parâmetro de contagem
       END OF ty_maitpp209,

       ty_t_maitpp209 TYPE STANDARD TABLE OF ty_maitpp209,                               " Controle de embarque - faturas

       BEGIN OF ty_maitpp210,
         werks TYPE /vwk/maitpp210-werks,                                                " Centro
         lgort TYPE /vwk/maitpp210-lgort,                                                " Depósito
       END OF ty_maitpp210,

       ty_t_maitpp210 TYPE STANDARD TABLE OF ty_maitpp210,

       BEGIN OF ty_mard,
         matnr TYPE mard-matnr,                                                          " Nº do material
         werks TYPE mard-werks,                                                          " Centro
         labst TYPE mard-labst,                                                          " Estoque avaliado de utilização livre
       END OF ty_mard,

       ty_t_mard TYPE STANDARD TABLE OF ty_mard,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,                                                          " Nº do material
         maktx TYPE makt-maktx,                                                          " Texto breve de material
       END OF ty_makt,

       ty_t_makt TYPE SORTED TABLE OF ty_makt WITH UNIQUE KEY matnr,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,                                                          " Nº conta do fornecedor
         name1 TYPE lfa1-name1,                                                          " Nome 1
       END OF ty_lfa1,

       ty_t_lfa1 TYPE SORTED TABLE OF ty_lfa1 WITH UNIQUE KEY lifnr,

       BEGIN OF ty_marc,
         matnr TYPE marc-matnr,                                                          " Nº do material
         werks TYPE marc-werks,                                                          " Centro
         dispo TYPE marc-dispo,                                                          " Planejador MRP
       END OF ty_marc,

       ty_t_marc TYPE SORTED TABLE OF ty_marc WITH UNIQUE KEY matnr werks,

       BEGIN OF ty_t024d,
         werks TYPE t024d-werks,                                                         " Centro
         dispo TYPE t024d-dispo,                                                         " Planejador MRP
         dsnam TYPE t024d-dsnam,                                                         " Nome do planejador
       END OF ty_t024d,

       ty_t_t024d TYPE SORTED TABLE OF ty_t024d WITH UNIQUE KEY werks dispo,

       BEGIN OF ty_werks,
         werks TYPE werks_d,                                                             " Centro
         field TYPE char6,                                                               " Field of type CHAR6
       END OF ty_werks,

       ty_t_werks TYPE STANDARD TABLE OF ty_werks,

       BEGIN OF ty_week,
         per_segmt TYPE /vwk/maispp085-per_segmt,
         week      TYPE numc2,                                                           " Nº de dois dígitos
         year      TYPE numc4,                                                           " Parâmetro de contagem
       END OF ty_week,

       ty_t_week        TYPE STANDARD TABLE OF ty_week,

       ty_t_notfulltxti TYPE STANDARD TABLE OF bapi2080_notfulltxti,                     " Txt.descritivo nota

       ty_t_long_text   TYPE STANDARD TABLE OF tdline,                                   " Linha de texto

       ty_t_tline       TYPE STANDARD TABLE OF tline,                                    " SAPscript: linhas de textos

       ty_t_maitpp211   TYPE SORTED TABLE OF /vwk/maitpp211 WITH UNIQUE KEY matnr werks, " Controle de cobertura mínima para release unificado

       ty_t_maitpp213   TYPE STANDARD TABLE OF /vwk/maitpp213,                           " Controle de cobertura mínima para release unificado

       ty_t_maitpp214   TYPE STANDARD TABLE OF /vwk/maitpp214,                           " Controle de cobertura mínima para release unificado

       ty_t_maitpp215   TYPE STANDARD TABLE OF /vwk/maitpp215.                           " Controle de cobertura mínima para release unificado

CLASS lcl_event_receiver DEFINITION FINAL. " Event_receiver class

  PUBLIC SECTION.
    CLASS-METHODS: handle_data_changed
                  FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed,

      handle_hotspot_click
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id es_row_no.

ENDCLASS.

DATA:
  BEGIN OF wa_selscreen,
    werks      TYPE t001w-werks,                                              " Centro
    avail_date TYPE bapi_mrp_total_lines-avail_date,                          " Data da entrada/necessidade
    eta        TYPE /vwk/maitpp209-eta,                                       " Data estimada de chegada
    belnr      TYPE /vwk/maitpp209-belnr,                                     " Nº documento de um documento contábil
  END OF wa_selscreen,

  gv_container                TYPE REF TO cl_gui_custom_container ##NEEDED,   " Gui_custom_container class
  gv_week_info_container      TYPE REF TO cl_gui_custom_container ##NEEDED,   " Gui_custom_container class
  gv_gen_notes_container      TYPE REF TO cl_gui_custom_container ##NEEDED,   " Gui_custom_container class
  gv_unif_rep_split           TYPE REF TO cl_gui_splitter_container ##NEEDED, " Gui_splitter_container class
  gv_header_split             TYPE REF TO cl_gui_splitter_container ##NEEDED, " Gui_splitter_container class
  gv_general_notes_split      TYPE REF TO cl_gui_splitter_container ##NEEDED, " Gui_splitter_container class
  gv_header_container         TYPE REF TO cl_gui_container ##NEEDED,          " Gui_container class
  gv_unif_rep_container       TYPE REF TO cl_gui_container ##NEEDED,          " Gui_container class
  gv_stock_container          TYPE REF TO cl_gui_container ##NEEDED,          " Gui_container class
  gv_material_info_container  TYPE REF TO cl_gui_container ##NEEDED,          " Gui_container class
  gv_general_notes_container  TYPE REF TO cl_gui_container ##NEEDED,          " Gui_container class
  gv_gen_notes_edit_container TYPE REF TO cl_gui_container ##NEEDED,          " Gui_container class
  gv_gen_notes_hist_container TYPE REF TO cl_gui_container ##NEEDED,          " Gui_container class
  gv_stock_grid               TYPE REF TO cl_gui_alv_grid ##NEEDED,           " Gui_alv_grid class
  gv_material_info_grid       TYPE REF TO cl_gui_alv_grid ##NEEDED,           " Gui_alv_grid class
  gv_unif_rep_grid            TYPE REF TO cl_gui_alv_grid ##NEEDED,           " Gui_alv_grid class
  gv_week_info_grid           TYPE REF TO cl_gui_alv_grid ##NEEDED,           " Gui_alv_grid class
  gv_general_notes_txt        TYPE REF TO cl_gui_textedit ##NEEDED,           " Gui_textedit class
  gv_general_notes_txt_edit   TYPE REF TO cl_gui_textedit ##NEEDED,           " Gui_textedit class
  gv_general_notes_txt_hist   TYPE REF TO cl_gui_textedit ##NEEDED,           " Gui_textedit class
  gt_werks                    TYPE ty_t_werks ##NEEDED,
  gt_stock                    TYPE /vwk/maittpp022 ##NEEDED,
  gt_material_info            TYPE /vwk/maittpp023 ##NEEDED,
  gt_unif_rep                 TYPE /vwk/maittpp024 ##NEEDED,
  gt_mrp_ind_lines            TYPE ty_t_mrp_ind_lines_werks ##NEEDED,
  gt_week_info                TYPE /vwk/maittpp025 ##NEEDED,
  gt_maitpp209                TYPE ty_t_maitpp209 ##NEEDED,
  flg_tran                    TYPE flag ##NEEDED.                             " Flag geral

CONSTANTS:
  BEGIN OF c_structure,
    stock         TYPE dd02l-tabname VALUE '/VWK/MAISPP083', " Nome da tabela
    material_info TYPE dd02l-tabname VALUE '/VWK/MAISPP084', " Nome da tabela
    unif_rep      TYPE dd02l-tabname VALUE '/VWK/MAISPP085', " Nome da tabela
    week_info     TYPE dd02l-tabname VALUE '/VWK/MAISPP086', " Nome da tabela
  END OF c_structure,

  BEGIN OF c_general_notes_text,
    object TYPE thead-tdobject VALUE 'ZTXTUNIREP',           " Textos: objeto de aplicação
    id     TYPE thead-tdid VALUE 'ZREP',                     " ID de texto
    name   TYPE thead-tdname VALUE 'ZUNIFREP',               " Nome
  END OF c_general_notes_text.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.

SELECT-OPTIONS: s_werks FOR wa_selscreen-werks.
PARAMETERS: p_matnr TYPE mara-matnr. " Nº do material

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.

PARAMETERS: p_plscn TYPE plscn. " Cenário planejamento do plan.longo prazo
SELECT-OPTIONS: s_dperio FOR wa_selscreen-avail_date.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.

SELECT-OPTIONS: s_belnr FOR wa_selscreen-belnr,
                s_fperio FOR wa_selscreen-eta.

SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-t04.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_dpt TYPE flag RADIOBUTTON GROUP g1 DEFAULT 'X'. " Flag geral
SELECTION-SCREEN COMMENT (35) TEXT-t05 FOR FIELD p_dpt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_cntr TYPE flag RADIOBUTTON GROUP g1. " Flag geral
SELECTION-SCREEN COMMENT (34) TEXT-t06 FOR FIELD p_cntr.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN END OF BLOCK b4.

INITIALIZATION.
  LOOP AT SCREEN.
    IF screen-name EQ 'S_WERKS-LOW'.
      screen-required = '2'.
    ELSEIF screen-name EQ 'P_MATNR'.
      screen-required = '2'.
    ENDIF. " IF screen-name EQ 'S_WERKS-LOW'
    MODIFY SCREEN.
  ENDLOOP. " LOOP AT SCREEN
  sscrfields-functxt_01 = TEXT-t22.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      CALL TRANSACTION '/VWK/MAIREPP222'.
  ENDCASE.

START-OF-SELECTION.

  PERFORM zf_inventory_analysis.

FORM zf_inventory_analysis.

  DATA: lt_t001w           TYPE ty_t_t001w,
        lt_mrp_total_lines TYPE ty_t_mrp_total_lines_werks,
        lt_stock_detail    TYPE ty_t_stock_detail,
        lt_maitpp210       TYPE ty_t_maitpp210,
        lt_mard            TYPE ty_t_mard,
        lt_makt            TYPE ty_t_makt,
        lt_lfa1            TYPE ty_t_lfa1,
        lt_marc            TYPE ty_t_marc,
        lt_t024d           TYPE ty_t_t024d,
        wa_werks           LIKE LINE OF gt_werks,
        cnt_werks          TYPE n VALUE 1, " Werks of type Numeric Text Fields
        lt_maitpp211       TYPE ty_t_maitpp211 ##NEEDED.

  IF s_werks IS INITIAL
  OR p_matnr IS INITIAL.
    MESSAGE s000(oo) WITH TEXT-e04 DISPLAY LIKE 'E'. " & & & &
    RETURN.
  ENDIF. " IF s_werks IS INITIAL

  PERFORM zf_read_t001w_by_selscreen CHANGING lt_t001w.

  IF lines( lt_t001w ) GT 4.
    MESSAGE s000(oo) WITH TEXT-e02 DISPLAY LIKE 'E'. " & & & &
    RETURN.
  ENDIF. " IF lines( lt_t001w ) GT 4

  LOOP AT lt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>).
    CLEAR wa_werks.

    wa_werks-werks = <fs_t001w>-werks.
    CONCATENATE 'WERKS' cnt_werks INTO wa_werks-field.
    cnt_werks = cnt_werks + 1.
    APPEND wa_werks TO gt_werks.

  ENDLOOP. " LOOP AT lt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>)

  SORT gt_werks BY werks.

  PERFORM zf_call_material_stock_req USING lt_t001w
                                  CHANGING lt_mrp_total_lines
                                           lt_stock_detail.

  PERFORM zf_read_maitpp209_by_t001w USING lt_t001w
                                  CHANGING gt_maitpp209.

  IF gt_maitpp209 IS INITIAL.
    MESSAGE s000(oo) WITH TEXT-e01 DISPLAY LIKE 'W'. " & & & &
  ENDIF. " IF gt_maitpp209 IS INITIAL

  PERFORM zf_read_maitpp210_by_selscreen CHANGING lt_maitpp210.

  IF lt_maitpp210 IS INITIAL.
    MESSAGE s000(oo) WITH TEXT-e03 DISPLAY LIKE 'E'. " & & & &
    RETURN.
  ENDIF. " IF lt_maitpp210 IS INITIAL

  PERFORM zf_read_mard_by_maitpp209 USING gt_maitpp209
                                 CHANGING lt_mard.

  SORT lt_mard BY matnr werks.

  PERFORM zf_collect_mard_labst CHANGING lt_mard.

  PERFORM zf_read_maitpp211_by_t001w USING lt_t001w
                                  CHANGING lt_maitpp211.

  PERFORM zf_create_stock_table USING lt_t001w
                                      lt_mard
                                      lt_maitpp211.

  PERFORM zf_read_makt_by_maitpp209 USING gt_maitpp209
                                 CHANGING lt_makt.

  PERFORM zf_read_lfa1_by_maitpp209 USING gt_maitpp209
                                 CHANGING lt_lfa1.

  PERFORM zf_read_marc_by_maitpp209 USING gt_maitpp209
                                 CHANGING lt_marc.

  PERFORM zf_read_t024d_by_marc USING lt_marc
                             CHANGING lt_t024d.

  PERFORM zf_create_material_info_tale USING gt_maitpp209
                                             lt_makt
                                             lt_lfa1
                                             lt_marc
                                             lt_t024d
                                    CHANGING gt_material_info.

  DELETE lt_mrp_total_lines WHERE mrp_element_ind(1) IS NOT INITIAL.
  SORT lt_mrp_total_lines BY year week werks.
  DELETE gt_mrp_ind_lines WHERE mrp_element_ind NE 'PP'.

  PERFORM zf_fill_total_lines_by_week CHANGING lt_mrp_total_lines.

  SORT lt_mrp_total_lines BY per_segmt werks.
  DELETE ADJACENT DUPLICATES FROM lt_mrp_total_lines COMPARING per_segmt werks.
  SORT lt_mrp_total_lines BY year week werks.

  SORT gt_maitpp209 BY year
                       week
                       werks
                       matnr.

  PERFORM zf_create_unified_report_table USING lt_mrp_total_lines
                                               gt_maitpp209
                                               gt_werks
                                      CHANGING gt_unif_rep.

  CALL SCREEN 9000.

ENDFORM.
FORM zf_read_t001w_by_selscreen CHANGING pt_t001w TYPE ty_t_t001w.

  CLEAR pt_t001w.

  IF s_werks[] IS INITIAL.
    RETURN.
  ENDIF. " IF s_werks[] IS INITIAL

  SELECT werks name1 " Centro
    FROM t001w       " Centros/filiais
    INTO TABLE pt_t001w
    WHERE werks IN s_werks.

ENDFORM.
FORM zf_call_material_stock_req USING pt_t001w TYPE ty_t_t001w
                             CHANGING pt_mrp_total_lines TYPE ty_t_mrp_total_lines_werks
                                      pt_stock_detail TYPE ty_t_stock_detail.

  DATA: lt_mrp_ind_lines   TYPE ty_t_mrp_ind_lines,
        lt_mrp_total_lines TYPE ty_t_mrp_total_lines,
        wa_stock_detail    TYPE bapi_mrp_stock_detail, " MRP: campos de estatística para estoques
        wa_total_lines     LIKE LINE OF pt_mrp_total_lines,
        wa_ind_lines       LIKE LINE OF gt_mrp_ind_lines,
        lv_week            TYPE scal-week.             " Calendário de fábrica: semana

  CLEAR: pt_mrp_total_lines,
         gt_mrp_ind_lines.

  LOOP AT pt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>).

    CLEAR: lt_mrp_ind_lines,
           lt_mrp_total_lines,
           wa_stock_detail.

    CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
      EXPORTING
        material         = p_matnr
        plant            = <fs_t001w>-werks
        selection_rule   = 'SAP00002'
        period_indicator = 'W'
        get_total_lines  = abap_true
      IMPORTING
        mrp_stock_detail = wa_stock_detail
      TABLES
        mrp_ind_lines    = lt_mrp_ind_lines
        mrp_total_lines  = lt_mrp_total_lines.

    IF s_dperio[] IS NOT INITIAL.
      DELETE: lt_mrp_ind_lines WHERE avail_date NOT IN s_dperio,
              lt_mrp_total_lines WHERE avail_date NOT IN s_dperio.
    ENDIF. " IF s_dperio[] IS NOT INITIAL

    LOOP AT lt_mrp_total_lines ASSIGNING FIELD-SYMBOL(<fs_total_lines>).

      CLEAR wa_total_lines.

      MOVE-CORRESPONDING <fs_total_lines> TO wa_total_lines.
      wa_total_lines-werks = <fs_t001w>-werks.
      wa_total_lines-week = <fs_total_lines>-per_segmt+2(2).
      wa_total_lines-year = <fs_total_lines>-per_segmt+5(4).

      APPEND wa_total_lines TO pt_mrp_total_lines.

    ENDLOOP. " LOOP AT lt_mrp_total_lines ASSIGNING FIELD-SYMBOL(<fs_total_lines>)

    LOOP AT lt_mrp_ind_lines ASSIGNING FIELD-SYMBOL(<fs_ind_lines>).

      CLEAR: wa_ind_lines.

      MOVE-CORRESPONDING <fs_ind_lines> TO wa_ind_lines.

      wa_ind_lines-werks = <fs_t001w>-werks.
      CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
        EXPORTING
          date = <fs_ind_lines>-avail_date
        IMPORTING
          week = lv_week.

      wa_ind_lines-year = lv_week(4).
      wa_ind_lines-week = lv_week+4(2).

      IF wa_ind_lines-rec_reqd_qty LT 0.
        wa_ind_lines-rec_reqd_qty = wa_ind_lines-rec_reqd_qty * -1.
      ENDIF. " IF wa_ind_lines-rec_reqd_qty LT 0

      APPEND wa_ind_lines TO gt_mrp_ind_lines.

    ENDLOOP. " LOOP AT lt_mrp_ind_lines ASSIGNING FIELD-SYMBOL(<fs_ind_lines>)

    IF p_cntr IS NOT INITIAL.
      APPEND wa_stock_detail TO pt_stock_detail.
    ENDIF. " IF p_cntr IS NOT INITIAL

  ENDLOOP. " LOOP AT pt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>)

ENDFORM.
FORM zf_read_maitpp211_by_t001w USING pt_t001w TYPE ty_t_t001w
                             CHANGING pt_maitpp211 TYPE ty_t_maitpp211.

  CLEAR pt_maitpp211.

  IF pt_t001w IS INITIAL.
    RETURN.
  ENDIF. " IF pt_t001w IS INITIAL

  SELECT *
    FROM /vwk/maitpp211 " Controle de cobertura mínima para release unificado
    INTO TABLE pt_maitpp211
    FOR ALL ENTRIES IN pt_t001w
    WHERE matnr EQ p_matnr
      AND werks EQ pt_t001w-werks.

ENDFORM.
FORM zf_fill_total_lines_by_week CHANGING pt_mrp_total_lines TYPE ty_t_mrp_total_lines_werks.

  DATA: lv_lines TYPE n,     " Lines of type Numeric Text Fields
        lv_week  TYPE numc2, " Nº de dois dígitos
        lv_year  TYPE numc4. " Nº de dois dígitos

  lv_lines = lines( pt_mrp_total_lines ).

  READ TABLE pt_mrp_total_lines ASSIGNING FIELD-SYMBOL(<fs_first_line>) INDEX 1.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  READ TABLE pt_mrp_total_lines ASSIGNING FIELD-SYMBOL(<fs_last_line>) INDEX lv_lines.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  LOOP AT pt_mrp_total_lines ASSIGNING FIELD-SYMBOL(<fs_total_lines>).
    IF <fs_total_lines>-plngsegmt IS INITIAL.
      EXIT.
    ENDIF. " IF <fs_total_lines>-plngsegmt IS INITIAL
    IF sy-tabix EQ 1.
      IF <fs_total_lines>-week NA sy-abcde.
        lv_week = <fs_total_lines>-week.
      ENDIF. " IF <fs_total_lines>-week NA sy-abcde
      IF <fs_total_lines>-year NA sy-abcde.
        lv_year = <fs_total_lines>-year.
      ENDIF. " IF <fs_total_lines>-year NA sy-abcde
      CONTINUE.
    ENDIF. " IF sy-tabix EQ 1

    IF <fs_total_lines>-week NA sy-abcde
   AND <fs_total_lines>-year NA sy-abcde.
      WHILE <fs_total_lines>-week NE lv_week
        OR <fs_total_lines>-year NE lv_year.
        lv_week = lv_week + 1.
        IF lv_week GT 52.
          lv_week = 01.
          lv_year = lv_year + 1.
        ENDIF. " IF lv_week GT 52
        IF lv_week EQ <fs_total_lines>-week
       AND lv_year EQ <fs_total_lines>-year.
          EXIT.
        ENDIF. " IF lv_week EQ <fs_total_lines>-week
        APPEND INITIAL LINE TO pt_mrp_total_lines ASSIGNING FIELD-SYMBOL(<fs_total_lines_new>).
        <fs_total_lines_new>-werks = <fs_total_lines>-werks.
        CONCATENATE <fs_total_lines>-per_segmt(1) lv_week INTO <fs_total_lines_new>-per_segmt SEPARATED BY space.
        CONCATENATE <fs_total_lines_new>-per_segmt lv_year INTO <fs_total_lines_new>-per_segmt SEPARATED BY '/'.
        <fs_total_lines_new>-week = lv_week.
        <fs_total_lines_new>-year = lv_year.
      ENDWHILE.
    ENDIF. " IF <fs_total_lines>-week NA sy-abcde

    IF <fs_total_lines>-year NA sy-abcde.
      lv_year = <fs_total_lines>-year.
    ENDIF. " IF <fs_total_lines>-year NA sy-abcde

    IF <fs_total_lines>-week NA sy-abcde.
      lv_week = <fs_total_lines>-week.
    ENDIF. " IF <fs_total_lines>-week NA sy-abcde

  ENDLOOP. " LOOP AT pt_mrp_total_lines ASSIGNING FIELD-SYMBOL(<fs_total_lines>)

ENDFORM.
FORM zf_read_maitpp209_by_t001w USING pt_t001w TYPE ty_t_t001w
                             CHANGING pt_maitpp209 TYPE ty_t_maitpp209.

  DATA: lv_week TYPE scal-week. " Calendário de fábrica: semana

  CLEAR pt_maitpp209.

  IF pt_t001w IS INITIAL.
    RETURN.
  ENDIF. " IF pt_t001w IS INITIAL

  SELECT *
    FROM /vwk/maitpp209 " Controle de embarque - faturas
    INTO TABLE pt_maitpp209
    FOR ALL ENTRIES IN pt_t001w
    WHERE werks EQ pt_t001w-werks
      AND matnr EQ p_matnr
      AND belnr IN s_belnr
      AND eta IN s_fperio.

  LOOP AT pt_maitpp209 ASSIGNING FIELD-SYMBOL(<fs_maitpp209>).

    CLEAR lv_week.

    CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
      EXPORTING
        date = <fs_maitpp209>-eta
      IMPORTING
        week = lv_week.

    <fs_maitpp209>-year = lv_week(4).
    <fs_maitpp209>-week = lv_week+4(2).

  ENDLOOP. " LOOP AT pt_maitpp209 ASSIGNING FIELD-SYMBOL(<fs_maitpp209>)

ENDFORM.
FORM zf_read_maitpp210_by_selscreen CHANGING pt_maitpp210 TYPE ty_t_maitpp210.

  CLEAR pt_maitpp210.

  IF s_werks[] IS INITIAL.
    RETURN.
  ENDIF. " IF s_werks[] IS INITIAL
*
  SELECT werks lgort
    FROM /vwk/maitpp210 " Configuração de depósitos release unificado
    INTO TABLE pt_maitpp210
    WHERE werks IN s_werks.

ENDFORM.
FORM zf_read_mard_by_maitpp209 USING pt_maitpp209 TYPE ty_t_maitpp209
                            CHANGING pt_mard TYPE ty_t_mard.

  CLEAR pt_mard.

  IF pt_maitpp209 IS INITIAL.
    RETURN.
  ENDIF. " IF pt_maitpp209 IS INITIAL

  SELECT matnr werks labst " Estoque avaliado de utilização livre
    FROM mard              " Dados de depósito para material
    INTO TABLE pt_mard
    FOR ALL ENTRIES IN pt_maitpp209
    WHERE matnr EQ pt_maitpp209-matnr
      AND werks EQ pt_maitpp209-werks
      AND labst GT 0.

ENDFORM.
FORM zf_create_stock_table USING pt_t001w TYPE ty_t_t001w
                                 pt_mard TYPE ty_t_mard
                                 pt_maitpp211 TYPE ty_t_maitpp211.

  DATA: wa_stock     LIKE LINE OF gt_stock,
        lt_edit      LIKE wa_stock-edit,
        wa_edit      LIKE LINE OF wa_stock-edit,
        lv_min_cover TYPE /vwk/maispp083-min_cover,
        lv_stock     TYPE /vwk/maispp083-stock.

  CLEAR gt_stock.

  LOOP AT pt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>).

    CLEAR: wa_stock,
           lt_edit.

    wa_stock-werks = <fs_t001w>-werks.

    CLEAR wa_edit.
    wa_edit-style = cl_gui_alv_grid=>mc_style_disabled.
    wa_edit-fieldname = 'WERKS'.
    INSERT wa_edit INTO TABLE lt_edit.

    CLEAR wa_edit.
    wa_edit-style = cl_gui_alv_grid=>mc_style_disabled.
    wa_edit-fieldname = 'COVER'.
    INSERT wa_edit INTO TABLE lt_edit.

    READ TABLE pt_mard ASSIGNING FIELD-SYMBOL(<fs_mard>) WITH KEY matnr = p_matnr
                                                                  werks = <fs_t001w>-werks BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_stock-stock = <fs_mard>-labst.
    ENDIF. " IF sy-subrc IS INITIAL

    READ TABLE pt_maitpp211 ASSIGNING FIELD-SYMBOL(<fs_maitpp211>) WITH KEY matnr = p_matnr
                                                                            werks = <fs_t001w>-werks.
    IF sy-subrc IS INITIAL.
      wa_stock-min_cover = <fs_maitpp211>-min_cover.
      wa_stock-consumption = <fs_maitpp211>-consumption.
    ELSE. " ELSE -> IF sy-subrc IS INITIAL
      wa_stock-min_cover = 15.
    ENDIF. " IF sy-subrc IS INITIAL

    wa_stock-edit = lt_edit.

    lv_stock = lv_stock + wa_stock-stock.
    lv_min_cover = lv_min_cover + wa_stock-min_cover.

    APPEND wa_stock TO gt_stock.

  ENDLOOP. " LOOP AT pt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>)

  CLEAR wa_stock.
  wa_stock-werks = TEXT-t14.

  CLEAR wa_edit.
  wa_edit-style = cl_gui_alv_grid=>mc_style_disabled.
  wa_edit-fieldname = 'WERKS'.
  INSERT wa_edit INTO TABLE lt_edit.

  CLEAR wa_edit.
  wa_edit-style = cl_gui_alv_grid=>mc_style_disabled.
  wa_edit-fieldname = 'STOCK'.
  INSERT wa_edit INTO TABLE lt_edit.

  CLEAR wa_edit.
  wa_edit-style = cl_gui_alv_grid=>mc_style_disabled.
  wa_edit-fieldname = 'CONSUMPTION'.
  INSERT wa_edit INTO TABLE lt_edit.

  CLEAR wa_edit.
  wa_edit-style = cl_gui_alv_grid=>mc_style_disabled.
  wa_edit-fieldname = 'COVER'.
  INSERT wa_edit INTO TABLE lt_edit.

  CLEAR wa_edit.
  wa_edit-style = cl_gui_alv_grid=>mc_style_disabled.
  wa_edit-fieldname = 'MIN_COVER'.
  INSERT wa_edit INTO TABLE lt_edit.

  wa_stock-min_cover = lv_min_cover.
  wa_stock-stock = lv_stock.

  wa_stock-edit = lt_edit.
  APPEND wa_stock TO gt_stock.

ENDFORM.
FORM zf_collect_mard_labst CHANGING pt_mard TYPE ty_t_mard.

  DATA: lt_collect TYPE ty_t_mard.

  LOOP AT pt_mard ASSIGNING FIELD-SYMBOL(<fs_mard>).

    COLLECT <fs_mard> INTO lt_collect.

  ENDLOOP. " LOOP AT pt_mard ASSIGNING FIELD-SYMBOL(<fs_mard>)

  pt_mard = lt_collect.

ENDFORM.
FORM zf_create_containers.

  IF gv_container IS BOUND.
    RETURN.
  ENDIF. " IF gv_container IS BOUND

  CREATE OBJECT gv_container
    EXPORTING
      container_name = 'GV_GRID'.

  PERFORM zf_create_unif_rep_container.
  PERFORM zf_create_header_containers.

ENDFORM.
FORM zf_create_unif_rep_container.

  IF gv_unif_rep_split IS BOUND.
    RETURN.
  ENDIF. " IF gv_unif_rep_split IS BOUND

  CREATE OBJECT gv_unif_rep_split
    EXPORTING
      parent            = gv_container
      rows              = 2
      columns           = 1
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  CALL METHOD gv_unif_rep_split->set_row_height
    EXPORTING
      id                = 1
      height            = 25
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  gv_header_container = gv_unif_rep_split->get_container( row = 1 column = 1 ).
  gv_unif_rep_container = gv_unif_rep_split->get_container( row = 2 column = 1 ).

ENDFORM.
FORM zf_create_header_containers.

  IF gv_header_split IS BOUND.
    RETURN.
  ENDIF. " IF gv_header_split IS BOUND

  CREATE OBJECT gv_header_split
    EXPORTING
      parent            = gv_header_container
      rows              = 1
      columns           = 3
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  CALL METHOD gv_header_split->set_column_width
    EXPORTING
      id                = 1
      width             = 34
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  CALL METHOD gv_header_split->set_column_width
    EXPORTING
      id                = 2
      width             = 33
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  gv_stock_container = gv_header_split->get_container( row = 1 column = 1 ).
  gv_material_info_container = gv_header_split->get_container( row = 1 column = 2 ).
  gv_general_notes_container = gv_header_split->get_container( row = 1 column = 3 ).

ENDFORM.
FORM zf_create_stock_alv.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo,                " Controle VLA: estrutura layout
        lv_handler  TYPE REF TO lcl_event_receiver. " Event_receiver class
*        lt_exclude  TYPE ui_functions. " Código de função

  wa_layout-zebra = abap_true.
  wa_layout-cwidth_opt = abap_true.
  wa_layout-no_toolbar = abap_true.
  wa_layout-edit = abap_true.
  wa_layout-stylefname = 'EDIT'.

  PERFORM zf_create_stock_field_cat CHANGING lt_fieldcat.

  PERFORM zf_create_stock_grid.

  PERFORM zf_create_stock_handler CHANGING lv_handler.

  CALL METHOD gv_stock_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
*     it_toolbar_excluding          = lt_exclude
    CHANGING
      it_outtab                     = gt_stock
      it_fieldcatalog               = lt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_create_stock_field_cat CHANGING pt_fieldcat TYPE lvc_t_fcat.

  CLEAR pt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = c_structure-stock
    CHANGING
      ct_fieldcat            = pt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  LOOP AT pt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

    IF <fs_fieldcat>-fieldname EQ 'STOCK'
    OR <fs_fieldcat>-fieldname EQ 'CONSUMPTION'
    OR <fs_fieldcat>-fieldname EQ 'COVER'
    OR <fs_fieldcat>-fieldname EQ 'MIN_COVER'.
      <fs_fieldcat>-edit = abap_true.
    ENDIF. " IF <fs_fieldcat>-fieldname EQ 'STOCK'

  ENDLOOP. " LOOP AT pt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>)

ENDFORM.
FORM zf_create_stock_grid.

  IF gv_stock_grid IS BOUND.
    RETURN.
  ENDIF. " IF gv_stock_grid IS BOUND

  CREATE OBJECT gv_stock_grid
    EXPORTING
      i_parent = gv_stock_container.

  CALL METHOD gv_stock_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

ENDFORM.
FORM zf_create_stock_handler CHANGING pv_handler TYPE REF TO lcl_event_receiver. " Event_receiver class

  IF pv_handler IS BOUND.
    RETURN.
  ENDIF. " IF pv_handler IS BOUND

  CREATE OBJECT pv_handler.

  SET HANDLER pv_handler->handle_data_changed FOR gv_stock_grid.

ENDFORM.
FORM zf_create_material_info_alv.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo. " Controle VLA: estrutura layout

  wa_layout-zebra = abap_true.
  wa_layout-cwidth_opt = abap_true.
  wa_layout-no_toolbar = abap_true.

  PERFORM zf_create_material_info_fcat CHANGING lt_fieldcat.

  PERFORM zf_create_material_info_grid.

  CALL METHOD gv_material_info_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = gt_material_info
      it_fieldcatalog               = lt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_create_material_info_fcat CHANGING pt_fieldcat TYPE lvc_t_fcat.

  CLEAR pt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = c_structure-material_info
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
FORM zf_create_material_info_grid.

  IF gv_material_info_grid IS BOUND.
    RETURN.
  ENDIF. " IF gv_material_info_grid IS BOUND

  CREATE OBJECT gv_material_info_grid
    EXPORTING
      i_parent = gv_material_info_container.

ENDFORM.
FORM zf_read_makt_by_maitpp209 USING pt_maitpp209 TYPE ty_t_maitpp209
                            CHANGING pt_makt TYPE ty_t_makt.

  CLEAR pt_makt.

  IF pt_maitpp209 IS INITIAL.
    RETURN.
  ENDIF. " IF pt_maitpp209 IS INITIAL

  SELECT matnr maktx
    FROM makt " Textos breves de material
    INTO TABLE pt_makt
    FOR ALL ENTRIES IN pt_maitpp209
    WHERE matnr EQ pt_maitpp209-matnr
      AND spras EQ sy-langu.

ENDFORM.
FORM zf_read_lfa1_by_maitpp209 USING pt_maitpp209 TYPE ty_t_maitpp209
                            CHANGING pt_lfa1 TYPE ty_t_lfa1.

  CLEAR pt_lfa1.

  IF pt_maitpp209 IS INITIAL.
    RETURN.
  ENDIF. " IF pt_maitpp209 IS INITIAL

  SELECT lifnr name1
    FROM lfa1 " Mestre de fornecedores (parte geral)
    INTO TABLE pt_lfa1
    FOR ALL ENTRIES IN pt_maitpp209
    WHERE lifnr EQ pt_maitpp209-lifnr.

ENDFORM.
FORM zf_read_marc_by_maitpp209 USING pt_maitpp209 TYPE ty_t_maitpp209
                            CHANGING pt_marc TYPE ty_t_marc.

  CLEAR pt_marc.

  IF pt_maitpp209 IS INITIAL.
    RETURN.
  ENDIF. " IF pt_maitpp209 IS INITIAL

  SELECT matnr werks dispo
    FROM marc " Dados de centro para material
    INTO TABLE pt_marc
    FOR ALL ENTRIES IN pt_maitpp209
    WHERE matnr EQ pt_maitpp209-matnr
      AND werks EQ pt_maitpp209-werks.

ENDFORM.
FORM zf_read_t024d_by_marc USING pt_marc TYPE ty_t_marc
                        CHANGING pt_t024d TYPE ty_t_t024d.

  CLEAR pt_t024d.

  IF pt_marc IS INITIAL.
    RETURN.
  ENDIF. " IF pt_marc IS INITIAL

  SELECT werks dispo dsnam
    FROM t024d " Planejadores de material
    INTO TABLE pt_t024d
    FOR ALL ENTRIES IN pt_marc
    WHERE werks EQ pt_marc-werks
      AND dispo EQ pt_marc-dispo.

ENDFORM.
FORM zf_create_material_info_tale USING pt_maitpp209 TYPE ty_t_maitpp209
                                        pt_makt TYPE ty_t_makt
                                        pt_lfa1 TYPE ty_t_lfa1
                                        pt_marc TYPE ty_t_marc
                                        pt_t024d TYPE ty_t_t024d
                               CHANGING pt_material_info TYPE /vwk/maittpp023.

  DATA: wa_material_info LIKE LINE OF pt_material_info,
        lv_lifnr         TYPE char45,   " Lifnr of type CHAR45
        lv_dispo         TYPE char21,   " Dispo of type CHAR21
        lv_tabix         TYPE sy-tabix, " Campo do sistema ABAP: índice de linhas das tabelas internas
        lt_maitpp209     TYPE ty_t_maitpp209.

  CLEAR pt_material_info.

  lt_maitpp209 = pt_maitpp209.

  SORT lt_maitpp209 BY used_model.
  DELETE ADJACENT DUPLICATES FROM lt_maitpp209 COMPARING used_model.

  wa_material_info-field = 'Part Number' ##NO_TEXT.
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = p_matnr
    IMPORTING
      output = wa_material_info-value.
  APPEND wa_material_info TO pt_material_info.
  CLEAR wa_material_info.

  wa_material_info-field = TEXT-t15.

  READ TABLE pt_makt ASSIGNING FIELD-SYMBOL(<fs_makt>) WITH KEY matnr = p_matnr.
  IF sy-subrc IS INITIAL.
    wa_material_info-value = <fs_makt>-maktx.
  ENDIF. " IF sy-subrc IS INITIAL

  APPEND wa_material_info TO pt_material_info.
  CLEAR wa_material_info.

  wa_material_info-field = TEXT-t16.
  LOOP AT gt_werks ASSIGNING FIELD-SYMBOL(<fs_werks>).
    IF sy-tabix EQ 1.
      wa_material_info-value = <fs_werks>-werks.
      CONTINUE.
    ENDIF. " IF sy-tabix EQ 1
    CONCATENATE wa_material_info-value <fs_werks>-werks INTO wa_material_info-value SEPARATED BY ','.
  ENDLOOP. " LOOP AT gt_werks ASSIGNING FIELD-SYMBOL(<fs_werks>)

  APPEND wa_material_info TO pt_material_info.
  CLEAR wa_material_info.

  wa_material_info-field = TEXT-t17.
  LOOP AT lt_maitpp209 ASSIGNING FIELD-SYMBOL(<fs_maitpp209>).
    IF sy-tabix EQ 1.
      wa_material_info-value = <fs_maitpp209>-used_model.
      CONTINUE.
    ENDIF. " IF sy-tabix EQ 1
    CONCATENATE wa_material_info-value <fs_maitpp209>-used_model INTO wa_material_info-value SEPARATED BY ','.
  ENDLOOP. " LOOP AT lt_maitpp209 ASSIGNING FIELD-SYMBOL(<fs_maitpp209>)

  APPEND wa_material_info TO pt_material_info.
  CLEAR wa_material_info.

  wa_material_info-field = TEXT-t18.
  LOOP AT pt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>).

    CLEAR lv_lifnr.

    IF sy-tabix EQ 1.
      CONCATENATE <fs_lfa1>-lifnr <fs_lfa1>-name1 INTO wa_material_info-value SEPARATED BY '-'.
      wa_material_info-value = <fs_lfa1>-name1.
      CONTINUE.
    ENDIF. " IF sy-tabix EQ 1
    CONCATENATE <fs_lfa1>-lifnr <fs_lfa1>-name1 INTO lv_lifnr SEPARATED BY '-'.
    CONCATENATE wa_material_info-value lv_lifnr INTO wa_material_info-value SEPARATED BY ','.
  ENDLOOP. " LOOP AT pt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)

  APPEND wa_material_info TO pt_material_info.
  CLEAR wa_material_info.

  wa_material_info-field = TEXT-t19.
  LOOP AT pt_marc ASSIGNING FIELD-SYMBOL(<fs_marc>).

    CLEAR: lv_dispo,
           lv_tabix.

    lv_tabix = sy-tabix.

    READ TABLE pt_t024d ASSIGNING FIELD-SYMBOL(<fs_t024d>) WITH KEY werks = <fs_marc>-werks
                                                                    dispo = <fs_marc>-dispo.

    IF lv_tabix IS INITIAL.
      CONCATENATE <fs_marc>-dispo <fs_t024d>-dsnam INTO lv_dispo SEPARATED BY '-'.
      IF sy-tabix EQ 1.
        wa_material_info-value = lv_dispo.
        CONTINUE.
      ENDIF. " IF sy-tabix EQ 1
      CONCATENATE <fs_marc>-dispo <fs_t024d>-dsnam INTO lv_dispo SEPARATED BY '-'.
      CONCATENATE wa_material_info-value lv_dispo INTO wa_material_info-value SEPARATED BY ','.
    ELSE. " ELSE -> IF lv_tabix IS INITIAL
      IF lv_tabix EQ 1.
        wa_material_info-value = <fs_marc>-dispo.
        CONTINUE.
      ENDIF. " IF lv_tabix EQ 1
      CONCATENATE wa_material_info-value <fs_marc>-dispo INTO wa_material_info-value SEPARATED BY ','.
    ENDIF. " IF lv_tabix IS INITIAL
  ENDLOOP. " LOOP AT pt_marc ASSIGNING FIELD-SYMBOL(<fs_marc>)

  APPEND wa_material_info TO pt_material_info.

ENDFORM.
FORM zf_create_unified_report_table USING pt_mrp_total_lines TYPE ty_t_mrp_total_lines_werks
                                          pt_maitpp209 TYPE ty_t_maitpp209
                                          pt_werks TYPE ty_t_werks
                                 CHANGING pt_unif_rep TYPE /vwk/maittpp024.

  DATA: wa_unified_report LIKE LINE OF pt_unif_rep,
        lt_week           TYPE ty_t_week,
        lv_field          TYPE string.

  FIELD-SYMBOLS: <fs_field> TYPE any.

  CLEAR pt_unif_rep.

  lt_week = VALUE #( FOR wa IN pt_mrp_total_lines ( per_segmt = wa-per_segmt week = wa-per_segmt+2(2) year = wa-per_segmt+5(4) ) ).

  SORT lt_week BY year week.
  DELETE ADJACENT DUPLICATES FROM lt_week COMPARING year week.

  LOOP AT lt_week ASSIGNING FIELD-SYMBOL(<fs_week>).

    CLEAR wa_unified_report.

    READ TABLE pt_mrp_total_lines TRANSPORTING NO FIELDS WITH KEY per_segmt = <fs_week>-per_segmt.

    LOOP AT pt_mrp_total_lines ASSIGNING FIELD-SYMBOL(<fs_total_lines>) FROM sy-tabix.

      IF <fs_total_lines>-per_segmt NE <fs_week>-per_segmt.
        EXIT.
      ENDIF. " IF <fs_total_lines>-per_segmt NE <fs_week>-per_segmt

      READ TABLE pt_werks ASSIGNING FIELD-SYMBOL(<fs_werks>) WITH KEY werks = <fs_total_lines>-werks BINARY SEARCH.
      CHECK sy-subrc IS INITIAL.

      CONCATENATE 'DEM' <fs_werks>-field INTO lv_field SEPARATED BY '_'.
      ASSIGN COMPONENT lv_field OF STRUCTURE wa_unified_report TO <fs_field>.
      CHECK sy-subrc IS INITIAL.

      <fs_field> = ( <fs_total_lines>-pld_ind_reqs + <fs_total_lines>-reqmts ) * -1.

      CONCATENATE 'TRAN' <fs_werks>-field INTO lv_field SEPARATED BY '_'.
      ASSIGN COMPONENT lv_field OF STRUCTURE wa_unified_report TO <fs_field>.
      CHECK sy-subrc IS INITIAL.

      LOOP AT pt_maitpp209 ASSIGNING FIELD-SYMBOL(<fs_maitpp209>) WHERE year = <fs_total_lines>-year
                                                                    AND week = <fs_total_lines>-week
                                                                    AND werks = <fs_total_lines>-werks
                                                                    AND matnr = p_matnr.

        <fs_field> = <fs_field> + <fs_maitpp209>-menge.

      ENDLOOP. " LOOP AT pt_maitpp209 ASSIGNING FIELD-SYMBOL(<fs_maitpp209>) WHERE year = <fs_total_lines>-year
      UNASSIGN <fs_field>.

      CONCATENATE 'COVER' <fs_werks>-field INTO lv_field SEPARATED BY '_'.
      ASSIGN COMPONENT lv_field OF STRUCTURE wa_unified_report TO <fs_field>.
      CHECK sy-subrc IS INITIAL.

      READ TABLE gt_stock ASSIGNING FIELD-SYMBOL(<fs_stock>) WITH KEY werks = <fs_werks>-werks.
      CHECK sy-subrc IS INITIAL.

      IF <fs_field> LT <fs_stock>-min_cover.
        PERFORM zf_add_color_field USING lv_field
                                         '6'
                                CHANGING wa_unified_report-color.
      ENDIF. " IF <fs_field> LT <fs_stock>-min_cover

    ENDLOOP. " LOOP AT pt_mrp_total_lines ASSIGNING FIELD-SYMBOL(<fs_total_lines>) FROM sy-tabix

    wa_unified_report-per_segmt = <fs_week>-per_segmt.
    wa_unified_report-total_demand = wa_unified_report-dem_werks1 + wa_unified_report-dem_werks2 + wa_unified_report-dem_werks3 + wa_unified_report-dem_werks4.
    wa_unified_report-total_transit = wa_unified_report-tran_werks1 + wa_unified_report-tran_werks2 + wa_unified_report-tran_werks3 + wa_unified_report-tran_werks4.
    APPEND wa_unified_report TO pt_unif_rep.

  ENDLOOP. " LOOP AT lt_week ASSIGNING FIELD-SYMBOL(<fs_week>)

  PERFORM zf_update_cover USING gt_stock
                       CHANGING pt_unif_rep.

ENDFORM.
FORM zf_create_unif_rep_alv.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo,                " Controle VLA: estrutura layout
        lv_handler  TYPE REF TO lcl_event_receiver. " Código de função

  wa_layout-zebra = abap_true.
  wa_layout-cwidth_opt = abap_true.
  wa_layout-ctab_fname = 'COLOR'.

  PERFORM zf_create_unif_rep_fcat CHANGING lt_fieldcat.

  PERFORM zf_create_unif_rep_grid.

  PERFORM zf_create_unif_rep_handler CHANGING lv_handler.

  CALL METHOD gv_unif_rep_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = gt_unif_rep
      it_fieldcatalog               = lt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_create_unif_rep_fcat CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA: lt_t001w TYPE ty_t_t001w.

  CLEAR pt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = c_structure-unif_rep
    CHANGING
      ct_fieldcat            = pt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  PERFORM zf_read_t001w_by_selscreen CHANGING lt_t001w.

  LOOP AT lt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>).

    TRANSLATE <fs_t001w>-name1 TO LOWER CASE.

  ENDLOOP. " LOOP AT lt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>)

  LOOP AT pt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

    CASE <fs_fieldcat>-fieldname.
      WHEN 'TOTAL_DEMAND'.
        <fs_fieldcat>-coltext = TEXT-t08.
        <fs_fieldcat>-emphasize = 'C100'.

      WHEN 'TOTAL_TRANSIT'.
        <fs_fieldcat>-coltext = TEXT-t09.
        <fs_fieldcat>-emphasize = 'C100'.

      WHEN 'TOTAL_COVER'.
        <fs_fieldcat>-coltext = TEXT-t10.
        <fs_fieldcat>-emphasize = 'C100'.

      WHEN 'STOCK_WERKS1' OR 'STOCK_WERKS2' OR 'STOCK_WERKS3' OR 'STOCK_WERKS4'.
        <fs_fieldcat>-tech = abap_true.

    ENDCASE.

    CASE <fs_fieldcat>-fieldname+4(6).
      WHEN 'WERKS1'.
        READ TABLE gt_werks ASSIGNING FIELD-SYMBOL(<fs_werks>) WITH KEY field = 'WERKS1'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t11 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t11 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.
        <fs_fieldcat>-hotspot = abap_true.

      WHEN 'WERKS2'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS2'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t11 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t11 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.
        <fs_fieldcat>-hotspot = abap_true.

      WHEN 'WERKS3'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS3'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t11 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t11 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.
        <fs_fieldcat>-hotspot = abap_true.

      WHEN 'WERKS4'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS4'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t11 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t11 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.
        <fs_fieldcat>-hotspot = abap_true.

    ENDCASE.

    CASE <fs_fieldcat>-fieldname+5(6).
      WHEN 'WERKS1'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS1'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.
        <fs_fieldcat>-hotspot = abap_true.

      WHEN 'WERKS2'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS2'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.
        <fs_fieldcat>-hotspot = abap_true.

      WHEN 'WERKS3'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS3'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.
        <fs_fieldcat>-hotspot = abap_true.

      WHEN 'WERKS4'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS4'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.
        <fs_fieldcat>-hotspot = abap_true.

    ENDCASE.

    CASE <fs_fieldcat>-fieldname+6(6).
      WHEN 'WERKS1'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS1'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t13 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t13 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.

      WHEN 'WERKS2'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS2'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t13 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.

      WHEN 'WERKS3'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS3'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t13 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.

      WHEN 'WERKS4'.
        READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = 'WERKS4'.
        CHECK sy-subrc IS INITIAL.
        READ TABLE lt_t001w ASSIGNING <fs_t001w> WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.
        CONCATENATE TEXT-t13 <fs_t001w>-name1 INTO <fs_fieldcat>-coltext SEPARATED BY space.
        CONCATENATE TEXT-t12 <fs_t001w>-name1 INTO <fs_fieldcat>-reptext SEPARATED BY space.

    ENDCASE.

  ENDLOOP. " LOOP AT pt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>)


  IF lines( gt_werks ) = 1.
    DELETE pt_fieldcat WHERE fieldname CA '2'
                          OR fieldname CA '3'
                          OR fieldname CA '4'.
  ELSEIF lines( gt_werks ) = 2.
    DELETE pt_fieldcat WHERE fieldname CA '3'
                          OR fieldname CA '4'.
  ELSEIF lines( gt_werks ) = 3.
    DELETE pt_fieldcat WHERE fieldname CA '4'.
  ENDIF. " IF lines( gt_werks ) = 1

ENDFORM.
FORM zf_create_unif_rep_grid.

  IF gv_unif_rep_grid IS BOUND.
    RETURN.
  ENDIF. " IF gv_unif_rep_grid IS BOUND

  CREATE OBJECT gv_unif_rep_grid
    EXPORTING
      i_parent = gv_unif_rep_container.

ENDFORM.
FORM zf_create_unif_rep_handler CHANGING pv_handler TYPE REF TO lcl_event_receiver. " Event_receiver class

  IF pv_handler IS BOUND.
    RETURN.
  ENDIF. " IF pv_handler IS BOUND

  CREATE OBJECT pv_handler.

  SET HANDLER pv_handler->handle_hotspot_click FOR gv_unif_rep_grid.

ENDFORM.
FORM zf_update_stock_totals CHANGING pt_stock TYPE /vwk/maittpp022.

  DATA: lv_stock       TYPE /vwk/maispp083-stock,       " Quantidade de estoque do release unificado
        lv_consumption TYPE /vwk/maispp083-consumption, " Quantidade de consumo do release unificado
        lv_cover       TYPE /vwk/maispp083-cover,       " Quantidade de cobertura do release unificado
        lv_min_cover   TYPE /vwk/maispp083-min_cover,   " Quantidade de cobertura do release unificado
        lv_lines       TYPE n.                          " Lines of type Numeric Text Fields

  lv_lines = lines( gt_stock ).

  LOOP AT pt_stock ASSIGNING FIELD-SYMBOL(<fs_stock>).

    IF sy-tabix EQ lv_lines.
      <fs_stock>-stock = lv_stock.
      <fs_stock>-min_cover = lv_min_cover.
      lv_lines = lv_lines - 1.
      IF lv_lines GT 0.
        <fs_stock>-cover = lv_cover / lv_lines.
        <fs_stock>-consumption = lv_consumption / lv_lines.
      ENDIF. " IF lv_lines GT 0
      CONTINUE.
    ENDIF. " IF sy-tabix EQ lv_lines

    lv_stock = lv_stock + <fs_stock>-stock.
    lv_consumption = lv_consumption + <fs_stock>-consumption.
    lv_min_cover = lv_min_cover + <fs_stock>-min_cover.
    IF <fs_stock>-consumption GT 0.
      lv_cover = <fs_stock>-stock / <fs_stock>-consumption.
      <fs_stock>-cover = <fs_stock>-stock / <fs_stock>-consumption.
    ENDIF. " IF <fs_stock>-consumption GT 0

  ENDLOOP. " LOOP AT pt_stock ASSIGNING FIELD-SYMBOL(<fs_stock>)

ENDFORM.
FORM zf_update_cover USING pt_stock TYPE /vwk/maittpp022
                  CHANGING pt_unif_rep TYPE /vwk/maittpp024.

  DATA: lv_per_segmt    TYPE /vwk/maispp085-per_segmt, " Período ou segmento
        lv_field        TYPE char6,                    " Field of type CHAR6
        lv_field2       TYPE string,
        lv_index        TYPE n,                        " Index of type Numeric Text Fields
        lv_stock_werks1 TYPE menge_d,                  " Período ou segmento
        lv_stock_werks2 TYPE menge_d,                  " Período ou segmento
        lv_stock_werks3 TYPE menge_d,                  " Período ou segmento
        lv_stock_werks4 TYPE menge_d,                  " Quantidade
        lv_stock        TYPE menge_d.                  " Período ou segmento

  FIELD-SYMBOLS: <fs_field>  TYPE any,
                 <fs_field2> TYPE any,
                 <fs_field3> TYPE any,
                 <fs_field4> TYPE any.

  LOOP AT pt_unif_rep ASSIGNING FIELD-SYMBOL(<fs_unif_rep>).

    CLEAR: <fs_unif_rep>-stock_werks1, <fs_unif_rep>-stock_werks2, <fs_unif_rep>-stock_werks3,
           <fs_unif_rep>-stock_werks4, <fs_unif_rep>-total_cover, <fs_unif_rep>-cover_werks1,
           <fs_unif_rep>-cover_werks2, <fs_unif_rep>-cover_werks3, <fs_unif_rep>-cover_werks4.


    IF sy-tabix EQ 1.
      lv_per_segmt = <fs_unif_rep>-per_segmt.
    ENDIF. " IF sy-tabix EQ 1

    IF <fs_unif_rep>-per_segmt EQ lv_per_segmt.
      DO lines( gt_werks ) TIMES.

        lv_index = sy-index.
        CONCATENATE 'WERKS' lv_index INTO lv_field.

        READ TABLE gt_werks ASSIGNING FIELD-SYMBOL(<fs_werks>) WITH KEY field = lv_field.
        CHECK sy-subrc IS INITIAL.

        READ TABLE pt_stock ASSIGNING FIELD-SYMBOL(<fs_stock>) WITH KEY werks = <fs_werks>-werks.
        CHECK sy-subrc IS INITIAL.

        CONCATENATE 'DEM' lv_field INTO lv_field2 SEPARATED BY '_'.

        ASSIGN COMPONENT lv_field2 OF STRUCTURE <fs_unif_rep> TO <fs_field2>.
        CHECK sy-subrc IS INITIAL.

        CONCATENATE 'TRAN' lv_field INTO lv_field2 SEPARATED BY '_'.

        ASSIGN COMPONENT lv_field2 OF STRUCTURE <fs_unif_rep> TO <fs_field3>.
        CHECK sy-subrc IS INITIAL.

        CONCATENATE 'STOCK' lv_field INTO lv_field2 SEPARATED BY '_'.

        ASSIGN COMPONENT lv_field2 OF STRUCTURE <fs_unif_rep> TO <fs_field4>.
        CHECK sy-subrc IS INITIAL.
        <fs_field4> = <fs_stock>-stock - <fs_field2> + <fs_field3>.
        IF <fs_field4> LT 0.
          <fs_field4> = 0.
        ENDIF. " IF <fs_field4> LT 0

        CASE lv_index.
          WHEN 1.
            lv_stock_werks1 = <fs_field4>.

          WHEN 2.
            lv_stock_werks2 = <fs_field4>.

          WHEN 3.
            lv_stock_werks3 = <fs_field4>.

          WHEN 4.
            lv_stock_werks4 = <fs_field4>.

        ENDCASE.
      ENDDO.

    ELSE. " ELSE -> IF <fs_unif_rep>-per_segmt EQ lv_per_segmt

      DO lines( gt_werks ) TIMES.

        lv_index = sy-index.
        CONCATENATE 'WERKS' lv_index INTO lv_field.

        CONCATENATE 'DEM' lv_field INTO lv_field2 SEPARATED BY '_'.

        ASSIGN COMPONENT lv_field2 OF STRUCTURE <fs_unif_rep> TO <fs_field>.
        CHECK sy-subrc IS INITIAL.

        CONCATENATE 'TRAN' lv_field INTO lv_field2 SEPARATED BY '_'.

        ASSIGN COMPONENT lv_field2 OF STRUCTURE <fs_unif_rep> TO <fs_field2>.
        CHECK sy-subrc IS INITIAL.

        CONCATENATE 'STOCK' lv_field INTO lv_field2 SEPARATED BY '_'.

        ASSIGN COMPONENT lv_field2 OF STRUCTURE <fs_unif_rep> TO <fs_field3>.
        CHECK sy-subrc IS INITIAL.

        CASE lv_index.
          WHEN 1.
            <fs_field3> = lv_stock_werks1 - <fs_field> + <fs_field2>.
            lv_stock_werks1 = <fs_field3>.

          WHEN 2.
            <fs_field3> = lv_stock_werks2 - <fs_field> + <fs_field2>.
            lv_stock_werks2 = <fs_field3>.

          WHEN 3.
            <fs_field3> = lv_stock_werks3 - <fs_field> + <fs_field2>.
            lv_stock_werks3 = <fs_field3>.

          WHEN 4.
            <fs_field3> = lv_stock_werks4 - <fs_field> + <fs_field2>.
            lv_stock_werks4 = <fs_field3>.

        ENDCASE.
      ENDDO.

    ENDIF. " IF <fs_unif_rep>-per_segmt EQ lv_per_segmt

    DO lines( gt_werks ) TIMES.

      lv_index = sy-index.
      CONCATENATE 'WERKS' lv_index INTO lv_field.

      READ TABLE gt_werks ASSIGNING <fs_werks> WITH KEY field = lv_field(6).
      CHECK sy-subrc IS INITIAL.

      READ TABLE pt_stock ASSIGNING <fs_stock> WITH KEY werks = <fs_werks>-werks.
      CHECK sy-subrc IS INITIAL.

      lv_stock = <fs_stock>-consumption * 5.

      CONCATENATE 'STOCK' lv_field INTO lv_field2 SEPARATED BY '_'.

      ASSIGN COMPONENT lv_field2 OF STRUCTURE <fs_unif_rep> TO <fs_field>.
      CHECK sy-subrc IS INITIAL.

      CONCATENATE 'COVER' lv_field INTO lv_field2 SEPARATED BY '_'.

      ASSIGN COMPONENT lv_field2 OF STRUCTURE <fs_unif_rep> TO <fs_field2>.
      CHECK sy-subrc IS INITIAL.

      IF lv_stock IS NOT INITIAL.
        <fs_field2> = <fs_field> / lv_stock.
      ENDIF. " IF lv_stock IS NOT INITIAL

      IF <fs_field2> LT <fs_stock>-min_cover.
        PERFORM zf_add_color_field USING lv_field2
                                         '6'
                                CHANGING <fs_unif_rep>-color.
      ELSE. " ELSE -> IF <fs_field2> LT <fs_stock>-min_cover
        CLEAR <fs_unif_rep>-color.
      ENDIF. " IF <fs_field2> LT <fs_stock>-min_cover

    ENDDO.

    IF lines( gt_werks ) GT 0.
      <fs_unif_rep>-total_cover = ( <fs_unif_rep>-cover_werks1 + <fs_unif_rep>-cover_werks2 + <fs_unif_rep>-cover_werks3 + <fs_unif_rep>-cover_werks4 ) / lines( gt_werks ).
    ENDIF. " IF lines( gt_werks ) GT 0

  ENDLOOP. " LOOP AT pt_unif_rep ASSIGNING FIELD-SYMBOL(<fs_unif_rep>)

ENDFORM.
FORM zf_create_general_notes_field.

  IF gv_general_notes_txt IS NOT BOUND.
    CREATE OBJECT gv_general_notes_txt
      EXPORTING
        parent                     = gv_general_notes_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
  ENDIF. " IF gv_general_notes_txt IS NOT BOUND

  gv_general_notes_txt->set_readonly_mode( ).

  PERFORM zf_get_general_notes_history CHANGING gv_general_notes_txt.

ENDFORM.
FORM zf_save_general_notes_text.

  DATA: lt_notfulltxti TYPE ty_t_notfulltxti.

  PERFORM zf_get_general_notes_text CHANGING lt_notfulltxti.

  PERFORM zf_save_long_text USING lt_notfulltxti.

ENDFORM.
FORM zf_get_general_notes_text CHANGING pt_notfulltxti TYPE ty_t_notfulltxti.

  DATA: lt_long_text   TYPE ty_t_long_text,
        wa_notfulltxti LIKE LINE OF pt_notfulltxti.

  CLEAR pt_notfulltxti.

  CALL METHOD gv_general_notes_txt_edit->get_text_as_r3table
    IMPORTING
      table = lt_long_text.

  LOOP AT lt_long_text ASSIGNING FIELD-SYMBOL(<fs_long_text>).

    CLEAR wa_notfulltxti.

    wa_notfulltxti-text_line = <fs_long_text>.
    APPEND wa_notfulltxti TO pt_notfulltxti.

  ENDLOOP. " LOOP AT lt_long_text ASSIGNING FIELD-SYMBOL(<fs_long_text>)

ENDFORM.
FORM zf_save_long_text USING pt_notfulltxti TYPE ty_t_notfulltxti.

  DATA: lt_long_text_save TYPE ty_t_tline,
        wa_header         TYPE thead, " SAPscript: cabeçalho de texto
        lt_lines          TYPE ty_t_tline.

  IF pt_notfulltxti IS INITIAL.
    RETURN.
  ENDIF. " IF pt_notfulltxti IS INITIAL

  CONCATENATE c_general_notes_text-name p_matnr INTO wa_header-tdname SEPARATED BY '_'.
  wa_header-tdspras = sy-langu.
  wa_header-tdobject = c_general_notes_text-object.
  wa_header-tdid = c_general_notes_text-id.

  PERFORM zf_call_read_text USING wa_header-tdname
                         CHANGING lt_lines.

  PERFORM zf_format_text_save USING pt_notfulltxti
                                    lt_lines
                           CHANGING lt_long_text_save.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      client          = sy-mandt
      header          = wa_header
      savemode_direct = abap_true
    TABLES
      lines           = lt_long_text_save
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  CALL FUNCTION 'COMMIT_TEXT'
    EXPORTING
      object = c_general_notes_text-object
      name   = wa_header-tdname.

  PERFORM zf_get_general_notes_history CHANGING gv_general_notes_txt.

ENDFORM.
FORM zf_format_text_save USING pt_notfulltxti TYPE ty_t_notfulltxti
                               pt_historic TYPE ty_t_tline
                      CHANGING pt_long_text_save TYPE ty_t_tline.

  DATA: lv_datum          TYPE char10, " Datum of type CHAR10
        lv_uzeit          TYPE char8,  " Uzeit of type CHAR8
        wa_long_text_save LIKE LINE OF pt_long_text_save.

  CLEAR pt_long_text_save.

  CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO lv_datum SEPARATED BY '/'.

  CONCATENATE sy-uzeit(2) sy-uzeit+2(2) sy-uzeit+4(2) INTO lv_uzeit SEPARATED BY ':'.

  INSERT INITIAL LINE INTO pt_long_text_save ASSIGNING FIELD-SYMBOL(<fs_long_text>) INDEX 1.
  CONCATENATE sy-uname lv_datum lv_uzeit INTO <fs_long_text>-tdline SEPARATED BY space.
  <fs_long_text>-tdformat = '*'.

  LOOP AT pt_notfulltxti ASSIGNING FIELD-SYMBOL(<fs_notfulltxti>).

    CLEAR wa_long_text_save.

    wa_long_text_save-tdformat = '*'.
    wa_long_text_save-tdline = <fs_notfulltxti>-text_line.
    APPEND wa_long_text_save TO pt_long_text_save.

  ENDLOOP. " LOOP AT pt_notfulltxti ASSIGNING FIELD-SYMBOL(<fs_notfulltxti>)

  CLEAR wa_long_text_save.
  APPEND wa_long_text_save TO pt_long_text_save.

  APPEND LINES OF pt_historic TO pt_long_text_save.

ENDFORM.
FORM zf_get_general_notes_history CHANGING pv_notes_txt TYPE REF TO cl_gui_textedit. " Gui_textedit class

  DATA: lv_name    TYPE thead-tdname,              " Nome
        lt_lines   TYPE ty_t_tline,                " Nome
        lt_r3table TYPE STANDARD TABLE OF char255, " R3table type standard table of type CHAR255
        wa_r3table LIKE LINE OF lt_r3table.

  CONCATENATE c_general_notes_text-name p_matnr INTO lv_name SEPARATED BY '_'.

  PERFORM zf_call_read_text USING lv_name
                         CHANGING lt_lines.

  IF lt_lines IS INITIAL.
    RETURN.
  ENDIF. " IF lt_lines IS INITIAL

  LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).

    wa_r3table = <fs_lines>-tdline.
    APPEND wa_r3table TO lt_r3table.

  ENDLOOP. " LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>)

  pv_notes_txt->set_text_as_r3table(
  EXPORTING
    table = lt_r3table
  EXCEPTIONS
    error_dp = 1
    OTHERS = 2 ).

  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_add_color_field USING pv_field TYPE any
                              pv_col TYPE lvc_col " Controle VLA: cor
                     CHANGING pt_color TYPE lvc_t_scol.

  DATA:ls_color LIKE LINE OF pt_color.

  ls_color-fname     = pv_field.
  ls_color-color-col = pv_col.

  APPEND ls_color TO pt_color.

ENDFORM.
FORM zf_call_read_text USING pv_name TYPE thead-tdname " SAPscript: cabeçalho de texto
                    CHANGING pt_lines TYPE ty_t_tline.

  CLEAR pt_lines.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = c_general_notes_text-id
      language                = sy-langu
      name                    = pv_name
      object                  = c_general_notes_text-object
    TABLES
      lines                   = pt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc IS INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS INITIAL

ENDFORM.
FORM zf_create_week_info_container.

  IF gv_week_info_container IS BOUND.
    RETURN.
  ENDIF. " IF gv_week_info_container IS BOUND

  CREATE OBJECT gv_week_info_container
    EXPORTING
      container_name = 'GV_WEEK_INFO_GRID'.

ENDFORM.
FORM zf_create_week_info_alv.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_layout   TYPE lvc_s_layo, " Controle VLA: estrutura layout
        wa_row      TYPE lvc_s_row,  " Controle VLA: descrição de uma linha
        wa_column   TYPE lvc_s_col,  " Controle VLA: ID colunas
        wa_row_no   TYPE lvc_s_roid. " Código de função

  wa_layout-zebra = abap_true.
  wa_layout-cwidth_opt = abap_true.
  wa_layout-ctab_fname = 'COLOR'.

  PERFORM zf_create_week_info_fcat CHANGING lt_fieldcat.

  PERFORM zf_create_week_info_grid.

  CALL METHOD gv_week_info_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = gt_week_info
      it_fieldcatalog               = lt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  IMPORT wa_row TO wa_row FROM MEMORY ID 'ZUNIFREP_ROW'.
  IMPORT wa_column TO wa_column FROM MEMORY ID 'ZUNIFREP_COL'.
  IMPORT wa_row_no TO wa_row_no FROM MEMORY ID 'ZUNIFREP_ROLNO'.

  gv_week_info_grid->set_scroll_info_via_id( EXPORTING is_row_info = wa_row
                                                       is_col_info = wa_column
                                                       is_row_no   = wa_row_no ).

ENDFORM.
FORM zf_create_week_info_fcat CHANGING pt_fieldcat TYPE lvc_t_fcat.

  CLEAR pt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = c_structure-week_info
    CHANGING
      ct_fieldcat            = pt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  IF flg_tran IS NOT INITIAL.
    DELETE pt_fieldcat WHERE fieldname EQ 'ELEMNT_DATA'.
  ELSE. " ELSE -> IF flg_tran IS NOT INITIAL
    DELETE pt_fieldcat WHERE fieldname EQ 'BELNR'.
  ENDIF. " IF flg_tran IS NOT INITIAL

ENDFORM.
FORM zf_create_week_info_grid.

  IF gv_week_info_grid IS BOUND.
    RETURN.
  ENDIF. " IF gv_week_info_grid IS BOUND

  CREATE OBJECT gv_week_info_grid
    EXPORTING
      i_parent = gv_week_info_container.

ENDFORM.
FORM zf_create_gen_notes_containers.

  IF gv_gen_notes_container IS BOUND.
    RETURN.
  ENDIF. " IF gv_gen_notes_container IS BOUND

  CREATE OBJECT gv_gen_notes_container
    EXPORTING
      container_name = 'GV_GENERAL_NOTES_GRID'.

  PERFORM zf_split_gen_notes_container.

ENDFORM.
FORM zf_split_gen_notes_container.

  IF gv_general_notes_split IS BOUND.
    RETURN.
  ENDIF. " IF gv_general_notes_split IS BOUND

  CREATE OBJECT gv_general_notes_split
    EXPORTING
      parent            = gv_gen_notes_container
      rows              = 2
      columns           = 1
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  CALL METHOD gv_general_notes_split->set_row_height
    EXPORTING
      id                = 1
      height            = 75
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  gv_gen_notes_edit_container = gv_general_notes_split->get_container( row = 1 column = 1 ).
  gv_gen_notes_hist_container = gv_general_notes_split->get_container( row = 2 column = 1 ).

ENDFORM.
FORM zf_create_gen_notes_text.

  IF gv_general_notes_txt_edit IS NOT BOUND.
    CREATE OBJECT gv_general_notes_txt_edit
      EXPORTING
        parent                     = gv_gen_notes_edit_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
  ENDIF. " IF gv_general_notes_txt_edit IS NOT BOUND

  IF gv_general_notes_txt_hist IS NOT BOUND.
    CREATE OBJECT gv_general_notes_txt_hist
      EXPORTING
        parent                     = gv_gen_notes_hist_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
  ENDIF. " IF gv_general_notes_txt_hist IS NOT BOUND

  gv_general_notes_txt_edit->set_textstream( space ).
  gv_general_notes_txt_hist->set_readonly_mode( ).
  gv_general_notes_txt_hist->set_toolbar_mode( 0 ).

ENDFORM.
FORM zf_save_rep_version.

  DATA: lv_name      TYPE /vwk/maitpp212-name, " Nome
        lv_subrc     TYPE sy-subrc,            " Campo do sistema ABAP: código de retorno de instruções ABAP
        lv_answer    TYPE c VALUE '1',         " Answer of type Character
        wa_maitpp212 TYPE /vwk/maitpp212,      " Nome
        lv_id        TYPE /vwk/maitpp212-id.   " Nome

  PERFORM zf_get_name_from_popup CHANGING lv_name.

  IF lv_name IS INITIAL.
    RETURN.
  ENDIF. " IF lv_name IS INITIAL

  PERFORM zf_check_exist_version_datum CHANGING lv_subrc.

  IF lv_subrc IS INITIAL.
    PERFORM zf_confirm_vers_sub_from_popup CHANGING lv_answer.
  ENDIF. " IF lv_subrc IS INITIAL

  IF lv_answer NE '1'.
    RETURN.
  ENDIF. " IF lv_answer NE '1'

  PERFORM zf_read_maitpp212_by_datum CHANGING wa_maitpp212.

  IF wa_maitpp212 IS NOT INITIAL.
    DELETE FROM /vwk/maitpp214 WHERE id EQ wa_maitpp212-id.
    DELETE FROM /vwk/maitpp213 WHERE id EQ wa_maitpp212-id.
    DELETE FROM /vwk/maitpp212 WHERE id EQ wa_maitpp212-id.
    DELETE FROM /vwk/maitpp215 WHERE id EQ wa_maitpp212-id.
  ELSE. " ELSE -> IF wa_maitpp212 IS NOT INITIAL
    PERFORM zf_get_next_id_from_maitpp212 CHANGING lv_id.
    lv_id = lv_id + 1.
  ENDIF. " IF wa_maitpp212 IS NOT INITIAL

  PERFORM zf_fill_maitpp212_by_unif_rep USING lv_name
                                              lv_id
                                     CHANGING wa_maitpp212.

  PERFORM zf_fill_maitpp213_by_stock USING wa_maitpp212-id.

  PERFORM zf_fill_maitpp214_by_unif_rep USING wa_maitpp212-id.

  PERFORM zf_fill_maitpp215_by_mat_info USING wa_maitpp212-id.

  MESSAGE s000(oo) WITH TEXT-s01. " & & & &

ENDFORM.
FORM zf_get_name_from_popup CHANGING pv_name TYPE /vwk/maitpp212-name. " Nome

  DATA: lt_fields        TYPE STANDARD TABLE OF sval, " Interface para grupo de funções SP04
        wa_field         LIKE LINE OF lt_fields,
        lv_returncode(1).

  CLEAR pv_name.

  wa_field-tabname = '/VWK/MAITPP212'.
  wa_field-fieldname = 'NAME'.
  wa_field-fieldtext = TEXT-t20.
  wa_field-field_obl = abap_true.
  wa_field-novaluehlp = abap_true.

  APPEND wa_field TO lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-t21
    IMPORTING
      returncode      = lv_returncode
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc IS NOT INITIAL
  OR lv_returncode EQ 'A'.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>) WITH KEY fieldname = 'NAME'.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

  pv_name = <fs_fields>-value.

ENDFORM.
FORM zf_check_exist_version_datum CHANGING pv_subrc TYPE sy-subrc. " Campo do sistema ABAP: código de retorno de instruções ABAP

  CLEAR pv_subrc.

  SELECT COUNT(*) UP TO 1 ROWS
    FROM /vwk/maitpp212 " Cabeçalho do relatório de release unificado
    WHERE date_created EQ sy-datum.

  pv_subrc = sy-subrc.

ENDFORM.
FORM zf_confirm_vers_sub_from_popup CHANGING pv_answer TYPE c. " Confirm_vers_sub_from_ of type Character

  CLEAR pv_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = TEXT-t23
      display_cancel_button = abap_false
    IMPORTING
      answer                = pv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF. " IF sy-subrc IS NOT INITIAL

ENDFORM.
FORM zf_read_maitpp212_by_datum CHANGING pwa_maitpp212 TYPE /vwk/maitpp212. " Cabeçalho do relatório de release unificado

  CLEAR pwa_maitpp212.

  SELECT * UP TO 1 ROWS
    FROM /vwk/maitpp212 " Cabeçalho do relatório de release unificado
    INTO pwa_maitpp212
    WHERE date_created EQ sy-datum.
  ENDSELECT.

ENDFORM.
FORM zf_get_next_id_from_maitpp212 CHANGING pv_id TYPE /vwk/maitpp212-id. " ID da variante do relatório de release unificado

  CLEAR pv_id.

  SELECT MAX( id )
    FROM /vwk/maitpp212 " Cabeçalho do relatório de release unificado
    INTO pv_id.

ENDFORM.
FORM zf_fill_maitpp212_by_unif_rep USING pv_name TYPE /vwk/maitpp212-name   " Nome
                                         pv_id TYPE /vwk/maitpp212-id       " ID da variante do relatório de release unificado
                                CHANGING pwa_maitpp212 TYPE /vwk/maitpp212. " Cabeçalho do relatório de release unificado
  IF pwa_maitpp212-id IS INITIAL.
    pwa_maitpp212-id = pv_id.
  ENDIF. " IF pwa_maitpp212-id IS INITIAL
  pwa_maitpp212-name = pv_name.
  pwa_maitpp212-matnr = p_matnr.
  pwa_maitpp212-date_created = sy-datum.
  pwa_maitpp212-uzeit = sy-uzeit.
  pwa_maitpp212-uname = sy-uname.

  INSERT /vwk/maitpp212 FROM pwa_maitpp212.

ENDFORM.
FORM zf_fill_maitpp213_by_stock USING pv_id TYPE /vwk/maitpp212-id. " ID da variante do relatório de release unificado

  DATA: lt_maitpp213 TYPE ty_t_maitpp213,
        wa_maitpp213 LIKE LINE OF lt_maitpp213, " Estoque do relatório de release unificado
        lt_stock     TYPE /vwk/maittpp022,
        lv_lines     TYPE n.                    " Lines of type Numeric Text Fields

  lt_stock = gt_stock.
  lv_lines = lines( lt_stock ).
  DELETE lt_stock INDEX lv_lines.

  LOOP AT lt_stock ASSIGNING FIELD-SYMBOL(<fs_stock>).

    CLEAR wa_maitpp213.
    wa_maitpp213-id = pv_id.
    wa_maitpp213-werks = <fs_stock>-werks.
    wa_maitpp213-stock = <fs_stock>-stock.
    wa_maitpp213-consumption = <fs_stock>-consumption.
    wa_maitpp213-min_cover = <fs_stock>-min_cover.

    APPEND wa_maitpp213 TO lt_maitpp213.
  ENDLOOP. " LOOP AT lt_stock ASSIGNING FIELD-SYMBOL(<fs_stock>)

  INSERT /vwk/maitpp213 FROM TABLE lt_maitpp213 ACCEPTING DUPLICATE KEYS.

ENDFORM.
FORM zf_fill_maitpp214_by_unif_rep USING pv_id TYPE /vwk/maitpp212-id. " ID da variante do relatório de release unificado

  DATA: lt_maitpp214 TYPE ty_t_maitpp214,
        wa_maitpp214 LIKE LINE OF lt_maitpp214,
        lv_field     TYPE string.

  LOOP AT gt_unif_rep ASSIGNING FIELD-SYMBOL(<fs_unif_rep>).

    CHECK <fs_unif_rep>-per_segmt+2(2) NA sy-abcde
      AND <fs_unif_rep>-per_segmt+5(4) NA sy-abcde.

    LOOP AT gt_werks ASSIGNING FIELD-SYMBOL(<fs_werks>).

      CLEAR wa_maitpp214.

      CONCATENATE <fs_unif_rep>-per_segmt+2(2) <fs_unif_rep>-per_segmt+5(4) INTO wa_maitpp214-spwoc.

      wa_maitpp214-id = pv_id.
      wa_maitpp214-matnr = p_matnr.

      wa_maitpp214-werks = <fs_werks>-werks.

      CONCATENATE 'DEM' <fs_werks>-field INTO lv_field SEPARATED BY '_'.

      ASSIGN COMPONENT lv_field OF STRUCTURE <fs_unif_rep> TO FIELD-SYMBOL(<fs_field>).
      CHECK sy-subrc IS INITIAL.

      wa_maitpp214-demand = <fs_field>.

      CONCATENATE 'TRAN' <fs_werks>-field INTO lv_field SEPARATED BY '_'.

      ASSIGN COMPONENT lv_field OF STRUCTURE <fs_unif_rep> TO <fs_field>.
      CHECK sy-subrc IS INITIAL.

      wa_maitpp214-transit = <fs_field>.

      CONCATENATE 'COVER' <fs_werks>-field INTO lv_field SEPARATED BY '_'.

      ASSIGN COMPONENT lv_field OF STRUCTURE <fs_unif_rep> TO <fs_field>.
      CHECK sy-subrc IS INITIAL.

      wa_maitpp214-cover = <fs_field>.

      APPEND wa_maitpp214 TO lt_maitpp214.

    ENDLOOP. " LOOP AT gt_werks ASSIGNING FIELD-SYMBOL(<fs_werks>)

  ENDLOOP. " LOOP AT gt_unif_rep ASSIGNING FIELD-SYMBOL(<fs_unif_rep>)

  INSERT /vwk/maitpp214 FROM TABLE lt_maitpp214 ACCEPTING DUPLICATE KEYS.

ENDFORM.
FORM zf_fill_maitpp215_by_mat_info USING pv_id TYPE /vwk/maitpp212-id. " ID da variante do relatório de release unificado

  DATA: lt_values    TYPE STANDARD TABLE OF /vwk/maispp084-value, " Valores dos campos de release unificado
        lt_maitpp215 TYPE ty_t_maitpp215,
        wa_maitpp215 LIKE LINE OF lt_maitpp215.

  READ TABLE gt_material_info ASSIGNING FIELD-SYMBOL(<fs_material_info>) WITH KEY field = TEXT-t17.
  IF sy-subrc IS INITIAL.

    SPLIT <fs_material_info>-value AT ',' INTO TABLE lt_values.

    LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<fs_values>).

      CLEAR wa_maitpp215.

      wa_maitpp215-id = pv_id.
      wa_maitpp215-field = 'MODEL'.
      wa_maitpp215-value = <fs_values>.
      APPEND wa_maitpp215 TO lt_maitpp215.

    ENDLOOP. " LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<fs_values>)
  ENDIF. " IF sy-subrc IS INITIAL

  READ TABLE gt_material_info ASSIGNING <fs_material_info> WITH KEY field = TEXT-t18.
  IF sy-subrc IS INITIAL.

    SPLIT <fs_material_info>-value AT ',' INTO TABLE lt_values.

    LOOP AT lt_values ASSIGNING <fs_values>.

      CLEAR wa_maitpp215.

      wa_maitpp215-id = pv_id.
      wa_maitpp215-field = 'LIFNR'.
      wa_maitpp215-value = <fs_values>.
      APPEND wa_maitpp215 TO lt_maitpp215.

    ENDLOOP. " LOOP AT lt_values ASSIGNING <fs_values>
  ENDIF. " IF sy-subrc IS INITIAL

  READ TABLE gt_material_info ASSIGNING <fs_material_info> WITH KEY field = TEXT-t19.
  IF sy-subrc IS INITIAL.

    SPLIT <fs_material_info>-value AT ',' INTO TABLE lt_values.

    LOOP AT lt_values ASSIGNING <fs_values>.

      CLEAR wa_maitpp215.

      wa_maitpp215-id = pv_id.
      wa_maitpp215-field = 'DISPO'.
      wa_maitpp215-value = <fs_values>.
      APPEND wa_maitpp215 TO lt_maitpp215.

    ENDLOOP. " LOOP AT lt_values ASSIGNING <fs_values>
  ENDIF. " IF sy-subrc IS INITIAL

  INSERT /vwk/maitpp215 FROM TABLE lt_maitpp215 ACCEPTING DUPLICATE KEYS.

ENDFORM.
FORM zf_update_maitpp211_by_stock USING pt_stock TYPE /vwk/maittpp022.

  DATA: lt_maitpp211 TYPE ty_t_maitpp211,
        wa_maitpp211 LIKE LINE OF lt_maitpp211,
        lt_stock     TYPE /vwk/maittpp022,
        lv_lines     TYPE i. " Lines of type Integers

  lt_stock = pt_stock.
  lv_lines = lines( lt_stock ).
  DELETE lt_stock INDEX lv_lines.

  LOOP AT lt_stock ASSIGNING FIELD-SYMBOL(<fs_stock>).

    CLEAR wa_maitpp211.

    wa_maitpp211-matnr = p_matnr.
    wa_maitpp211-werks = <fs_stock>-werks.
    wa_maitpp211-consumption = <fs_stock>-consumption.
    wa_maitpp211-min_cover = <fs_stock>-min_cover.
    wa_maitpp211-uname = sy-uname.
    wa_maitpp211-datum = sy-datum.
    wa_maitpp211-uzeit = sy-uzeit.

    APPEND wa_maitpp211 TO lt_maitpp211.

  ENDLOOP. " LOOP AT lt_stock ASSIGNING FIELD-SYMBOL(<fs_stock>)

  MODIFY /vwk/maitpp211 FROM TABLE lt_maitpp211.

ENDFORM.

INCLUDE /vwk/mairepp225o01. " Include /VWK/MAIREPP225O01

INCLUDE /vwk/mairepp225i01. " Include /VWK/MAIREPP225I01

INCLUDE /vwk/mairepp225c01. " Include /VWK/MAIREPP225C01
