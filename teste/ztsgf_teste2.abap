*&---------------------------------------------------------------------*
*& Report  ZTSGF_TESTE2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ztsgf_teste2.
*data: fcode type TABLE OF sy-ucomm.
*call screen 9000 STARTING AT 64 01 ENDING AT 160 22.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETER tn TYPE dd02l-tabname OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

DATA table_name TYPE string.
table_name = tn.

DATA line_type_native TYPE REF TO cl_abap_typedescr.
CALL METHOD cl_abap_typedescr=>describe_by_name
  EXPORTING
    p_name         = table_name
  RECEIVING
    p_descr_ref    = line_type_native
  EXCEPTIONS
    type_not_found = 1
    OTHERS         = 2.

CHECK sy-subrc IS INITIAL.

DATA line_type TYPE REF TO cl_abap_structdescr.
line_type ?= line_type_native.

DATA table_type TYPE REF TO cl_abap_tabledescr.
table_type = cl_abap_tabledescr=>create( p_line_type = line_type
                                         p_table_kind = cl_abap_tabledescr=>tablekind_std ).

DATA internal_table TYPE REF TO data.
CREATE DATA internal_table TYPE HANDLE table_type.

FIELD-SYMBOLS <internal_table> TYPE STANDARD TABLE.
ASSIGN internal_table->* TO <internal_table>.

SELECT * FROM (tn) INTO TABLE <internal_table>.

DATA o_alv TYPE REF TO cl_salv_table.

CALL METHOD cl_salv_table=>factory
  IMPORTING
    r_salv_table = o_alv
  CHANGING
    t_table      = <internal_table>.

o_alv->display( ).

*data t_matnr TYPE STANDARD TABLE OF matnr WITH HEADER LINE.
*
*append '1234567' to t_matnr.
*APPEND '1234566' to t_matnr.
*
*t_matnr = '4444444'.
*
*clear t_matnr.
*free t_matnr.
*clear t_matnr[].
*
**sort t_matnr ASCENDING.
*sort t_matnr descending.
*
*if t_matnr is not initial.
*
*endif.

TYPE-POOLS: abap.

DATA : datasistema  TYPE d,
       horasistema  TYPE t,
       datausuario  TYPE d,
       horausuario  TYPE t,
       datastamp    TYPE timestamp,
       datastampl   TYPE timestampl,
       horarioverao TYPE abap_bool,
       diasemana    TYPE d.

* Trabalhando com Data e Hora

datasistema = sy-datum.
horasistema = sy-uzeit.

datausuario = sy-datlo.
horausuario = sy-timlo.

diasemana = sy-fdayw.

PERFORM diaextenso.

WRITE : / 'Data de sistema:', datasistema DD/MM/YYYY,
        / 'Hora do sistema:', horasistema USING EDIT MASK '__:__',
        / ,
        / 'Data do usuario:', datausuario DD/MM/YYYY,
        / 'Hora do usuario:', horausuario USING EDIT MASK '__:__', /.

* Trabalhando com TimeStamp

GET TIME STAMP FIELD datastamp.
GET TIME STAMP FIELD datastampl.

WRITE: / 'Time Stamp      :', datastamp TIME ZONE sy-zonlo,
       / 'Time Stamp Longo:', datastampl TIME ZONE sy-zonlo, /.

* Convertendo TIMESTAMP

CONVERT TIME STAMP datastamp TIME ZONE sy-zonlo
INTO DATE datausuario TIME horausuario
DAYLIGHT SAVING TIME horarioverao.

WRITE: / 'Data de hoje: ', datausuario DD/MM/YYYY,
       /(60) horausuario USING EDIT MASK 'A hora atual é: __:__:__', /.

IF horarioverao EQ abap_true.
  WRITE : / 'Estamos em horario de verão'.
ELSE.
  WRITE : / 'Não estamos em horario de verão'.
ENDIF.

*&---------------------------------------------------------------------*
*&      Form  diaextenso
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM diaextenso.
  CASE sy-fdayw.
    WHEN  1.
      WRITE : / 'Segunda-feira'.
    WHEN  2.
      WRITE : / 'Terça-feira'.
    WHEN  3.
      WRITE : / 'Quarta-feira'.
    WHEN  4.
      WRITE : / 'Quinta-feira'.
    WHEN  5.
      WRITE : / 'Sexta-feira'.
    WHEN  6.
      WRITE : / 'Sabado'.
    WHEN  7.
      WRITE : / 'Domingo'.
  ENDCASE.
ENDFORM.                    "diaextenso

*
**&---------------------------------------------------------------------*
**&      Module  STATUS_9000  OUTPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE STATUS_9000 OUTPUT.
*  SET PF-STATUS 'XXXXXXXX' EXCLUDING fcode.
**  SET TITLEBAR 'xxx'.
*
*ENDMODULE.                 " STATUS_9000  OUTPUT
**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_9000  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE USER_COMMAND_9000 INPUT.
*
*  case sy-ucomm.
*    when 'BACK' or 'CANC' or 'EXIT'.
*      set SCREEN 0.
*      leave SCREEN.
*  ENDCASE.
*
*ENDMODULE.                 " USER_COMMAND_9000  INPUT
