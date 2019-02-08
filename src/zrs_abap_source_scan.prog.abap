*----------------------------------------------------------------------*
* Scan a set of source code for a particular string
*
* Modified version from
* https://blogs.sap.com/2018/09/19/code-search-in-modifications-and-enhancements/
*
* See https://github.com/sap-russia/ZRS_ABAP_SOURCE_SCAN for this project
*----------------------------------------------------------------------*
REPORT zrs_abap_source_scan MESSAGE-ID eu.

TYPE-POOLS:
  slis,
  sscr.

TABLES:
  seoclasstx,
  tadir,
  tlibt,
  d020s,
  trdir.

CLASS:
  lcl_source_scan DEFINITION DEFERRED.

DATA:
  lo_sscan   TYPE REF TO lcl_source_scan,
  lv_sstring TYPE text255,
  lv_appl    TYPE taplt-appl.

SELECTION-SCREEN  BEGIN OF BLOCK: a05 WITH FRAME TITLE a05.
SELECT-OPTIONS    sstring     FOR lv_sstring NO INTERVALS MODIF ID dsp.
PARAMETERS        p_regex     TYPE xfeld AS CHECKBOX MODIF ID dsp.
SELECTION-SCREEN: END OF BLOCK a05,
                  BEGIN OF BLOCK a10 WITH FRAME TITLE a10.
SELECT-OPTIONS:   repname  FOR trdir-name MEMORY ID rs_scan_repid,
                  dynnr    FOR d020s-dnum,
                  subc     FOR trdir-subc,
                  appl     FOR lv_appl,
                  cnam     FOR trdir-cnam MATCHCODE OBJECT user_addr,
                  unam     FOR trdir-unam MATCHCODE OBJECT user_addr.
SELECTION-SCREEN: END OF BLOCK a10,
                  BEGIN OF BLOCK a11 WITH FRAME TITLE a11.
SELECT-OPTIONS    devclass FOR tadir-devclass.
SELECTION-SCREEN: END OF BLOCK a11,
                  BEGIN OF BLOCK a12 WITH FRAME TITLE a12.
SELECT-OPTIONS:   funcgrp  FOR tlibt-area.
SELECTION-SCREEN: END OF BLOCK a12,
                  BEGIN OF BLOCK a13 WITH FRAME TITLE a13.
SELECT-OPTIONS:   p_class  FOR seoclasstx-clsname.
SELECTION-SCREEN: END OF BLOCK a13,
                  BEGIN OF BLOCK a20 WITH FRAME TITLE a20.
PARAMETERS:       plusminu(2) TYPE n DEFAULT 2,
                  inclu       TYPE xfeld AS CHECKBOX DEFAULT 'X',
                  modiass     TYPE xfeld AS CHECKBOX USER-COMMAND dummy,
                  comment     TYPE xfeld AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK a20,
                  BEGIN OF BLOCK a30 WITH FRAME TITLE a30.
PARAMETERS:       rb_code RADIOBUTTON GROUP r10,
                  rb_dyn  RADIOBUTTON GROUP r10,
                  rb_all  RADIOBUTTON GROUP r10,
                  p_vers  TYPE xfeld AS CHECKBOX,
                  p_zz_mod TYPE xfeld AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK a30.

*----------------------------------------------------------------------*
*       CLASS lcx_scan_exceptions DEFINITION
*----------------------------------------------------------------------*
*       Exceptions for source scanning
*----------------------------------------------------------------------*
CLASS lcx_scan_exceptions DEFINITION INHERITING FROM cx_static_check FINAL.
ENDCLASS.                    "lcx_scan_exceptions DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_source_scan DEFINITION
*----------------------------------------------------------------------*
*       ABAP source scanner
*----------------------------------------------------------------------*
CLASS lcl_source_scan DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor,

      f4_class
        CHANGING
          cv_class_name TYPE clike,

      f4_function_group
        IMPORTING
          iv_group_name TYPE clike,

      f4_repname
        CHANGING
          cv_repname TYPE clike,

      pbo,

      start.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_dynpro,
        repname LIKE d020s-prog,
        dynnr   LIKE d020s-dnum,
      END OF ty_dynpro.

    TYPES:
      BEGIN OF ty_ls_objname,
        report TYPE sy-repid,
        dynnr  TYPE sy-dynnr,
      END OF ty_ls_objname.

    DATA:
      go_alv        TYPE REF TO cl_salv_hierseq_table,
      gv_hit_count  TYPE i,
      gv_sstring    TYPE string,
      gv_dynp_found TYPE xfeld,
      gv_vers_found TYPE xfeld,
      gt_dynpro     TYPE STANDARD TABLE OF ty_dynpro,
      gt_object     TYPE STANDARD TABLE OF tadir-obj_name,
      gt_vrsd       TYPE HASHED TABLE OF vrsd
                      WITH UNIQUE KEY objname versno,
      gt_source     TYPE abaptxt255_tab,
      gv_report     TYPE syrepid,
      gv_dynpro     TYPE sydynnr,

      BEGIN OF gs_alv_header,
        repname TYPE tadir-obj_name,
        dynnr   TYPE sy-dynnr,
        expand  TYPE xfeld,
        versno  TYPE vrsd-versno,
      END OF gs_alv_header,

      gt_alv_header LIKE STANDARD TABLE OF gs_alv_header,

      BEGIN OF gs_alv_item,
        repname    TYPE sy-repid,
        dynnr      TYPE sy-dynnr,
        versno     TYPE vrsd-versno,
        line_no    TYPE rsrow,
        text       TYPE text255,
        hit        TYPE xfeld,
        cell_color TYPE lvc_t_scol,
      END OF gs_alv_item,

      gt_alv_item LIKE STANDARD TABLE OF gs_alv_item.

    CONSTANTS:
      gc_x TYPE xfeld VALUE 'X'.

    METHODS:
      add_to_hitlist
        IMPORTING
          iv_report      TYPE clike
          iv_dynpro      TYPE clike OPTIONAL
          iv_source_line TYPE clike
          iv_tabix       TYPE sy-tabix
          iv_hit         TYPE xfeld
          iv_versno      TYPE vrsd-versno,

      call_abap_editor
        IMPORTING
          is_alv_item LIKE gs_alv_item,

      call_screen_painter
        IMPORTING
          is_alv_item LIKE gs_alv_item,

      display,

      display_abap_version
        IMPORTING
          is_alv_item LIKE gs_alv_item,

      display_screen_painter_version
        IMPORTING
          is_alv_item LIKE gs_alv_item,

      display_version_management
        IMPORTING
          is_alv_header LIKE gs_alv_header,

      get_alv_instance,
      get_dynpro_flow_logic
        IMPORTING
                  iv_report       TYPE clike
                  iv_dynpro       TYPE clike
        RETURNING VALUE(rt_dflow) TYPE abaptxt255_tab,

      get_hit_set
        IMPORTING
          iv_report TYPE clike
          iv_dynpro TYPE clike OPTIONAL
          it_abap   TYPE abaptxt255_tab
          iv_tabix  TYPE sy-tabix
          iv_versno TYPE vrsd-versno,

      get_version_numbers
        IMPORTING
                  iv_report      TYPE clike
                  iv_dynpro      TYPE clike OPTIONAL
        RETURNING VALUE(rt_vrsd) LIKE gt_vrsd,

      get_dynpros,
      get_source_names,

      get_source_by_version
        IMPORTING
                  iv_report      TYPE clike
                  iv_dynpro      TYPE clike OPTIONAL
                  iv_versno      TYPE vrsd-versno
        RETURNING VALUE(rt_abap) TYPE abaptxt255_tab,

      get_report_names,
      get_function_names,
      get_class_names,
      get_interface_names,
      get_includes,

      search_abap_source   RAISING lcx_scan_exceptions,
      search_dynpro_source RAISING lcx_scan_exceptions,

      search_source        RAISING lcx_scan_exceptions,

      set_alv_attributes,

      on_link_click
            FOR EVENT link_click OF cl_salv_events_hierseq
        IMPORTING
            sender
            level
            row
            column.

ENDCLASS.                    "lcl_source_scan DEFINITION

"$. Region added
CLASS lcl_zz DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF tp_dynpro, " needs to be identical to ty_dynpro in lcl_source_scan
             repname LIKE d020s-prog,
             dynnr   LIKE d020s-dnum,
           END OF tp_dynpro.
    TYPES: ttp_dynpro TYPE STANDARD TABLE OF tp_dynpro WITH DEFAULT KEY.
    CLASS-METHODS:
      add_modification_includes
        CHANGING c_includes TYPE prognames
                 c_screens  TYPE ttp_dynpro
    , add_enhancement_includes
        CHANGING c_includes TYPE prognames
    , filter_modifications
        IMPORTING i_include     TYPE progname
                  i_screen      TYPE sy-dynnr
                  i_source_tab   TYPE abaptxt255_tab
        CHANGING  c_findings TYPE match_result_tab
    .
  PRIVATE SECTION.
    TYPES: BEGIN OF tp_mod_include
         , include TYPE reposrc-progname
         , without_assistant TYPE abap_bool
         , END OF tp_mod_include
         , ttp_mod_includes TYPE STANDARD TABLE OF tp_mod_include WITH KEY include

         , BEGIN OF tp_mod_screen
         ,   program TYPE d020s-prog
         ,   screen  TYPE d020s-dnum
         ,   without_assistant TYPE abap_bool
         , END OF tp_mod_screen
         , ttp_mod_screens TYPE STANDARD TABLE OF tp_mod_screen WITH KEY program
         .
    CLASS-METHODS:
      get_method_include
        IMPORTING i_class        TYPE seoclsname
                  i_method       TYPE seocpdname
        RETURNING VALUE(r_include) TYPE progname
    , get_function_include
        IMPORTING i_function     TYPE tfdir-funcname
        RETURNING VALUE(r_include) TYPE progname
    , get_all_includes_and_screens
        IMPORTING i_type TYPE smodilog-obj_type
                  i_name TYPE smodilog-obj_name
        EXPORTING e_includes TYPE prognames
                  e_screens TYPE ttp_mod_screens
    , get_screen_flow_logic
        IMPORTING i_screen TYPE tp_mod_screen
        RETURNING VALUE(r_source_tab) TYPE d022s_t
    .
    CLASS-DATA:
      s_mod_includes TYPE ttp_mod_includes
    , s_mod_screens  TYPE ttp_mod_screens
    .
ENDCLASS.

CLASS lcl_zz IMPLEMENTATION.
  METHOD add_enhancement_includes.
    DATA devclass_range TYPE RANGE OF tadir-devclass.
    IF devclass IS INITIAL. " if no devclass restriction on selection screen, assume the user only wants the customer (Z and Y) enhancements
      devclass_range = VALUE #( sign = 'I' option = 'CP' ( low = 'Z*' ) ( low = 'Y*' ) ).
    ELSE.
      devclass_range = devclass[].
    ENDIF.
    SELECT obj_name
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'ENHO'
      AND   devclass IN @devclass_range
      AND   delflag = ''
      INTO TABLE @data(enhancements).
    LOOP AT enhancements INTO DATA(enh).
      TRANSLATE enh(30) USING ' ='.
      enh+30 = 'E'.
      APPEND enh TO c_includes.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_modification_includes.
    DATA include TYPE progname.
    CLEAR: s_mod_includes, s_mod_screens.
    IF sy-batch IS INITIAL.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = 'GET MODIFICATION INCLUDES...'.
    ENDIF.
    SELECT DISTINCT obj_type, obj_name, sub_type, sub_name, operation, prot_only AS without_assistant
      FROM smodilog
      WHERE NOT operation IN ('MIGR', 'IMPL', 'TRSL', 'NOTE')  "in sync with SE95 and Clone Finder
          AND inactive = ''
          AND int_type NOT IN ('DUMY') "clone finder also ignores 'XXXX'=without modification assistant, but we think this is wrong
          AND obj_type IN ('PROG', 'LDBA',          " LDBA = logical database, similar to PROG
                           'FUGR', 'FUGX', 'FUGS',  " FUGX and FUGS = parts of function groups for user exits
                           'CLAS')
          AND sub_type NOT IN ( 'REPT', 'FUGT', 'CUAD', 'DOCU', 'VARI' )
      ORDER BY obj_type, obj_name, sub_type, sub_name, prot_only
      INTO TABLE @DATA(modifications).

    LOOP AT modifications REFERENCE INTO DATA(mod).
      CASE mod->sub_type.
        WHEN 'REPS'. " report source
          s_mod_includes = VALUE #( BASE s_mod_includes
                                   ( include           = mod->sub_name
                                     without_assistant = mod->without_assistant ) ).
        WHEN 'METH'.
          s_mod_includes = VALUE #( BASE s_mod_includes
                                   ( include           = get_method_include( i_class  = EXACT #( mod->obj_name )
                                                                             i_method = EXACT #( mod->sub_name+30 ) )
                                     without_assistant = mod->without_assistant ) ).
        WHEN 'FUNC'.
          s_mod_includes = VALUE #( BASE s_mod_includes
                                   ( include           = get_function_include( i_function = EXACT #( mod->sub_name ) )
                                     without_assistant = mod->without_assistant ) ).
        WHEN 'LDBA'. " logical databases. Seem to have these two code objects
          s_mod_includes = VALUE #( BASE s_mod_includes
                                    ( include           = mod->sub_name && 'SEL'
                                      without_assistant = mod->without_assistant )
                                    ( include           = 'SAP' && mod->sub_name
                                      without_assistant = mod->without_assistant ) ).
        WHEN 'CLAS'. " complete class
          cl_oo_classname_service=>get_all_class_includes( EXPORTING class_name = EXACT #( mod->obj_name )
                                                           RECEIVING result     = DATA(class_includes)
                                                           EXCEPTIONS OTHERS    = 0 ).
          DELETE class_includes WHERE table_line+30(2) = 'CS' OR table_line+30(2) = 'CP'.  " CS = complete source, CP = frameprogram for class pool
          s_mod_includes = VALUE #( BASE s_mod_includes
                                    FOR class_incl IN class_includes
                                    ( include           = class_incl
                                      without_assistant = mod->without_assistant ) ).
        WHEN 'CINC'. " class include
          s_mod_includes = VALUE #( BASE s_mod_includes
                                   ( include           = EXACT #( mod->sub_name )
                                     without_assistant = mod->without_assistant ) ).
        WHEN 'CPUB' OR 'CPRO' OR 'CPRI'. " class public/protected/private definitions
          include = mod->sub_name.
          TRANSLATE include USING ' ='.
          include+30 =  SWITCH #( mod->sub_type WHEN 'CPUB' THEN 'CU'
                                                WHEN 'CPRO' THEN 'CO'
                                                WHEN 'CPRI' THEN 'CI' ).
          s_mod_includes = VALUE #( BASE s_mod_includes
                                   ( include           = include
                                     without_assistant = mod->without_assistant ) ).
        WHEN 'FUGR' OR 'PROG'. " complete report or function group
          get_all_includes_and_screens( EXPORTING i_type = mod->sub_type
                                                  i_name = EXACT #( mod->sub_name )
                                        IMPORTING e_includes = DATA(includes)
                                                  e_screens  = DATA(screens) ).
          s_mod_includes = VALUE #( BASE s_mod_includes
                                    FOR incl IN includes
                                    ( include           = incl
                                      without_assistant = mod->without_assistant ) ).
          s_mod_screens = VALUE #( BASE s_mod_screens
                                   FOR scr IN screens
                                   ( program           = scr-program
                                     screen            = scr-screen
                                     without_assistant = mod->without_assistant ) ).
        WHEN 'DYNP'. " screen (dynpro) - the program only searches through the flow logic (code)
          s_mod_screens = VALUE #( BASE s_mod_screens
                                   ( program           = mod->sub_name(40)
                                     screen            = mod->sub_name+40(4)
                                     without_assistant = mod->without_assistant ) ).
        WHEN OTHERS.
*          LOG-POINT ID zlog FIELDS mod->obj_type mod->obj_name mod->sub_type mod->sub_name. " unknown subtype of modification
      ENDCASE.
    ENDLOOP.

    " pass result to caller, but also keep our own data, to be used later for filtering
    SORT s_mod_includes BY include.                 " for binary search later
    DELETE ADJACENT DUPLICATES FROM s_mod_includes.
    SORT s_mod_screens BY program screen.           " for binary search later
    DELETE ADJACENT DUPLICATES FROM s_mod_screens COMPARING program screen.  " there are duplicates here because of the way SMODILOG is used
    c_includes = VALUE #( BASE c_includes
                             FOR incl_ IN s_mod_includes
                             ( incl_-include ) ).
    c_screens = VALUE #( BASE c_screens
                             FOR scr_ IN s_mod_screens
                             ( repname = scr_-program
                               dynnr   = scr_-screen ) ).
  ENDMETHOD.

  METHOD get_method_include.
    DATA: generic_instance TYPE REF TO if_oo_clif_incl_naming
          , class_instance TYPE REF TO if_oo_class_incl_naming.
    cl_oo_include_naming=>get_instance_by_name( EXPORTING  name           = i_class
                                                RECEIVING  cifref         = generic_instance
                                                EXCEPTIONS no_objecttype  = 1
                                                           internal_error = 2
                                                           OTHERS         = 3 ).
    class_instance ?= generic_instance.
    class_instance->get_include_by_mtdname( EXPORTING  mtdname                      = i_method
                                                       with_enhancements            = abap_true
                                                       with_alias_resolution        = abap_true
                                            RECEIVING  progname                     = r_include
                                            EXCEPTIONS internal_method_not_existing = 1
                                                       OTHERS                       = 2 ).
    ASSERT sy-subrc = 0. " method not found
  ENDMETHOD.

  METHOD get_function_include.
    SELECT SINGLE pname
      FROM tfdir
      WHERE funcname = @i_function
      INTO @r_include.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD get_all_includes_and_screens.
    IF i_type = 'PROG'.
      DATA(frame_program) = i_name.
    ELSE.
      DATA(function_group) = EXACT rs38l_area( i_name ).
      CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
        CHANGING
          program       = frame_program
          complete_area = function_group
        EXCEPTIONS
          OTHERS        = 1.
      ASSERT sy-subrc = 0.
    ENDIF.

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program    = frame_program
      TABLES
        includetab = e_includes
      EXCEPTIONS
        OTHERS     = 0.
    DELETE e_includes WHERE table_line CP 'LSVIM*'.  "remove reused includes from maintenance views
    SORT e_includes.
    DELETE ADJACENT DUPLICATES FROM e_includes.

    SELECT prog AS program, dnum AS screen
      FROM d020s
      WHERE prog = @frame_program
      INTO CORRESPONDING FIELDS OF TABLE @e_screens.
  ENDMETHOD.

  METHOD get_screen_flow_logic.
    DATA: header TYPE d020s
        , fields TYPE STANDARD TABLE OF d021s
        , parameters TYPE STANDARD TABLE OF d023s
        , BEGIN OF screen
        ,   prog TYPE d020s-prog
        ,   dnum TYPE d020s-dnum
        , END OF screen
        .
    screen = VALUE #( prog = i_screen-program
                      dnum = i_screen-screen ).
    IMPORT DYNPRO header fields r_source_tab parameters ID screen.
  ENDMETHOD.

  METHOD filter_modifications.
    DATA in_modified_section TYPE abap_bool.

    IF i_screen IS INITIAL.
      READ TABLE s_mod_includes WITH KEY include = i_include REFERENCE INTO DATA(mod_incl) BINARY SEARCH.
      CHECK sy-subrc = 0.                             " do not filter findings of unmodified code
      CHECK mod_incl->without_assistant = abap_false. " do not filter findings of code modified without assistant
    ELSE.
      READ TABLE s_mod_screens WITH KEY program = i_include screen = i_screen REFERENCE INTO DATA(mod_scr) BINARY SEARCH.
      CHECK sy-subrc = 0.                             " do not filter findings of unmodified code
      CHECK mod_scr->without_assistant = abap_false.  " do not filter findings of code modified without assistant
    ENDIF.

    DATA(next_finding_index) = 1.
    READ TABLE c_findings INDEX 1 REFERENCE INTO DATA(next_finding).
    LOOP AT i_source_tab REFERENCE INTO DATA(source).
      DATA(source_index) = sy-tabix.
      IF source->*(2) = '*{'.
        IF in_modified_section = abap_true.
*          LOG-POINT ID zlog FIELDS i_include i_screen. " modified include with irregular *{ *} pattern - can happen when modified code is copied into a modification
          RETURN.                                      " the best way can do is to leave the remaining source code findings unfiltered
        ELSE.
          in_modified_section = abap_true.
        ENDIF.
      ELSEIF source->*(2) = '*}'.
        IF in_modified_section = abap_false.
*          LOG-POINT ID zlog FIELDS i_include i_screen. " modified include with irregular *{ *} pattern - can happen when modified code is copied into a modification
          RETURN.                                      " the best way can do is to leave the remaining source code findings unfiltered
        ELSE.
          in_modified_section = abap_false.
        ENDIF.
      ENDIF.
      DO.  " loop necessary, because there can be multiple findings for same source line
        IF source_index <> next_finding->line.
          EXIT.
        ENDIF.
        IF in_modified_section = abap_true.
          ADD 1 TO next_finding_index.                   " leave current, continue with next
        ELSE.
          DELETE c_findings INDEX next_finding_index. " delete current, continue with next
        ENDIF.
        READ TABLE c_findings INDEX next_finding_index REFERENCE INTO next_finding.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
"$. Endregion added

*----------------------------------------------------------------------*
*       CLASS lcl_source_scan IMPLEMENTATION
*----------------------------------------------------------------------*
*       ABAP source scanner
*----------------------------------------------------------------------*
CLASS lcl_source_scan IMPLEMENTATION.
  METHOD display_screen_painter_version.
    DATA:
      lv_object_name TYPE versobjnam,
      ls_infolna     TYPE vrsinfolna,
      ls_infolnb     TYPE vrsinfolnb,
      ls_vrsd        LIKE LINE OF gt_vrsd,
      ls_object_name TYPE ty_ls_objname.

    ls_object_name-report = is_alv_item-repname.
    ls_object_name-dynnr  = is_alv_item-dynnr.
    lv_object_name        = ls_object_name.

    READ TABLE gt_vrsd WITH TABLE KEY objname = lv_object_name
                                      versno  = is_alv_item-versno
                                      INTO ls_vrsd.

    CHECK sy-subrc IS INITIAL.

    ls_infolna = lv_object_name.
    MOVE-CORRESPONDING ls_vrsd TO ls_infolnb.

    CALL FUNCTION 'RS_SCRP_SHOW_VERS'
      EXPORTING
        infolna = ls_infolna
        infolnb = ls_infolnb
        objname = lv_object_name
        versno  = is_alv_item-versno
      EXCEPTIONS
        OTHERS  = 0.

  ENDMETHOD.                    "display_screen_painter_version

  METHOD display_abap_version.
    DATA:
      lt_trdir       TYPE STANDARD TABLE OF trdir,
      lv_object_name TYPE versobjnam,
      lv_title       TYPE sy-title,
      lt_abap        TYPE abaptxt255_tab.

    lv_object_name = is_alv_item-repname.

*   Display report version
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name                  = lv_object_name
        object_type                  = 'REPS'
        versno                       = is_alv_item-versno
        iv_no_release_transformation = gc_x
      TABLES
        repos_tab                    = lt_abap
        trdir_tab                    = lt_trdir
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.

    CHECK sy-subrc IS INITIAL.

    CONCATENATE 'Programm:'(004)
                is_alv_item-repname
                'Version'(005)
                 is_alv_item-versno
                 INTO lv_title SEPARATED BY space.

    EDITOR-CALL FOR lt_abap TITLE lv_title DISPLAY-MODE.

  ENDMETHOD.                    "display_abap_version

  METHOD call_screen_painter.
    CALL FUNCTION 'RS_SCRP'
      EXPORTING
        abl_line    = is_alv_item-line_no
        dynnr       = is_alv_item-dynnr
        progname    = is_alv_item-repname
        wanted_mode = 'SHOW'
      EXCEPTIONS
        OTHERS      = 0.

  ENDMETHOD.                    "call_screen_painter

  METHOD call_abap_editor.
    CALL FUNCTION 'EDITOR_PROGRAM'
      EXPORTING
        appid   = 'PG'
        display = gc_x
        program = is_alv_item-repname
        line    = is_alv_item-line_no
        topline = is_alv_item-line_no
      EXCEPTIONS
        OTHERS  = 0.

  ENDMETHOD.                    "call_abap_editor

  METHOD display_version_management.
    IF is_alv_header-dynnr IS INITIAL.
*     call version management for programs
      CALL FUNCTION 'RS_PROGRAM_VERSIONS'
        EXPORTING
          progname = is_alv_header-repname
        EXCEPTIONS
          OTHERS   = 0.
    ELSE.
      CALL FUNCTION 'RS_SCRP_VERSION'
        EXPORTING
          dynnr     = is_alv_header-dynnr
          progname  = is_alv_header-repname
          no_update = gc_x.
    ENDIF.
  ENDMETHOD.                    "display_version_management

  METHOD constructor.
    DATA:
      ls_restrict    TYPE sscr_restrict,
      ls_opt_list    TYPE sscr_opt_list,
      ls_association TYPE sscr_ass.

    ls_opt_list-name       = 'RESTRICT'.
    ls_opt_list-options-cp = gc_x.
    ls_opt_list-options-eq = gc_x.

    APPEND ls_opt_list TO ls_restrict-opt_list_tab.

    ls_association-kind    = 'S'.
    ls_association-name    = 'SSTRING'.
    ls_association-sg_main = 'I'.
    ls_association-op_main = ls_association-op_addy = 'RESTRICT'.

    APPEND ls_association TO ls_restrict-ass_tab.

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        program     = sy-repid
        restriction = ls_restrict
      EXCEPTIONS
        OTHERS      = 0.

  ENDMETHOD.                    "constructor

  METHOD get_dynpro_flow_logic.
    DATA: ls_dhead  TYPE d020s,
          lt_dfield TYPE STANDARD TABLE OF d021s,
          lt_dflow  TYPE STANDARD TABLE OF d022s,
          lt_dmatch TYPE STANDARD TABLE OF d023s,

          BEGIN OF ls_dynp_id,
            prog TYPE d020s-prog,
            dnum TYPE d020s-dnum,
          END OF ls_dynp_id.

    ls_dynp_id-prog = iv_report.
    ls_dynp_id-dnum = iv_dynpro.

    IMPORT DYNPRO ls_dhead lt_dfield lt_dflow lt_dmatch ID ls_dynp_id.

    rt_dflow = lt_dflow.
  ENDMETHOD.                    "get_dynpro_flow_logic

  METHOD on_link_click.
    DATA:
      ls_alv_header LIKE LINE OF gt_alv_header,
      ls_alv_item   LIKE LINE OF gt_alv_item.

    CASE level.
      WHEN '1'.
        READ TABLE gt_alv_header INDEX row INTO ls_alv_header.
        CHECK sy-subrc IS INITIAL.

        display_version_management( ls_alv_header ).

      WHEN '2'.
        READ TABLE gt_alv_item INDEX row INTO ls_alv_item.
        CHECK sy-subrc IS INITIAL.

        IF ls_alv_item-dynnr IS INITIAL.
          IF ls_alv_item-versno IS INITIAL.
            call_abap_editor( ls_alv_item ).
          ELSE.
            display_abap_version( ls_alv_item ).
          ENDIF.

          SET PARAMETER ID 'RID' FIELD sy-repid.
        ELSE.
*         Call screen painter
          IF ls_alv_item-versno IS INITIAL.
            call_screen_painter( ls_alv_item ).
          ELSE.
            display_screen_painter_version( ls_alv_item ).
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "on_link_click

  METHOD set_alv_attributes.
    DATA:
      lo_layout    TYPE REF TO cl_salv_layout,
      lo_events    TYPE REF TO cl_salv_events_hierseq,
      lo_functions TYPE REF TO cl_salv_functions_list,
      lo_level     TYPE REF TO cl_salv_hierseq_level,
      lo_column    TYPE REF TO cl_salv_column_hierseq,
      lo_columns   TYPE REF TO cl_salv_columns_hierseq,
      lt_columns   TYPE salv_t_column_ref,
      ls_columns   LIKE LINE OF lt_columns,
      lo_settings  TYPE REF TO cl_salv_display_settings,
      lv_title     TYPE lvc_title,
      lv_hits      TYPE lvc_title,
      ls_color     TYPE lvc_s_colo,
      ls_layout    TYPE salv_s_layout_key,
      lt_functions TYPE salv_t_ui_func.

*   Layout
    ls_layout-report = sy-repid.
    ls_layout-handle = 'SCAN'.

    lo_layout = go_alv->get_layout( ).
    lo_layout->set_key( ls_layout ).
    lo_layout->set_save_restriction( ).

*   Function keys/buttons
    lo_functions = go_alv->get_functions( ).
    lo_functions->set_all( gc_x ).

*   exclude the following functions (column paging buttons)
    lt_functions = lo_functions->get_functions( ).

*   Display settings
    lo_settings = go_alv->get_display_settings( ).

*   Title
    lv_hits = gv_hit_count.
    SHIFT lv_hits LEFT DELETING LEADING space.

    CONCATENATE lv_hits
                'Treffer'(001)
                INTO lv_hits SEPARATED BY space.

    lv_title = 'Source Scan für String:'(002).

    CONCATENATE lv_title
                gv_sstring
                INTO lv_title SEPARATED BY space.

    CONCATENATE lv_title
                lv_hits
                INTO lv_title SEPARATED BY ' - '.

    lo_settings->set_list_header( lv_title ).

*   Event handling
    lo_events = go_alv->get_event( ).
    SET HANDLER on_link_click FOR lo_events.

*   Field catalog
    TRY.
*       Field catalog/columns - header table
        lo_columns  = go_alv->get_columns( '1' ).
        lt_columns = lo_columns->get( ).

        TRY.
            lo_columns->set_expand_column( 'EXPAND' ).

            lo_level = go_alv->get_level( '1' ).
            lo_level->set_items_expanded( gc_x ).

          CATCH cx_salv_data_error.
        ENDTRY.

        LOOP AT lt_columns INTO ls_columns.
          CASE ls_columns-columnname.
            WHEN 'EXPAND'.
              ls_columns-r_column->set_technical( ).

            WHEN 'DYNNR'.
              IF gv_dynp_found IS INITIAL.
                ls_columns-r_column->set_technical( ).
              ELSE.
                ls_columns-r_column->set_output_length( '15' ).
              ENDIF.

            WHEN 'VERSNO'.
              IF gv_vers_found IS INITIAL.
                ls_columns-r_column->set_technical( ).
              ELSE.
                ls_columns-r_column->set_leading_zero( gc_x ).
                ls_columns-r_column->set_output_length( '15' ).
                TRY.
                    lo_column ?= ls_columns-r_column.
                    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
                  CATCH cx_sy_move_cast_error.
                ENDTRY.
              ENDIF.
          ENDCASE.
        ENDLOOP.

*       Field catalog/columns - item table
        lo_columns = go_alv->get_columns( '2' ).

        TRY.
            lo_columns->set_color_column( 'CELL_COLOR' ).
          CATCH cx_salv_data_error.
        ENDTRY.

        lt_columns = lo_columns->get( ).

        LOOP AT lt_columns INTO ls_columns.
          CASE ls_columns-columnname.
            WHEN 'REPNAME'.
              ls_columns-r_column->set_technical( ).

            WHEN 'DYNNR'.
              ls_columns-r_column->set_technical( ).

            WHEN 'VERSNO'.
              ls_columns-r_column->set_technical( ).

            WHEN 'CELL_COLOR'.
              ls_columns-r_column->set_technical( ).

            WHEN 'HIT'.
              ls_columns-r_column->set_technical( ).

            WHEN 'LINE_NO'.
              ls_color-col = '4'.
              TRY.
                  lo_column ?= ls_columns-r_column.
                  lo_column->set_color( ls_color ).
                  lo_column->set_leading_zero( gc_x ).
                CATCH cx_sy_move_cast_error.
              ENDTRY.

            WHEN 'TEXT'.
              TRY.
                  lo_column ?= ls_columns-r_column.
                  lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
                CATCH cx_sy_move_cast_error.
              ENDTRY.

          ENDCASE.
        ENDLOOP.
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.                    "set_alv_attributes

  METHOD get_alv_instance.
    DATA:
      lt_alv_bind TYPE salv_t_hierseq_binding,
      ls_alv_bind LIKE LINE OF lt_alv_bind.

    ls_alv_bind-master = ls_alv_bind-slave = 'REPNAME'.
    APPEND ls_alv_bind TO lt_alv_bind.

    ls_alv_bind-master = ls_alv_bind-slave = 'DYNNR'.
    APPEND ls_alv_bind TO lt_alv_bind.

    ls_alv_bind-master = ls_alv_bind-slave = 'VERSNO'.
    APPEND ls_alv_bind TO lt_alv_bind.

    TRY.
        CALL METHOD cl_salv_hierseq_table=>factory
          EXPORTING
            t_binding_level1_level2 = lt_alv_bind
          IMPORTING
            r_hierseq               = go_alv
          CHANGING
            t_table_level1          = gt_alv_header
            t_table_level2          = gt_alv_item.

      CATCH cx_salv_data_error.
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.                    "get_alv_instance

  METHOD f4_repname.
    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
      EXPORTING
        object_type          = 'PROG'
        object_name          = cv_repname
        suppress_selection   = 'X'
      IMPORTING
        object_name_selected = cv_repname
      EXCEPTIONS
        cancel               = 0.
  ENDMETHOD.                                                "f4_repname

  METHOD f4_function_group.
    DATA:
      lv_fname TYPE dynfnam.

    lv_fname = iv_group_name.

    CALL FUNCTION 'RS_HELP_HANDLING'
      EXPORTING
        dynpfield                 = lv_fname
        dynpname                  = sy-dynnr
        object                    = 'FG  '
        progname                  = sy-repid
        suppress_selection_screen = 'X'.

  ENDMETHOD.                    "f4_function_group

  METHOD f4_class.
    CALL FUNCTION 'F4_DD_ALLTYPES'
      EXPORTING
        object               = cv_class_name
        suppress_selection   = gc_x
        display_only         = space
        only_types_for_clifs = gc_x
      IMPORTING
        result               = cv_class_name.
  ENDMETHOD.                                                "f4_class

  METHOD display.

    DATA text TYPE c LENGTH 150.

    IF gv_hit_count IS INITIAL.
      MESSAGE s326 WITH gv_sstring.
      RETURN.
    ENDIF.

    IF sy-batch IS INITIAL.
      text = |DISPLAY { gv_hit_count } HITS...|.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = text.
    ENDIF.

    SORT gt_alv_item BY repname dynnr versno line_no hit DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_alv_item COMPARING repname dynnr versno line_no.

    get_alv_instance( ).
    CHECK go_alv IS NOT INITIAL.

    set_alv_attributes( ).

    go_alv->display( ).

  ENDMETHOD.                    "display

  METHOD add_to_hitlist.
    DATA:
      ls_col LIKE LINE OF gs_alv_item-cell_color.

    gs_alv_item-repname = iv_report.
    gs_alv_item-dynnr   = iv_dynpro.
    gs_alv_item-line_no = iv_tabix.
    gs_alv_item-versno  = iv_versno.
    gs_alv_item-text    = iv_source_line.

    IF iv_hit IS NOT INITIAL.
      gs_alv_item-hit = gc_x.
      ADD 1 TO gv_hit_count.
      ls_col-fname     = 'TEXT'.
      ls_col-color-col = '5'.
      APPEND ls_col TO gs_alv_item-cell_color.
    ENDIF.

    APPEND gs_alv_item TO gt_alv_item.

    CLEAR gs_alv_item.
  ENDMETHOD.                    "add_to_hitlist

  METHOD get_hit_set.
    DATA: lv_end     TYPE i,
          lv_start   TYPE i,
          lv_xtabix  TYPE sy-tabix,
          lv_hitline TYPE xfeld.

    FIELD-SYMBOLS:
      <lv_abap> TYPE any.

    lv_start = iv_tabix - plusminu .
    lv_end   = iv_tabix + plusminu.

    IF lv_start < 1.
      lv_start = 1.
    ENDIF.

    WHILE lv_start <= lv_end.
      READ TABLE it_abap ASSIGNING <lv_abap> INDEX lv_start.
      IF sy-subrc IS NOT INITIAL.
        EXIT.
      ENDIF.

      lv_xtabix = sy-tabix.

      IF lv_start = iv_tabix.
        lv_hitline = gc_x.
      ELSE.
        CLEAR lv_hitline.
      ENDIF.

      ADD 1 TO lv_start.

      IF comment IS NOT INITIAL.
        IF modiass IS INITIAL.
          IF <lv_abap>(1) = '*'
          OR <lv_abap>(1) = '"'.
            CONTINUE.
          ENDIF.
        ELSE.
          IF <lv_abap>(1) = '*'.
            IF  <lv_abap>(2) = '*{' OR <lv_abap>(2) = '*}'.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL METHOD add_to_hitlist
        EXPORTING
          iv_report      = iv_report
          iv_dynpro      = iv_dynpro
          iv_source_line = <lv_abap>
          iv_tabix       = lv_xtabix
          iv_hit         = lv_hitline
          iv_versno      = iv_versno.

    ENDWHILE.

  ENDMETHOD.                    "get_hit_set

  METHOD get_source_by_version.
    DATA:
      lv_object_name TYPE versobjnam,
      ls_object_name TYPE ty_ls_objname,
      lt_trdir       TYPE STANDARD TABLE OF trdir,
      lt_d022s       TYPE STANDARD TABLE OF d022s.

    IF iv_dynpro IS INITIAL.
      lv_object_name = iv_report.

      CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
        EXPORTING
          object_name                  = lv_object_name
          object_type                  = 'REPS'
          versno                       = iv_versno
          iv_no_release_transformation = 'X'
        TABLES
          repos_tab                    = rt_abap
          trdir_tab                    = lt_trdir
        EXCEPTIONS
          OTHERS                       = 0.
    ELSE.
      ls_object_name-report = iv_report.
      ls_object_name-dynnr  = iv_dynpro.

      lv_object_name = ls_object_name.

      CALL FUNCTION 'SVRS_GET_VERSION_DYNP_40'
        EXPORTING
          object_name = lv_object_name
          versno      = iv_versno
        TABLES
          d022s_tab   = lt_d022s
        EXCEPTIONS
          OTHERS      = 1.

      CHECK sy-subrc IS INITIAL AND lt_d022s IS NOT INITIAL.

      APPEND LINES OF lt_d022s TO rt_abap.

    ENDIF.
  ENDMETHOD.                    "get_source_by_version

  METHOD get_version_numbers.
    DATA:
      ls_objname TYPE ty_ls_objname,
      lv_objtype TYPE vrsd-objtype,
      lv_objname TYPE versobjnam,
      lv_versno  TYPE versno,
      lt_vrsn    TYPE STANDARD TABLE OF vrsn,
      lt_vrsd    TYPE STANDARD TABLE OF vrsd.

    ls_objname-report = iv_report.
    ls_objname-dynnr  = iv_dynpro.
    lv_objname        = ls_objname.

    IF iv_dynpro IS INITIAL.
      lv_objtype = 'REPS'.
    ELSE.
      lv_objtype = 'DYNP'.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
      EXPORTING
        objname      = lv_objname
        objtype      = lv_objtype
      TABLES
        lversno_list = lt_vrsn
        version_list = lt_vrsd
      EXCEPTIONS
        OTHERS       = 1.

    CHECK sy-subrc IS INITIAL .

    SORT lt_vrsd BY objname versno.
    DELETE ADJACENT DUPLICATES FROM lt_vrsd COMPARING objname versno.

    rt_vrsd = lt_vrsd.

    DELETE TABLE rt_vrsd WITH TABLE KEY objname = lv_objname
                                        versno  = lv_versno.

    SORT rt_vrsd.

    CHECK iv_dynpro IS NOT INITIAL.
*   For dynpros we need to save the version information for the version display
*   this is not required for source code
    INSERT LINES OF rt_vrsd INTO TABLE gt_vrsd.

  ENDMETHOD.                    "get_version_Numbers

  METHOD search_abap_source.
    DATA:
      percentage     TYPE i,
      old_percentage TYPE i VALUE -1,
      text           TYPE c LENGTH 150.

    LOOP AT gt_object INTO gv_report.

      IF sy-batch IS INITIAL.
        percentage = sy-tabix * 100 / lines( gt_object ).
        text = |SEARCH ABAP SOURCES ({ sy-tabix }/{ lines( gt_object ) })...|.

        IF old_percentage <> percentage.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = percentage
              text       = text.
          old_percentage = percentage.
        ENDIF.
      ENDIF.

      READ REPORT gv_report INTO gt_source.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      search_source( ).

    ENDLOOP.

    FREE gt_object.

  ENDMETHOD.

  METHOD search_source.
    DATA:
      lt_source_vers  TYPE abaptxt255_tab,
      lv_string_found TYPE xfeld,
      lt_vrsd         TYPE STANDARD TABLE OF vrsd,
      ls_vrsd         LIKE LINE OF lt_vrsd,
      lv_number       TYPE i,
      lv_index        TYPE i,
      lt_results      TYPE match_result_tab,
      ls_result       LIKE LINE OF lt_results,
      ls_sstring      LIKE LINE OF sstring.

    IF p_vers IS INITIAL.
      lv_number = 1.
    ELSE.
      lt_vrsd = get_version_numbers( iv_report = gv_report
                                     iv_dynpro = gv_dynpro ).

      lv_number = lines( lt_vrsd ) + 1.
    ENDIF.

    DO lv_number TIMES.
      CLEAR lv_string_found.

      IF sy-index = 1.
        CLEAR ls_vrsd.
      ELSE.
        lv_index = sy-index - 1.
        READ TABLE lt_vrsd INDEX lv_index INTO ls_vrsd.
        CHECK sy-subrc IS INITIAL.

        lt_source_vers = get_source_by_version( iv_report = gv_report
                                                iv_dynpro = gv_dynpro
                                                iv_versno = ls_vrsd-versno ).

        IF lt_source_vers IS NOT INITIAL.
          gt_source = lt_source_vers.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      LOOP AT sstring INTO ls_sstring.
        REFRESH lt_results.

        IF p_regex IS INITIAL.
          FIND ALL OCCURRENCES OF ls_sstring-low IN TABLE gt_source
            IN CHARACTER MODE
            IGNORING CASE
            RESULTS lt_results.
        ELSE.
          TRY.
              FIND ALL OCCURRENCES OF REGEX ls_sstring-low IN TABLE gt_source
                IN CHARACTER MODE
                IGNORING CASE
                RESULTS lt_results.
            CATCH cx_sy_regex.
*             invalid regex -> stop processing
              MESSAGE s384 WITH ls_sstring-low DISPLAY LIKE 'E'.
              RAISE EXCEPTION TYPE lcx_scan_exceptions.
          ENDTRY.
        ENDIF.

"$. Region added
        IF p_zz_mod = abap_true AND lt_results IS NOT INITIAL.
          lcl_zz=>filter_modifications( EXPORTING i_include     = gv_report
                                                  i_screen      = gv_dynpro
                                                  i_source_tab   = gt_source
                                        CHANGING  c_findings = lt_results ).
        ENDIF.
"$. Endregion added
        CHECK lt_results IS NOT INITIAL.

        lv_string_found = gc_x.

        SORT lt_results BY line.
        DELETE ADJACENT DUPLICATES FROM lt_results COMPARING line.

        LOOP AT lt_results INTO ls_result.
          CALL METHOD get_hit_set
            EXPORTING
              iv_report = gv_report
              iv_dynpro = gv_dynpro
              it_abap   = gt_source
              iv_tabix  = ls_result-line
              iv_versno = ls_vrsd-versno.
        ENDLOOP.

      ENDLOOP.
      IF lv_string_found IS NOT INITIAL.
*       Add ALV header entry
        CLEAR gs_alv_header.

        gs_alv_header-repname = gv_report.
        gs_alv_header-dynnr   = gv_dynpro.
        gs_alv_header-versno  = ls_vrsd-versno.
        APPEND gs_alv_header TO gt_alv_header.

        IF gv_dynpro IS NOT INITIAL.
          gv_dynp_found = gc_x.
        ENDIF.

        IF ls_vrsd-versno IS NOT INITIAL.
          gv_vers_found = gc_x.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD search_dynpro_source.

    DATA ls_dynpro LIKE LINE OF gt_dynpro.

    IF sy-batch IS INITIAL.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = 'SEARCH DYNPRO SOURCES...'.
    ENDIF.

    LOOP AT gt_dynpro INTO ls_dynpro.
      REFRESH gt_source.

      gv_report = ls_dynpro-repname.
      gv_dynpro = ls_dynpro-dynnr.

      gt_source = get_dynpro_flow_logic( iv_report =  ls_dynpro-repname
                                         iv_dynpro  = ls_dynpro-dynnr ).

      CHECK gt_source IS NOT INITIAL.

      search_source( ).

    ENDLOOP.

  ENDMETHOD.

  METHOD get_dynpros.
    CHECK gt_object IS NOT INITIAL.

    SELECT prog dnum INTO TABLE gt_dynpro
      FROM d020s FOR ALL ENTRIES IN gt_object
      WHERE prog = gt_object-table_line
      AND   dnum IN dynnr.
  ENDMETHOD.                    "get_dynpros

  METHOD get_includes.
    DATA:
      lt_inc         TYPE STANDARD TABLE OF tadir-obj_name,
      lt_inc_tmp     LIKE lt_inc,
      lv_program     TYPE sy-repid,
      lv_obj         TYPE tadir-obj_name,
      class_name     TYPE seoclsname,
      class_includes TYPE seoincl_t.

    CHECK inclu IS NOT INITIAL.

    LOOP AT gt_object INTO lv_obj.    "for classes we already have the includes

      IF lv_obj+30(2) = 'CP'. "Class Pool
        DELETE gt_object INDEX sy-tabix.

        class_name = lv_obj(30).
        TRANSLATE class_name USING '= '.

        cl_oo_classname_service=>get_all_class_includes(
          EXPORTING class_name = class_name
          RECEIVING result     = class_includes
          EXCEPTIONS OTHERS    = 0
        ).
        DELETE class_includes WHERE table_line+30(2) = 'CS' OR table_line+30(2) = 'CP'.
        APPEND LINES OF class_includes TO lt_inc.

      ELSEIF lv_obj+30(2) = 'IP'. "Interface Pool
        DELETE gt_object INDEX sy-tabix.

        lv_obj+31(1) = 'U'.
        APPEND lv_obj TO lt_inc.
      ENDIF.

      REFRESH lt_inc_tmp.
      lv_program = lv_obj.

      CALL FUNCTION 'RS_GET_ALL_INCLUDES'
        EXPORTING
          program    = lv_program
        TABLES
          includetab = lt_inc_tmp
        EXCEPTIONS
          OTHERS     = 0.

      APPEND LINES OF lt_inc_tmp TO lt_inc.
    ENDLOOP.

    SORT lt_inc.
    DELETE ADJACENT DUPLICATES FROM lt_inc.

    APPEND LINES OF lt_inc TO gt_object.

  ENDMETHOD.

  METHOD get_report_names.
    SELECT obj_name INTO TABLE gt_object
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'PROG'
      AND   devclass IN devclass.                       "#EC CI_GENBUFF
  ENDMETHOD.                    "get_report_names

  METHOD get_function_names.
    DATA:
      lt_obj     TYPE STANDARD TABLE OF tadir-obj_name,
      lv_obj     TYPE tadir-obj_name,
      lv_fgroup  TYPE rs38l-area,
      lv_program TYPE progname.

    FIELD-SYMBOLS:
      <lv_obj> LIKE LINE OF lt_obj.

    SELECT obj_name INTO TABLE lt_obj
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'FUGR'
      AND   devclass IN devclass
      AND   obj_name IN funcgrp.                        "#EC CI_GENBUFF

    LOOP AT lt_obj ASSIGNING <lv_obj>.
      lv_fgroup = <lv_obj>.
      CLEAR lv_program.

      CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
        CHANGING
          program       = lv_program
          complete_area = lv_fgroup
        EXCEPTIONS
          OTHERS        = 1.

      CHECK sy-subrc IS INITIAL AND lv_program IS NOT INITIAL.

      lv_obj = lv_program.
      APPEND lv_obj TO gt_object.
    ENDLOOP.
  ENDMETHOD.                    "get_function_names

  METHOD get_class_names.
    DATA lt_obj TYPE STANDARD TABLE OF tadir-obj_name.
    DATA ls_obj TYPE tadir-obj_name.

    SELECT obj_name INTO TABLE lt_obj
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'CLAS'
      AND   devclass IN devclass
      AND   obj_name IN p_class.                        "#EC CI_GENBUFF

    LOOP AT lt_obj INTO ls_obj.
      APPEND cl_oo_classname_service=>get_classpool_name( |{ ls_obj }| ) TO gt_object.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_interface_names.
    DATA lt_obj TYPE STANDARD TABLE OF tadir-obj_name.
    DATA ls_obj TYPE tadir-obj_name.

    SELECT obj_name INTO TABLE lt_obj
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'INTF'
      AND   devclass IN devclass
      AND   obj_name IN p_class.                        "#EC CI_GENBUFF

    LOOP AT lt_obj INTO ls_obj.
      APPEND cl_oo_classname_service=>get_interfacepool_name( |{ ls_obj }| ) TO gt_object.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_source_names.
    IF repname[] IS NOT INITIAL OR
       cnam[]    IS NOT INITIAL OR
       unam[]    IS NOT INITIAL OR
       subc[]    IS NOT INITIAL OR
       appl[]    IS NOT INITIAL.

      SELECT name APPENDING TABLE gt_object
        FROM trdir
        WHERE name IN repname
        AND   cnam IN cnam
        AND   unam IN unam
        AND   subc IN subc
        AND   appl IN appl.
    ENDIF.

    IF devclass[] IS NOT INITIAL.
      get_report_names( ).
      get_function_names( ).
      get_class_names( ).
      get_interface_names( ).
    ENDIF.

    IF funcgrp[] IS NOT INITIAL.
      get_function_names( ).
    ENDIF.

    IF p_class[] IS NOT INITIAL.
      get_class_names( ).
      get_interface_names( ).
    ENDIF.

    IF rb_code IS INITIAL.
      get_dynpros( ).
    ENDIF.

"$. Region added
    IF p_zz_mod = abap_true.
      lcl_zz=>add_enhancement_includes( CHANGING c_includes = gt_object ).
      lcl_zz=>add_modification_includes( CHANGING c_includes = gt_object
                                                  c_screens  = gt_dynpro ).
    ENDIF.

"$. Endregion added
  ENDMETHOD.                    "get_source_names

  METHOD start.
    DATA:
     ls_sstring LIKE LINE OF sstring[].

    IF sstring[] IS INITIAL AND modiass IS INITIAL.
*     Please specifiy a search term
      MESSAGE s385 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF NOT modiass IS INITIAL.
      REFRESH sstring.
      ls_sstring-sign    = 'I'.
      ls_sstring-option  = 'EQ'.
      ls_sstring-low     = '^\*\{'.
      APPEND ls_sstring TO sstring.
      ls_sstring-low     = '^\*\}'.
      APPEND ls_sstring TO sstring.

      p_regex = gc_x.
    ENDIF.

    READ TABLE sstring[] INTO ls_sstring INDEX 1.
    IF lines( sstring[] ) = 1.
      gv_sstring = ls_sstring-low.
    ELSE.
      CONCATENATE ls_sstring-low
                  '...'
                  INTO gv_sstring.
    ENDIF.

    IF sy-batch IS INITIAL.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = 'GET INCLUDES...'.
    ENDIF.

    get_source_names( ).
    get_includes( ).

    IF rb_dyn IS INITIAL.
      TRY.
          search_abap_source( ).
        CATCH lcx_scan_exceptions.
          RETURN.
      ENDTRY.
    ENDIF.

    IF rb_code IS INITIAL.
      TRY.
          search_dynpro_source( ).
        CATCH lcx_scan_exceptions.
          RETURN.
      ENDTRY.
    ENDIF.

    display( ).
  ENDMETHOD.                    "start

  METHOD pbo.
    DATA ls_screen TYPE screen.

    CHECK modiass IS NOT INITIAL.

    REFRESH sstring[].
    CLEAR   sstring.
    CLEAR   p_regex.

    LOOP AT SCREEN INTO ls_screen.
      CHECK ls_screen-group1 = 'DSP'.
      ls_screen-input = '0'.
      MODIFY screen FROM ls_screen.
    ENDLOOP.
  ENDMETHOD.                    "pbo
ENDCLASS.                    "lcl_source_scan IMPLEMENTATION

INITIALIZATION.
  CREATE OBJECT lo_sscan.

  a05 = 'Suchbegriff'(a05).
  a10 = 'Report/Dynpro Selektion'(a10).
  a11 = 'Paket Selektion'(a11).
  a12 = 'Funktionsgruppen Selektion'(a12).
  a13 = 'Klassen Selektion'(a13).
  a20 = 'Suchkriterien'(a20).
  a30 = 'Suchbereich'(a30).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_class-low.
  lo_sscan->f4_class( CHANGING cv_class_name = p_class-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_class-high.
  lo_sscan->f4_class( CHANGING cv_class_name = p_class-high ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR funcgrp-low.
  lo_sscan->f4_function_group( 'FUNCGRP-LOW' ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR funcgrp-high.
  lo_sscan->f4_function_group( 'FUNCGRP-HIGH' ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR repname-low.
  lo_sscan->f4_repname( CHANGING cv_repname = repname-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR repname-high.
  lo_sscan->f4_repname( CHANGING cv_repname = repname-high ).

AT SELECTION-SCREEN OUTPUT.
  lo_sscan->pbo( ).

START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
    ID 'DEVCLASS' DUMMY
    ID 'OBJTYPE'  DUMMY
    ID 'OBJNAME'  DUMMY
    ID 'P_GROUP'  DUMMY
    ID 'ACTVT'    FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE e516(eu) WITH '03' 'S_DEVELOP'.
  ENDIF.

  lo_sscan->start( ).
