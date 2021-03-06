# Copyright (c) 2000, Red Hat, Inc.
# 
# This file is part of Source-Navigator.
# 
# Source-Navigator is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# Source-Navigator is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with Source-Navigator; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.
# 
# html.tcl - Routines for generating HTML documentation
# Copyright (C) 1998 Cygnus Solutions.

set sym_description(cl) "Class"
set sym_description(com) "Common"
set sym_description(con) "Constants"
set sym_description(cov) "Common variable"
set sym_description(e) "Enum"
set sym_description(ec) "Enum value"
set sym_description(ex) "Exception"
set sym_description(fd) "Function declaration"
set sym_description(fr) "Friends"
set sym_description(fu) "Function"
set sym_description(gv) "Global variable"
set sym_description(in) "Substructure"
set sym_description(iu) "Include"
set sym_description(iv) "Inst variable"
set sym_description(lv) "Local variable"
set sym_description(ma) "Macro"
set sym_description(md) "Method definition"
set sym_description(mi) "Method implementation"
set sym_description(su) "Subroutine implementation"
set sym_description(t) "Typedef"
set sym_description(ud) "Undefined"
set sym_description(un) "Union"

proc list_symbol_defs {class sym type file} {
    global sym_description sn_sep

    if {[info commands paf_db_${type}] == ""} {
        return
    }
    if {${class} == "#"} {
        set qry "${sym}${sn_sep}"
    } else {
        set qry "${class}${sn_sep}${sym}"
    }
    set sym_tp $sym_description(${type})
    foreach def [paf_db_${type} seq -end ${file} ${qry}] {
        set key [lindex ${def} 0]
        set data [lindex ${def} 1]

        set field_num [llength ${key}]
        if {${field_num} == 3} {
            set class ""
            set sym [lindex ${key} 0]
            set pos [lindex ${key} 1]
        } else {
            set class [lindex ${key} 0]
            set sym [lindex ${key} 1]
            set pos [lindex ${key} 2]
        }

        set decl_type [lindex ${data} 1]
        set pars [lindex ${data} 2]
        set comment [lindex ${data} 3]

        if {${type} == "su" || ${type} == "fu"} {
            set tag "function"
        } else {
            set tag ""
        }
        if {[string compare ${comment} ""] == 0} {
            set comment [get_comment_infront_of_pos ${pos} ${file}]
        }
        doc_put_line ${tag} ${sym} ${class} ${sym_tp} ${decl_type} ${comment}\
          ${pars}
    }
}

# The input list describes the files we are interested in.
proc list_file_symbols {files} {
    global sn_options sn_sep
    if {[catch {set db [dbopen rem_db $sn_options(db_files_prefix).rem RDONLY\
      [sn_db_perms] btree]}]} {
        set db ""
    }

    if {[info commands paf_db_fil] == ""} {
        return
    }
    foreach f ${files} {
        set f [string trim ${f}]

        put_file_defheader ${f}

        foreach def [paf_db_fil seq -data "${f}${sn_sep}"] {
            set pos [lindex ${def} 1]
            set class [lindex ${def} 2]
            set sym [lindex ${def} 3]
            set type [lindex ${def} 4]

            list_symbol_defs ${class} ${sym} ${type} ${f}
        }
        put_file_deffooter ${f}

        list_cross_ref ${f} 0
        if {${db} != ""} {
            list_comments ${db} ${f}
        }
    }
    if {${db} != ""} {
        ${db} close
    }
}

# This functions filters the comments of a file:
#	1. Simple (one line) comments.
#	2. Compound (with ranges) comments.
#
# Examples:
#	Simple comments (examples):
#		/* AUTHOR: Zsolt Koppany */
#		/* DATE: 30-June.-1996 */
#		/* REQUIREMENT: At least 6 Mb */
#
#	Compound comments (examples):
#		/* Desc START: */
#		/* This procedure reads and filters comments of source.
#		   files. Comments can be simple or compound. */
#		/* Desc END: */
#		/* WARNING START: */
#		/* '\' characters of in file names
#		   will be converted to '/'! */
#		/* WARNING END: */
#
proc list_comments {db file} {
    global sn_sep
    set simple_comments [list PUI: AUTHOR: DATE: CSC: REQUIREMENT:]
    set compound_beg ".*START:"
    set compound_end ".*END:"

    # Get all comments of a file!
    set com [${db} seq -key "${file}${sn_sep}"]

    # Select the simple comments!
    set qry "\([join ${simple_comments} "\)|\("]\)"

    # Now the query looks like: (PUI:)|(AUTHOR:)|(DATE:)|(CSC:)|(REQUIREMENT:).
    set qry "^\[ \t\]*${qry}.*$"
    set s_coms [lmatch -regexp ${com} ${qry}]

    doc_insert_comments ${s_coms}

    # Select the compound comments!
    set len [llength ${com}]

    for {set off [lsearch -regexp ${com} ${compound_beg}]} {${off} != -1}\
      {set off [lsearch -regexp ${com} ${compound_beg}]} {

        set end_off [lsearch -regexp ${com} ${compound_end}]

        if {${end_off} == -1} {
            break
        }
        doc_insert_comments [lrange ${com} ${off} ${end_off}]
        set com [lrange ${com} [expr ${end_off} + 1] end]
    }
}

# The procedure below searches for the comment infront of a symbol
# position. It works only for projects that use comment styles as the
# ("C") example below shows:
# /*
#  * This function converts strings
#  * from ADOBE to ISO.
#  */
#  char *
#  cvtadobe(in,out)
#  ...
proc get_comment_infront_of_pos {defpos file} {
    global sn_options sn_sep

    # We seek to the record containing the position $defpos.
    # The parameter '-col [list "0 { }" 1]' will assure that the
    # first returned field contains the file name and the second
    # the position. The parameter '-first' means that we want to fetch
    # exatcly one record.
    paf_db_fil seq -first -col {0 1} "${file}${sn_sep}${defpos}${sn_sep}"

    # Seek one record back to the previous symbol definition!
    # The '*.fil' databases are sorted by file name and position, thus
    # the previous record contains the previous symbol definition.
    set prev_sym_def [paf_db_fil seq -first -data -col {0 1} "" "R_PREV"]
    set prev_sym_def [lindex ${prev_sym_def} 0]
    set prev_sym_file [lindex ${prev_sym_def} 0]

    if {${prev_sym_file} != ${file}} {
        set prev_sym_pos -1
    } else {
        set prev_sym_pos [lindex ${prev_sym_def} 1]
        # The second field contains the position.
    }

    # Convert the position to integer!
    set defpos [expr int(${defpos})]

    # The instruction below selects the comments of a file.
    # They are sorted by file_name and position (line.column).
    if {[catch {set comments [rem_db seq "${file}${sn_sep}"]}]} {
        return
    }
    set saved_comment ""
    foreach c ${comments} {
        set com_key [lindex ${c} 0]
        set com_data [lindex ${c} 1]
        set com_pos [split [lindex ${com_key} 1] "."]
        set com_line [string trimleft [lindex ${com_pos} 0] "0"]
        set com_col [string trimleft [lindex ${com_pos} 1] "0"]
        if {${com_col} == ""} {
            set com_col 0
        }
        # Now a restriction: if the column position of a comment is greater
        # than 0 we ignore it !!!
        if {${com_col} > 0} {
            continue
        }

        #	The comments' position must be greater than the position of
        #	the previous symbol definition.
        if {${com_line} > ${defpos}} {
            break
        }
        if {${com_line} > ${prev_sym_pos}} {
            set saved_comment ${com_data}
        }
    }

    return ${saved_comment}
}

# This functions reports the cross references of e file.
# If only_calls is true, only the function and subroutine
# calls will be reported. If uniq is true references to the
# same symbol and access will be reported only once.
proc list_cross_ref {file {only_calls 1} {uniq 1}} {
    global sym_description

    if {[info commands "paf_db_to"] == ""} {
        return
    }
    set last_func ""
    set last_line ""
    set func ""
    foreach c [paf_db_to seq -data -end ${file}] {
        set func [lindex ${c} 1]
        set funcType [lindex ${c} 2]
        set cls [lindex ${c} 3]
        set var [lindex ${c} 4]
        set scope_lev [lindex ${c} 5]
        set access [lindex ${c} 6]

        if {${func} != ${last_func}} {
            if {${last_func} != ""} {
                put_function_cross_end ${func}
            }
            set last_func ${func}

            put_function_cross_beg ${func} $sym_description(${funcType})
        }
        if {${only_calls} && ${scope_lev} != "fu" && ${scope_lev} != "su"} {
            continue
        }
        set lev $sym_description(${scope_lev})

        if {${cls} == "#" || ${scope_lev} == "lv"} {
            set cls ""
        }
        set line "${var}\t${cls}\t${lev}"
        if {[string first ${scope_lev} "cov iv lv gv"] != -1} {
            append line "\t${access}"
        }

        if {${uniq} && ${last_line} == ${line}} {
            continue
        }

        set last_line ${line}
        doc_put_line cross ${var} ${cls} ${lev} ${access}
    }
    if {${func} != ""} {
        put_function_cross_end ${func}
    }

    put_newline
}

proc put_newline {{cou 1}} {
}

proc put_file_deffooter {file} {
    global out_fd
    puts ${out_fd} "</TABLE>"
}

proc put_file_defheader {file} {
    global out_fd

    puts ${out_fd} "<H2>Definitions in ${file}</H2>"
    puts -nonewline ${out_fd} {<TABLE BORDER="1">}

    foreach fd [list Symbol Scope "Symbol type" Var Comment Parameter] {
        puts -nonewline ${out_fd} "<TH>${fd}"
    }
    puts ${out_fd} "<TR>"
}

proc put_function_cross_end {func} {
    global out_fd
    puts ${out_fd} "</TABLE>"
}

proc put_function_cross_beg {func type} {
    global out_fd

    puts ${out_fd} "<H2>Cross references in ${func} \(${type}\)</H2>"
    puts -nonewline ${out_fd} {<TABLE BORDER="1">}

    foreach fd [list Symbol Scope Type Access] {
        puts -nonewline ${out_fd} "<TH>${fd}"
    }
    puts ${out_fd} "<TR>"
}

proc put_fields args {
    global out_fd

    foreach f ${args} {
        puts -nonewline ${out_fd} "<TD>${f}"
    }
    puts ${out_fd} "<TR>"
}

proc doc_put_line args {
    eval put_fields [lrange ${args} 1 end]
}

proc doc_insert_comments {coms} {
    global out_fd

    puts ${out_fd} {<FONT FACE=Arial SIZE=3>}
    puts ${out_fd} {<pre>}

    foreach c ${coms} {
        puts ${out_fd} ${c}
    }
    puts ${out_fd} {</pre>}
}

proc doc_start {{files ""}} {
    global sn_options
    global out_fd

    #make sure we stay in project directory (different interpeters).
    catch {cd $sn_options(sys,project-dir)}

    if {${files} == "all"} {
        set files [sn_project_file_list 0]
    }
    if {[string compare ${files} ""] == 0} {
        return
    }

    set html_file [file rootname [file tail [lindex ${files} 0]]]
    set html_file [file join $sn_options(both,db-directory) ${html_file}.html]

    set out_fd [open ${html_file} "w+"]
    fconfigure ${out_fd} -encoding $sn_options(def,system-encoding) -blocking 0

    puts ${out_fd} {<HEAD>}
    puts ${out_fd} {<TITLE>Source-Navigator HTML Generator</TITLE>}
    puts ${out_fd} {</HEAD>}

    puts ${out_fd} {<FONT FACE=Arial SIZE=3>}

    list_file_symbols ${files}

    close ${out_fd}

    sn_help [file join $sn_options(sys,project-dir) ${html_file}] 0

}

