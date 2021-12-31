empty_:
        DB ";"

backsp_:
        DB "\\c@0=0=(1_\\c\\+`\b \b`);"
        ; DB "1_\\c\\+`\b \b`;"
        ; DB ";"

reedit_:
        DB "\\e\\@\\Z;"

edit_:
        .cstr "`?`\\K\\N`> `\\^A-\\Z;"

list_:
        .cstr "\\N26(\\i@\\Z\\c@0>(\\N))\\N`> `;"

printStack_:
        .cstr "`=> `\\P\\N\\N`> `;"        

toggleBase_:
        .cstr "\\b@0=\\b!;"

