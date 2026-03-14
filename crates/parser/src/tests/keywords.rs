use expect_test::expect;

use crate::tests::check;

#[test]
fn reserved_words_do_not_parse() {
    check(
        "
        var NULL = 0;
        var Self = 0;
        var abstract = 0;
        var active = 0;
        var alignas = 0;
        var alignof = 0;
        var as = 0;
        var asm = 0;
        var asm_fragment = 0;
        var async = 0;
        var attribute = 0;
        var auto = 0;
        var await = 0;
        var become = 0;
        var cast = 0;
        var catch = 0;
        var class = 0;
        var co_await = 0;
        var co_return = 0;
        var co_yield = 0;
        var coherent = 0;
        var column_major = 0;
        var common = 0;
        var compile = 0;
        var compile_fragment = 0;
        var concept = 0;
        var const_cast = 0;
        var consteval = 0;
        var constexpr = 0;
        var constinit = 0;
        var crate = 0;
        var debugger = 0;
        var decltype = 0;
        var delete = 0;
        var demote = 0;
        var demote_to_helper = 0;
        var do = 0;
        var dynamic_cast = 0;
        var enum = 0;
        var explicit = 0;
        var export = 0;
        var extends = 0;
        var extern = 0;
        var external = 0;
        var fallthrough = 0;
        var filter = 0;
        var final = 0;
        var finally = 0;
        var friend = 0;
        var from = 0;
        var fxgroup = 0;
        var get = 0;
        var goto = 0;
        var groupshared = 0;
        var highp = 0;
        var impl = 0;
        var implements = 0;
        var import = 0;
        var inline = 0;
        var instanceof = 0;
        var interface = 0;
        var layout = 0;
        var lowp = 0;
        var macro = 0;
        var macro_rules = 0;
        var match = 0;
        var mediump = 0;
        var meta = 0;
        var mod = 0;
        var module = 0;
        var move = 0;
        var mut = 0;
        var mutable = 0;
        var namespace = 0;
        var new = 0;
        var nil = 0;
        var noexcept = 0;
        var noinline = 0;
        var nointerpolation = 0;
        var non_coherent = 0;
        var noncoherent = 0;
        var noperspective = 0;
        var null = 0;
        var nullptr = 0;
        var of = 0;
        var operator = 0;
        var package = 0;
        var packoffset = 0;
        var partition = 0;
        var pass = 0;
        var patch = 0;
        var pixelfragment = 0;
        var precise = 0;
        var precision = 0;
        var premerge = 0;
        var priv = 0;
        var protected = 0;
        var pub = 0;
        var public = 0;
        var readonly = 0;
        var ref = 0;
        var regardless = 0;
        var register = 0;
        var reinterpret_cast = 0;
        var require = 0;
        var resource = 0;
        var restrict = 0;
        var self = 0;
        var set = 0;
        var shared = 0;
        var sizeof = 0;
        var smooth = 0;
        var snorm = 0;
        var static = 0;
        var static_assert = 0;
        var static_cast = 0;
        var std = 0;
        var subroutine = 0;
        var super = 0;
        var target = 0;
        var template = 0;
        var this = 0;
        var thread_local = 0;
        var throw = 0;
        var trait = 0;
        var try = 0;
        var type = 0;
        var typedef = 0;
        var typeid = 0;
        var typename = 0;
        var typeof = 0;
        var union = 0;
        var unless = 0;
        var unorm = 0;
        var unsafe = 0;
        var unsized = 0;
        var use = 0;
        var using = 0;
        var varying = 0;
        var virtual = 0;
        var volatile = 0;
        var wgsl = 0;
        var where = 0;
        var with = 0;
        var writeonly = 0;
        var yield = 0;
        ",
        expect![[r#"
            SourceFile@0..3635
              Blankspace@0..9 "\n        "
              VariableDeclaration@9..22
                Var@9..12 "var"
                Blankspace@12..13 " "
                Error@13..17
                  Reserved@13..17 "NULL"
                Blankspace@17..18 " "
                Equal@18..19 "="
                Blankspace@19..20 " "
                Literal@20..21
                  IntLiteral@20..21 "0"
                Semicolon@21..22 ";"
              Blankspace@22..31 "\n        "
              VariableDeclaration@31..44
                Var@31..34 "var"
                Blankspace@34..35 " "
                Error@35..39
                  Reserved@35..39 "Self"
                Blankspace@39..40 " "
                Equal@40..41 "="
                Blankspace@41..42 " "
                Literal@42..43
                  IntLiteral@42..43 "0"
                Semicolon@43..44 ";"
              Blankspace@44..53 "\n        "
              VariableDeclaration@53..70
                Var@53..56 "var"
                Blankspace@56..57 " "
                Error@57..65
                  Reserved@57..65 "abstract"
                Blankspace@65..66 " "
                Equal@66..67 "="
                Blankspace@67..68 " "
                Literal@68..69
                  IntLiteral@68..69 "0"
                Semicolon@69..70 ";"
              Blankspace@70..79 "\n        "
              VariableDeclaration@79..94
                Var@79..82 "var"
                Blankspace@82..83 " "
                Error@83..89
                  Reserved@83..89 "active"
                Blankspace@89..90 " "
                Equal@90..91 "="
                Blankspace@91..92 " "
                Literal@92..93
                  IntLiteral@92..93 "0"
                Semicolon@93..94 ";"
              Blankspace@94..103 "\n        "
              VariableDeclaration@103..119
                Var@103..106 "var"
                Blankspace@106..107 " "
                Error@107..114
                  Reserved@107..114 "alignas"
                Blankspace@114..115 " "
                Equal@115..116 "="
                Blankspace@116..117 " "
                Literal@117..118
                  IntLiteral@117..118 "0"
                Semicolon@118..119 ";"
              Blankspace@119..128 "\n        "
              VariableDeclaration@128..144
                Var@128..131 "var"
                Blankspace@131..132 " "
                Error@132..139
                  Reserved@132..139 "alignof"
                Blankspace@139..140 " "
                Equal@140..141 "="
                Blankspace@141..142 " "
                Literal@142..143
                  IntLiteral@142..143 "0"
                Semicolon@143..144 ";"
              Blankspace@144..153 "\n        "
              VariableDeclaration@153..164
                Var@153..156 "var"
                Blankspace@156..157 " "
                Error@157..159
                  Reserved@157..159 "as"
                Blankspace@159..160 " "
                Equal@160..161 "="
                Blankspace@161..162 " "
                Literal@162..163
                  IntLiteral@162..163 "0"
                Semicolon@163..164 ";"
              Blankspace@164..173 "\n        "
              VariableDeclaration@173..185
                Var@173..176 "var"
                Blankspace@176..177 " "
                Error@177..180
                  Reserved@177..180 "asm"
                Blankspace@180..181 " "
                Equal@181..182 "="
                Blankspace@182..183 " "
                Literal@183..184
                  IntLiteral@183..184 "0"
                Semicolon@184..185 ";"
              Blankspace@185..194 "\n        "
              VariableDeclaration@194..215
                Var@194..197 "var"
                Blankspace@197..198 " "
                Error@198..210
                  Reserved@198..210 "asm_fragment"
                Blankspace@210..211 " "
                Equal@211..212 "="
                Blankspace@212..213 " "
                Literal@213..214
                  IntLiteral@213..214 "0"
                Semicolon@214..215 ";"
              Blankspace@215..224 "\n        "
              VariableDeclaration@224..238
                Var@224..227 "var"
                Blankspace@227..228 " "
                Error@228..233
                  Reserved@228..233 "async"
                Blankspace@233..234 " "
                Equal@234..235 "="
                Blankspace@235..236 " "
                Literal@236..237
                  IntLiteral@236..237 "0"
                Semicolon@237..238 ";"
              Blankspace@238..247 "\n        "
              VariableDeclaration@247..265
                Var@247..250 "var"
                Blankspace@250..251 " "
                Error@251..260
                  Reserved@251..260 "attribute"
                Blankspace@260..261 " "
                Equal@261..262 "="
                Blankspace@262..263 " "
                Literal@263..264
                  IntLiteral@263..264 "0"
                Semicolon@264..265 ";"
              Blankspace@265..274 "\n        "
              VariableDeclaration@274..287
                Var@274..277 "var"
                Blankspace@277..278 " "
                Error@278..282
                  Reserved@278..282 "auto"
                Blankspace@282..283 " "
                Equal@283..284 "="
                Blankspace@284..285 " "
                Literal@285..286
                  IntLiteral@285..286 "0"
                Semicolon@286..287 ";"
              Blankspace@287..296 "\n        "
              VariableDeclaration@296..310
                Var@296..299 "var"
                Blankspace@299..300 " "
                Error@300..305
                  Reserved@300..305 "await"
                Blankspace@305..306 " "
                Equal@306..307 "="
                Blankspace@307..308 " "
                Literal@308..309
                  IntLiteral@308..309 "0"
                Semicolon@309..310 ";"
              Blankspace@310..319 "\n        "
              VariableDeclaration@319..334
                Var@319..322 "var"
                Blankspace@322..323 " "
                Error@323..329
                  Reserved@323..329 "become"
                Blankspace@329..330 " "
                Equal@330..331 "="
                Blankspace@331..332 " "
                Literal@332..333
                  IntLiteral@332..333 "0"
                Semicolon@333..334 ";"
              Blankspace@334..343 "\n        "
              VariableDeclaration@343..356
                Var@343..346 "var"
                Blankspace@346..347 " "
                Error@347..351
                  Reserved@347..351 "cast"
                Blankspace@351..352 " "
                Equal@352..353 "="
                Blankspace@353..354 " "
                Literal@354..355
                  IntLiteral@354..355 "0"
                Semicolon@355..356 ";"
              Blankspace@356..365 "\n        "
              VariableDeclaration@365..379
                Var@365..368 "var"
                Blankspace@368..369 " "
                Error@369..374
                  Reserved@369..374 "catch"
                Blankspace@374..375 " "
                Equal@375..376 "="
                Blankspace@376..377 " "
                Literal@377..378
                  IntLiteral@377..378 "0"
                Semicolon@378..379 ";"
              Blankspace@379..388 "\n        "
              VariableDeclaration@388..402
                Var@388..391 "var"
                Blankspace@391..392 " "
                Error@392..397
                  Reserved@392..397 "class"
                Blankspace@397..398 " "
                Equal@398..399 "="
                Blankspace@399..400 " "
                Literal@400..401
                  IntLiteral@400..401 "0"
                Semicolon@401..402 ";"
              Blankspace@402..411 "\n        "
              VariableDeclaration@411..428
                Var@411..414 "var"
                Blankspace@414..415 " "
                Error@415..423
                  Reserved@415..423 "co_await"
                Blankspace@423..424 " "
                Equal@424..425 "="
                Blankspace@425..426 " "
                Literal@426..427
                  IntLiteral@426..427 "0"
                Semicolon@427..428 ";"
              Blankspace@428..437 "\n        "
              VariableDeclaration@437..455
                Var@437..440 "var"
                Blankspace@440..441 " "
                Error@441..450
                  Reserved@441..450 "co_return"
                Blankspace@450..451 " "
                Equal@451..452 "="
                Blankspace@452..453 " "
                Literal@453..454
                  IntLiteral@453..454 "0"
                Semicolon@454..455 ";"
              Blankspace@455..464 "\n        "
              VariableDeclaration@464..481
                Var@464..467 "var"
                Blankspace@467..468 " "
                Error@468..476
                  Reserved@468..476 "co_yield"
                Blankspace@476..477 " "
                Equal@477..478 "="
                Blankspace@478..479 " "
                Literal@479..480
                  IntLiteral@479..480 "0"
                Semicolon@480..481 ";"
              Blankspace@481..490 "\n        "
              VariableDeclaration@490..507
                Var@490..493 "var"
                Blankspace@493..494 " "
                Error@494..502
                  Reserved@494..502 "coherent"
                Blankspace@502..503 " "
                Equal@503..504 "="
                Blankspace@504..505 " "
                Literal@505..506
                  IntLiteral@505..506 "0"
                Semicolon@506..507 ";"
              Blankspace@507..516 "\n        "
              VariableDeclaration@516..537
                Var@516..519 "var"
                Blankspace@519..520 " "
                Error@520..532
                  Reserved@520..532 "column_major"
                Blankspace@532..533 " "
                Equal@533..534 "="
                Blankspace@534..535 " "
                Literal@535..536
                  IntLiteral@535..536 "0"
                Semicolon@536..537 ";"
              Blankspace@537..546 "\n        "
              VariableDeclaration@546..561
                Var@546..549 "var"
                Blankspace@549..550 " "
                Error@550..556
                  Reserved@550..556 "common"
                Blankspace@556..557 " "
                Equal@557..558 "="
                Blankspace@558..559 " "
                Literal@559..560
                  IntLiteral@559..560 "0"
                Semicolon@560..561 ";"
              Blankspace@561..570 "\n        "
              VariableDeclaration@570..586
                Var@570..573 "var"
                Blankspace@573..574 " "
                Error@574..581
                  Reserved@574..581 "compile"
                Blankspace@581..582 " "
                Equal@582..583 "="
                Blankspace@583..584 " "
                Literal@584..585
                  IntLiteral@584..585 "0"
                Semicolon@585..586 ";"
              Blankspace@586..595 "\n        "
              VariableDeclaration@595..620
                Var@595..598 "var"
                Blankspace@598..599 " "
                Error@599..615
                  Reserved@599..615 "compile_fragment"
                Blankspace@615..616 " "
                Equal@616..617 "="
                Blankspace@617..618 " "
                Literal@618..619
                  IntLiteral@618..619 "0"
                Semicolon@619..620 ";"
              Blankspace@620..629 "\n        "
              VariableDeclaration@629..645
                Var@629..632 "var"
                Blankspace@632..633 " "
                Error@633..640
                  Reserved@633..640 "concept"
                Blankspace@640..641 " "
                Equal@641..642 "="
                Blankspace@642..643 " "
                Literal@643..644
                  IntLiteral@643..644 "0"
                Semicolon@644..645 ";"
              Blankspace@645..654 "\n        "
              VariableDeclaration@654..673
                Var@654..657 "var"
                Blankspace@657..658 " "
                Error@658..668
                  Reserved@658..668 "const_cast"
                Blankspace@668..669 " "
                Equal@669..670 "="
                Blankspace@670..671 " "
                Literal@671..672
                  IntLiteral@671..672 "0"
                Semicolon@672..673 ";"
              Blankspace@673..682 "\n        "
              VariableDeclaration@682..700
                Var@682..685 "var"
                Blankspace@685..686 " "
                Error@686..695
                  Reserved@686..695 "consteval"
                Blankspace@695..696 " "
                Equal@696..697 "="
                Blankspace@697..698 " "
                Literal@698..699
                  IntLiteral@698..699 "0"
                Semicolon@699..700 ";"
              Blankspace@700..709 "\n        "
              VariableDeclaration@709..727
                Var@709..712 "var"
                Blankspace@712..713 " "
                Error@713..722
                  Reserved@713..722 "constexpr"
                Blankspace@722..723 " "
                Equal@723..724 "="
                Blankspace@724..725 " "
                Literal@725..726
                  IntLiteral@725..726 "0"
                Semicolon@726..727 ";"
              Blankspace@727..736 "\n        "
              VariableDeclaration@736..754
                Var@736..739 "var"
                Blankspace@739..740 " "
                Error@740..749
                  Reserved@740..749 "constinit"
                Blankspace@749..750 " "
                Equal@750..751 "="
                Blankspace@751..752 " "
                Literal@752..753
                  IntLiteral@752..753 "0"
                Semicolon@753..754 ";"
              Blankspace@754..763 "\n        "
              VariableDeclaration@763..777
                Var@763..766 "var"
                Blankspace@766..767 " "
                Error@767..772
                  Reserved@767..772 "crate"
                Blankspace@772..773 " "
                Equal@773..774 "="
                Blankspace@774..775 " "
                Literal@775..776
                  IntLiteral@775..776 "0"
                Semicolon@776..777 ";"
              Blankspace@777..786 "\n        "
              VariableDeclaration@786..803
                Var@786..789 "var"
                Blankspace@789..790 " "
                Error@790..798
                  Reserved@790..798 "debugger"
                Blankspace@798..799 " "
                Equal@799..800 "="
                Blankspace@800..801 " "
                Literal@801..802
                  IntLiteral@801..802 "0"
                Semicolon@802..803 ";"
              Blankspace@803..812 "\n        "
              VariableDeclaration@812..829
                Var@812..815 "var"
                Blankspace@815..816 " "
                Error@816..824
                  Reserved@816..824 "decltype"
                Blankspace@824..825 " "
                Equal@825..826 "="
                Blankspace@826..827 " "
                Literal@827..828
                  IntLiteral@827..828 "0"
                Semicolon@828..829 ";"
              Blankspace@829..838 "\n        "
              VariableDeclaration@838..853
                Var@838..841 "var"
                Blankspace@841..842 " "
                Error@842..848
                  Reserved@842..848 "delete"
                Blankspace@848..849 " "
                Equal@849..850 "="
                Blankspace@850..851 " "
                Literal@851..852
                  IntLiteral@851..852 "0"
                Semicolon@852..853 ";"
              Blankspace@853..862 "\n        "
              VariableDeclaration@862..877
                Var@862..865 "var"
                Blankspace@865..866 " "
                Error@866..872
                  Reserved@866..872 "demote"
                Blankspace@872..873 " "
                Equal@873..874 "="
                Blankspace@874..875 " "
                Literal@875..876
                  IntLiteral@875..876 "0"
                Semicolon@876..877 ";"
              Blankspace@877..886 "\n        "
              VariableDeclaration@886..911
                Var@886..889 "var"
                Blankspace@889..890 " "
                Error@890..906
                  Reserved@890..906 "demote_to_helper"
                Blankspace@906..907 " "
                Equal@907..908 "="
                Blankspace@908..909 " "
                Literal@909..910
                  IntLiteral@909..910 "0"
                Semicolon@910..911 ";"
              Blankspace@911..920 "\n        "
              VariableDeclaration@920..931
                Var@920..923 "var"
                Blankspace@923..924 " "
                Error@924..926
                  Reserved@924..926 "do"
                Blankspace@926..927 " "
                Equal@927..928 "="
                Blankspace@928..929 " "
                Literal@929..930
                  IntLiteral@929..930 "0"
                Semicolon@930..931 ";"
              Blankspace@931..940 "\n        "
              VariableDeclaration@940..961
                Var@940..943 "var"
                Blankspace@943..944 " "
                Error@944..956
                  Reserved@944..956 "dynamic_cast"
                Blankspace@956..957 " "
                Equal@957..958 "="
                Blankspace@958..959 " "
                Literal@959..960
                  IntLiteral@959..960 "0"
                Semicolon@960..961 ";"
              Blankspace@961..970 "\n        "
              VariableDeclaration@970..983
                Var@970..973 "var"
                Blankspace@973..974 " "
                Error@974..978
                  Reserved@974..978 "enum"
                Blankspace@978..979 " "
                Equal@979..980 "="
                Blankspace@980..981 " "
                Literal@981..982
                  IntLiteral@981..982 "0"
                Semicolon@982..983 ";"
              Blankspace@983..992 "\n        "
              VariableDeclaration@992..1009
                Var@992..995 "var"
                Blankspace@995..996 " "
                Error@996..1004
                  Reserved@996..1004 "explicit"
                Blankspace@1004..1005 " "
                Equal@1005..1006 "="
                Blankspace@1006..1007 " "
                Literal@1007..1008
                  IntLiteral@1007..1008 "0"
                Semicolon@1008..1009 ";"
              Blankspace@1009..1018 "\n        "
              VariableDeclaration@1018..1033
                Var@1018..1021 "var"
                Blankspace@1021..1022 " "
                Error@1022..1028
                  Reserved@1022..1028 "export"
                Blankspace@1028..1029 " "
                Equal@1029..1030 "="
                Blankspace@1030..1031 " "
                Literal@1031..1032
                  IntLiteral@1031..1032 "0"
                Semicolon@1032..1033 ";"
              Blankspace@1033..1042 "\n        "
              VariableDeclaration@1042..1058
                Var@1042..1045 "var"
                Blankspace@1045..1046 " "
                Error@1046..1053
                  Reserved@1046..1053 "extends"
                Blankspace@1053..1054 " "
                Equal@1054..1055 "="
                Blankspace@1055..1056 " "
                Literal@1056..1057
                  IntLiteral@1056..1057 "0"
                Semicolon@1057..1058 ";"
              Blankspace@1058..1067 "\n        "
              VariableDeclaration@1067..1082
                Var@1067..1070 "var"
                Blankspace@1070..1071 " "
                Error@1071..1077
                  Reserved@1071..1077 "extern"
                Blankspace@1077..1078 " "
                Equal@1078..1079 "="
                Blankspace@1079..1080 " "
                Literal@1080..1081
                  IntLiteral@1080..1081 "0"
                Semicolon@1081..1082 ";"
              Blankspace@1082..1091 "\n        "
              VariableDeclaration@1091..1108
                Var@1091..1094 "var"
                Blankspace@1094..1095 " "
                Error@1095..1103
                  Reserved@1095..1103 "external"
                Blankspace@1103..1104 " "
                Equal@1104..1105 "="
                Blankspace@1105..1106 " "
                Literal@1106..1107
                  IntLiteral@1106..1107 "0"
                Semicolon@1107..1108 ";"
              Blankspace@1108..1117 "\n        "
              VariableDeclaration@1117..1137
                Var@1117..1120 "var"
                Blankspace@1120..1121 " "
                Error@1121..1132
                  Reserved@1121..1132 "fallthrough"
                Blankspace@1132..1133 " "
                Equal@1133..1134 "="
                Blankspace@1134..1135 " "
                Literal@1135..1136
                  IntLiteral@1135..1136 "0"
                Semicolon@1136..1137 ";"
              Blankspace@1137..1146 "\n        "
              VariableDeclaration@1146..1161
                Var@1146..1149 "var"
                Blankspace@1149..1150 " "
                Error@1150..1156
                  Reserved@1150..1156 "filter"
                Blankspace@1156..1157 " "
                Equal@1157..1158 "="
                Blankspace@1158..1159 " "
                Literal@1159..1160
                  IntLiteral@1159..1160 "0"
                Semicolon@1160..1161 ";"
              Blankspace@1161..1170 "\n        "
              VariableDeclaration@1170..1184
                Var@1170..1173 "var"
                Blankspace@1173..1174 " "
                Error@1174..1179
                  Reserved@1174..1179 "final"
                Blankspace@1179..1180 " "
                Equal@1180..1181 "="
                Blankspace@1181..1182 " "
                Literal@1182..1183
                  IntLiteral@1182..1183 "0"
                Semicolon@1183..1184 ";"
              Blankspace@1184..1193 "\n        "
              VariableDeclaration@1193..1209
                Var@1193..1196 "var"
                Blankspace@1196..1197 " "
                Error@1197..1204
                  Reserved@1197..1204 "finally"
                Blankspace@1204..1205 " "
                Equal@1205..1206 "="
                Blankspace@1206..1207 " "
                Literal@1207..1208
                  IntLiteral@1207..1208 "0"
                Semicolon@1208..1209 ";"
              Blankspace@1209..1218 "\n        "
              VariableDeclaration@1218..1233
                Var@1218..1221 "var"
                Blankspace@1221..1222 " "
                Error@1222..1228
                  Reserved@1222..1228 "friend"
                Blankspace@1228..1229 " "
                Equal@1229..1230 "="
                Blankspace@1230..1231 " "
                Literal@1231..1232
                  IntLiteral@1231..1232 "0"
                Semicolon@1232..1233 ";"
              Blankspace@1233..1242 "\n        "
              VariableDeclaration@1242..1255
                Var@1242..1245 "var"
                Blankspace@1245..1246 " "
                Error@1246..1250
                  Reserved@1246..1250 "from"
                Blankspace@1250..1251 " "
                Equal@1251..1252 "="
                Blankspace@1252..1253 " "
                Literal@1253..1254
                  IntLiteral@1253..1254 "0"
                Semicolon@1254..1255 ";"
              Blankspace@1255..1264 "\n        "
              VariableDeclaration@1264..1280
                Var@1264..1267 "var"
                Blankspace@1267..1268 " "
                Error@1268..1275
                  Reserved@1268..1275 "fxgroup"
                Blankspace@1275..1276 " "
                Equal@1276..1277 "="
                Blankspace@1277..1278 " "
                Literal@1278..1279
                  IntLiteral@1278..1279 "0"
                Semicolon@1279..1280 ";"
              Blankspace@1280..1289 "\n        "
              VariableDeclaration@1289..1301
                Var@1289..1292 "var"
                Blankspace@1292..1293 " "
                Error@1293..1296
                  Reserved@1293..1296 "get"
                Blankspace@1296..1297 " "
                Equal@1297..1298 "="
                Blankspace@1298..1299 " "
                Literal@1299..1300
                  IntLiteral@1299..1300 "0"
                Semicolon@1300..1301 ";"
              Blankspace@1301..1310 "\n        "
              VariableDeclaration@1310..1323
                Var@1310..1313 "var"
                Blankspace@1313..1314 " "
                Error@1314..1318
                  Reserved@1314..1318 "goto"
                Blankspace@1318..1319 " "
                Equal@1319..1320 "="
                Blankspace@1320..1321 " "
                Literal@1321..1322
                  IntLiteral@1321..1322 "0"
                Semicolon@1322..1323 ";"
              Blankspace@1323..1332 "\n        "
              VariableDeclaration@1332..1352
                Var@1332..1335 "var"
                Blankspace@1335..1336 " "
                Error@1336..1347
                  Reserved@1336..1347 "groupshared"
                Blankspace@1347..1348 " "
                Equal@1348..1349 "="
                Blankspace@1349..1350 " "
                Literal@1350..1351
                  IntLiteral@1350..1351 "0"
                Semicolon@1351..1352 ";"
              Blankspace@1352..1361 "\n        "
              VariableDeclaration@1361..1375
                Var@1361..1364 "var"
                Blankspace@1364..1365 " "
                Error@1365..1370
                  Reserved@1365..1370 "highp"
                Blankspace@1370..1371 " "
                Equal@1371..1372 "="
                Blankspace@1372..1373 " "
                Literal@1373..1374
                  IntLiteral@1373..1374 "0"
                Semicolon@1374..1375 ";"
              Blankspace@1375..1384 "\n        "
              VariableDeclaration@1384..1397
                Var@1384..1387 "var"
                Blankspace@1387..1388 " "
                Error@1388..1392
                  Reserved@1388..1392 "impl"
                Blankspace@1392..1393 " "
                Equal@1393..1394 "="
                Blankspace@1394..1395 " "
                Literal@1395..1396
                  IntLiteral@1395..1396 "0"
                Semicolon@1396..1397 ";"
              Blankspace@1397..1406 "\n        "
              VariableDeclaration@1406..1425
                Var@1406..1409 "var"
                Blankspace@1409..1410 " "
                Error@1410..1420
                  Reserved@1410..1420 "implements"
                Blankspace@1420..1421 " "
                Equal@1421..1422 "="
                Blankspace@1422..1423 " "
                Literal@1423..1424
                  IntLiteral@1423..1424 "0"
                Semicolon@1424..1425 ";"
              Blankspace@1425..1434 "\n        "
              VariableDeclaration@1434..1449
                Var@1434..1437 "var"
                Blankspace@1437..1438 " "
                Error@1438..1444
                  Reserved@1438..1444 "import"
                Blankspace@1444..1445 " "
                Equal@1445..1446 "="
                Blankspace@1446..1447 " "
                Literal@1447..1448
                  IntLiteral@1447..1448 "0"
                Semicolon@1448..1449 ";"
              Blankspace@1449..1458 "\n        "
              VariableDeclaration@1458..1473
                Var@1458..1461 "var"
                Blankspace@1461..1462 " "
                Error@1462..1468
                  Reserved@1462..1468 "inline"
                Blankspace@1468..1469 " "
                Equal@1469..1470 "="
                Blankspace@1470..1471 " "
                Literal@1471..1472
                  IntLiteral@1471..1472 "0"
                Semicolon@1472..1473 ";"
              Blankspace@1473..1482 "\n        "
              VariableDeclaration@1482..1501
                Var@1482..1485 "var"
                Blankspace@1485..1486 " "
                Error@1486..1496
                  Reserved@1486..1496 "instanceof"
                Blankspace@1496..1497 " "
                Equal@1497..1498 "="
                Blankspace@1498..1499 " "
                Literal@1499..1500
                  IntLiteral@1499..1500 "0"
                Semicolon@1500..1501 ";"
              Blankspace@1501..1510 "\n        "
              VariableDeclaration@1510..1528
                Var@1510..1513 "var"
                Blankspace@1513..1514 " "
                Error@1514..1523
                  Reserved@1514..1523 "interface"
                Blankspace@1523..1524 " "
                Equal@1524..1525 "="
                Blankspace@1525..1526 " "
                Literal@1526..1527
                  IntLiteral@1526..1527 "0"
                Semicolon@1527..1528 ";"
              Blankspace@1528..1537 "\n        "
              VariableDeclaration@1537..1552
                Var@1537..1540 "var"
                Blankspace@1540..1541 " "
                Error@1541..1547
                  Reserved@1541..1547 "layout"
                Blankspace@1547..1548 " "
                Equal@1548..1549 "="
                Blankspace@1549..1550 " "
                Literal@1550..1551
                  IntLiteral@1550..1551 "0"
                Semicolon@1551..1552 ";"
              Blankspace@1552..1561 "\n        "
              VariableDeclaration@1561..1574
                Var@1561..1564 "var"
                Blankspace@1564..1565 " "
                Error@1565..1569
                  Reserved@1565..1569 "lowp"
                Blankspace@1569..1570 " "
                Equal@1570..1571 "="
                Blankspace@1571..1572 " "
                Literal@1572..1573
                  IntLiteral@1572..1573 "0"
                Semicolon@1573..1574 ";"
              Blankspace@1574..1583 "\n        "
              VariableDeclaration@1583..1597
                Var@1583..1586 "var"
                Blankspace@1586..1587 " "
                Error@1587..1592
                  Reserved@1587..1592 "macro"
                Blankspace@1592..1593 " "
                Equal@1593..1594 "="
                Blankspace@1594..1595 " "
                Literal@1595..1596
                  IntLiteral@1595..1596 "0"
                Semicolon@1596..1597 ";"
              Blankspace@1597..1606 "\n        "
              VariableDeclaration@1606..1626
                Var@1606..1609 "var"
                Blankspace@1609..1610 " "
                Error@1610..1621
                  Reserved@1610..1621 "macro_rules"
                Blankspace@1621..1622 " "
                Equal@1622..1623 "="
                Blankspace@1623..1624 " "
                Literal@1624..1625
                  IntLiteral@1624..1625 "0"
                Semicolon@1625..1626 ";"
              Blankspace@1626..1635 "\n        "
              VariableDeclaration@1635..1649
                Var@1635..1638 "var"
                Blankspace@1638..1639 " "
                Error@1639..1644
                  Reserved@1639..1644 "match"
                Blankspace@1644..1645 " "
                Equal@1645..1646 "="
                Blankspace@1646..1647 " "
                Literal@1647..1648
                  IntLiteral@1647..1648 "0"
                Semicolon@1648..1649 ";"
              Blankspace@1649..1658 "\n        "
              VariableDeclaration@1658..1674
                Var@1658..1661 "var"
                Blankspace@1661..1662 " "
                Error@1662..1669
                  Reserved@1662..1669 "mediump"
                Blankspace@1669..1670 " "
                Equal@1670..1671 "="
                Blankspace@1671..1672 " "
                Literal@1672..1673
                  IntLiteral@1672..1673 "0"
                Semicolon@1673..1674 ";"
              Blankspace@1674..1683 "\n        "
              VariableDeclaration@1683..1696
                Var@1683..1686 "var"
                Blankspace@1686..1687 " "
                Error@1687..1691
                  Reserved@1687..1691 "meta"
                Blankspace@1691..1692 " "
                Equal@1692..1693 "="
                Blankspace@1693..1694 " "
                Literal@1694..1695
                  IntLiteral@1694..1695 "0"
                Semicolon@1695..1696 ";"
              Blankspace@1696..1705 "\n        "
              VariableDeclaration@1705..1717
                Var@1705..1708 "var"
                Blankspace@1708..1709 " "
                Error@1709..1712
                  Reserved@1709..1712 "mod"
                Blankspace@1712..1713 " "
                Equal@1713..1714 "="
                Blankspace@1714..1715 " "
                Literal@1715..1716
                  IntLiteral@1715..1716 "0"
                Semicolon@1716..1717 ";"
              Blankspace@1717..1726 "\n        "
              VariableDeclaration@1726..1741
                Var@1726..1729 "var"
                Blankspace@1729..1730 " "
                Error@1730..1736
                  Reserved@1730..1736 "module"
                Blankspace@1736..1737 " "
                Equal@1737..1738 "="
                Blankspace@1738..1739 " "
                Literal@1739..1740
                  IntLiteral@1739..1740 "0"
                Semicolon@1740..1741 ";"
              Blankspace@1741..1750 "\n        "
              VariableDeclaration@1750..1763
                Var@1750..1753 "var"
                Blankspace@1753..1754 " "
                Error@1754..1758
                  Reserved@1754..1758 "move"
                Blankspace@1758..1759 " "
                Equal@1759..1760 "="
                Blankspace@1760..1761 " "
                Literal@1761..1762
                  IntLiteral@1761..1762 "0"
                Semicolon@1762..1763 ";"
              Blankspace@1763..1772 "\n        "
              VariableDeclaration@1772..1784
                Var@1772..1775 "var"
                Blankspace@1775..1776 " "
                Error@1776..1779
                  Reserved@1776..1779 "mut"
                Blankspace@1779..1780 " "
                Equal@1780..1781 "="
                Blankspace@1781..1782 " "
                Literal@1782..1783
                  IntLiteral@1782..1783 "0"
                Semicolon@1783..1784 ";"
              Blankspace@1784..1793 "\n        "
              VariableDeclaration@1793..1809
                Var@1793..1796 "var"
                Blankspace@1796..1797 " "
                Error@1797..1804
                  Reserved@1797..1804 "mutable"
                Blankspace@1804..1805 " "
                Equal@1805..1806 "="
                Blankspace@1806..1807 " "
                Literal@1807..1808
                  IntLiteral@1807..1808 "0"
                Semicolon@1808..1809 ";"
              Blankspace@1809..1818 "\n        "
              VariableDeclaration@1818..1836
                Var@1818..1821 "var"
                Blankspace@1821..1822 " "
                Error@1822..1831
                  Reserved@1822..1831 "namespace"
                Blankspace@1831..1832 " "
                Equal@1832..1833 "="
                Blankspace@1833..1834 " "
                Literal@1834..1835
                  IntLiteral@1834..1835 "0"
                Semicolon@1835..1836 ";"
              Blankspace@1836..1845 "\n        "
              VariableDeclaration@1845..1857
                Var@1845..1848 "var"
                Blankspace@1848..1849 " "
                Error@1849..1852
                  Reserved@1849..1852 "new"
                Blankspace@1852..1853 " "
                Equal@1853..1854 "="
                Blankspace@1854..1855 " "
                Literal@1855..1856
                  IntLiteral@1855..1856 "0"
                Semicolon@1856..1857 ";"
              Blankspace@1857..1866 "\n        "
              VariableDeclaration@1866..1878
                Var@1866..1869 "var"
                Blankspace@1869..1870 " "
                Error@1870..1873
                  Reserved@1870..1873 "nil"
                Blankspace@1873..1874 " "
                Equal@1874..1875 "="
                Blankspace@1875..1876 " "
                Literal@1876..1877
                  IntLiteral@1876..1877 "0"
                Semicolon@1877..1878 ";"
              Blankspace@1878..1887 "\n        "
              VariableDeclaration@1887..1904
                Var@1887..1890 "var"
                Blankspace@1890..1891 " "
                Error@1891..1899
                  Reserved@1891..1899 "noexcept"
                Blankspace@1899..1900 " "
                Equal@1900..1901 "="
                Blankspace@1901..1902 " "
                Literal@1902..1903
                  IntLiteral@1902..1903 "0"
                Semicolon@1903..1904 ";"
              Blankspace@1904..1913 "\n        "
              VariableDeclaration@1913..1930
                Var@1913..1916 "var"
                Blankspace@1916..1917 " "
                Error@1917..1925
                  Reserved@1917..1925 "noinline"
                Blankspace@1925..1926 " "
                Equal@1926..1927 "="
                Blankspace@1927..1928 " "
                Literal@1928..1929
                  IntLiteral@1928..1929 "0"
                Semicolon@1929..1930 ";"
              Blankspace@1930..1939 "\n        "
              VariableDeclaration@1939..1963
                Var@1939..1942 "var"
                Blankspace@1942..1943 " "
                Error@1943..1958
                  Reserved@1943..1958 "nointerpolation"
                Blankspace@1958..1959 " "
                Equal@1959..1960 "="
                Blankspace@1960..1961 " "
                Literal@1961..1962
                  IntLiteral@1961..1962 "0"
                Semicolon@1962..1963 ";"
              Blankspace@1963..1972 "\n        "
              VariableDeclaration@1972..1993
                Var@1972..1975 "var"
                Blankspace@1975..1976 " "
                Error@1976..1988
                  Reserved@1976..1988 "non_coherent"
                Blankspace@1988..1989 " "
                Equal@1989..1990 "="
                Blankspace@1990..1991 " "
                Literal@1991..1992
                  IntLiteral@1991..1992 "0"
                Semicolon@1992..1993 ";"
              Blankspace@1993..2002 "\n        "
              VariableDeclaration@2002..2022
                Var@2002..2005 "var"
                Blankspace@2005..2006 " "
                Error@2006..2017
                  Reserved@2006..2017 "noncoherent"
                Blankspace@2017..2018 " "
                Equal@2018..2019 "="
                Blankspace@2019..2020 " "
                Literal@2020..2021
                  IntLiteral@2020..2021 "0"
                Semicolon@2021..2022 ";"
              Blankspace@2022..2031 "\n        "
              VariableDeclaration@2031..2053
                Var@2031..2034 "var"
                Blankspace@2034..2035 " "
                Error@2035..2048
                  Reserved@2035..2048 "noperspective"
                Blankspace@2048..2049 " "
                Equal@2049..2050 "="
                Blankspace@2050..2051 " "
                Literal@2051..2052
                  IntLiteral@2051..2052 "0"
                Semicolon@2052..2053 ";"
              Blankspace@2053..2062 "\n        "
              VariableDeclaration@2062..2075
                Var@2062..2065 "var"
                Blankspace@2065..2066 " "
                Error@2066..2070
                  Reserved@2066..2070 "null"
                Blankspace@2070..2071 " "
                Equal@2071..2072 "="
                Blankspace@2072..2073 " "
                Literal@2073..2074
                  IntLiteral@2073..2074 "0"
                Semicolon@2074..2075 ";"
              Blankspace@2075..2084 "\n        "
              VariableDeclaration@2084..2100
                Var@2084..2087 "var"
                Blankspace@2087..2088 " "
                Error@2088..2095
                  Reserved@2088..2095 "nullptr"
                Blankspace@2095..2096 " "
                Equal@2096..2097 "="
                Blankspace@2097..2098 " "
                Literal@2098..2099
                  IntLiteral@2098..2099 "0"
                Semicolon@2099..2100 ";"
              Blankspace@2100..2109 "\n        "
              VariableDeclaration@2109..2120
                Var@2109..2112 "var"
                Blankspace@2112..2113 " "
                Error@2113..2115
                  Reserved@2113..2115 "of"
                Blankspace@2115..2116 " "
                Equal@2116..2117 "="
                Blankspace@2117..2118 " "
                Literal@2118..2119
                  IntLiteral@2118..2119 "0"
                Semicolon@2119..2120 ";"
              Blankspace@2120..2129 "\n        "
              VariableDeclaration@2129..2146
                Var@2129..2132 "var"
                Blankspace@2132..2133 " "
                Error@2133..2141
                  Reserved@2133..2141 "operator"
                Blankspace@2141..2142 " "
                Equal@2142..2143 "="
                Blankspace@2143..2144 " "
                Literal@2144..2145
                  IntLiteral@2144..2145 "0"
                Semicolon@2145..2146 ";"
              Blankspace@2146..2155 "\n        "
              VariableDeclaration@2155..2171
                Var@2155..2158 "var"
                Blankspace@2158..2159 " "
                Error@2159..2166
                  Reserved@2159..2166 "package"
                Blankspace@2166..2167 " "
                Equal@2167..2168 "="
                Blankspace@2168..2169 " "
                Literal@2169..2170
                  IntLiteral@2169..2170 "0"
                Semicolon@2170..2171 ";"
              Blankspace@2171..2180 "\n        "
              VariableDeclaration@2180..2199
                Var@2180..2183 "var"
                Blankspace@2183..2184 " "
                Error@2184..2194
                  Reserved@2184..2194 "packoffset"
                Blankspace@2194..2195 " "
                Equal@2195..2196 "="
                Blankspace@2196..2197 " "
                Literal@2197..2198
                  IntLiteral@2197..2198 "0"
                Semicolon@2198..2199 ";"
              Blankspace@2199..2208 "\n        "
              VariableDeclaration@2208..2226
                Var@2208..2211 "var"
                Blankspace@2211..2212 " "
                Error@2212..2221
                  Reserved@2212..2221 "partition"
                Blankspace@2221..2222 " "
                Equal@2222..2223 "="
                Blankspace@2223..2224 " "
                Literal@2224..2225
                  IntLiteral@2224..2225 "0"
                Semicolon@2225..2226 ";"
              Blankspace@2226..2235 "\n        "
              VariableDeclaration@2235..2248
                Var@2235..2238 "var"
                Blankspace@2238..2239 " "
                Error@2239..2243
                  Reserved@2239..2243 "pass"
                Blankspace@2243..2244 " "
                Equal@2244..2245 "="
                Blankspace@2245..2246 " "
                Literal@2246..2247
                  IntLiteral@2246..2247 "0"
                Semicolon@2247..2248 ";"
              Blankspace@2248..2257 "\n        "
              VariableDeclaration@2257..2271
                Var@2257..2260 "var"
                Blankspace@2260..2261 " "
                Error@2261..2266
                  Reserved@2261..2266 "patch"
                Blankspace@2266..2267 " "
                Equal@2267..2268 "="
                Blankspace@2268..2269 " "
                Literal@2269..2270
                  IntLiteral@2269..2270 "0"
                Semicolon@2270..2271 ";"
              Blankspace@2271..2280 "\n        "
              VariableDeclaration@2280..2302
                Var@2280..2283 "var"
                Blankspace@2283..2284 " "
                Error@2284..2297
                  Reserved@2284..2297 "pixelfragment"
                Blankspace@2297..2298 " "
                Equal@2298..2299 "="
                Blankspace@2299..2300 " "
                Literal@2300..2301
                  IntLiteral@2300..2301 "0"
                Semicolon@2301..2302 ";"
              Blankspace@2302..2311 "\n        "
              VariableDeclaration@2311..2327
                Var@2311..2314 "var"
                Blankspace@2314..2315 " "
                Error@2315..2322
                  Reserved@2315..2322 "precise"
                Blankspace@2322..2323 " "
                Equal@2323..2324 "="
                Blankspace@2324..2325 " "
                Literal@2325..2326
                  IntLiteral@2325..2326 "0"
                Semicolon@2326..2327 ";"
              Blankspace@2327..2336 "\n        "
              VariableDeclaration@2336..2354
                Var@2336..2339 "var"
                Blankspace@2339..2340 " "
                Error@2340..2349
                  Reserved@2340..2349 "precision"
                Blankspace@2349..2350 " "
                Equal@2350..2351 "="
                Blankspace@2351..2352 " "
                Literal@2352..2353
                  IntLiteral@2352..2353 "0"
                Semicolon@2353..2354 ";"
              Blankspace@2354..2363 "\n        "
              VariableDeclaration@2363..2380
                Var@2363..2366 "var"
                Blankspace@2366..2367 " "
                Error@2367..2375
                  Reserved@2367..2375 "premerge"
                Blankspace@2375..2376 " "
                Equal@2376..2377 "="
                Blankspace@2377..2378 " "
                Literal@2378..2379
                  IntLiteral@2378..2379 "0"
                Semicolon@2379..2380 ";"
              Blankspace@2380..2389 "\n        "
              VariableDeclaration@2389..2402
                Var@2389..2392 "var"
                Blankspace@2392..2393 " "
                Error@2393..2397
                  Reserved@2393..2397 "priv"
                Blankspace@2397..2398 " "
                Equal@2398..2399 "="
                Blankspace@2399..2400 " "
                Literal@2400..2401
                  IntLiteral@2400..2401 "0"
                Semicolon@2401..2402 ";"
              Blankspace@2402..2411 "\n        "
              VariableDeclaration@2411..2429
                Var@2411..2414 "var"
                Blankspace@2414..2415 " "
                Error@2415..2424
                  Reserved@2415..2424 "protected"
                Blankspace@2424..2425 " "
                Equal@2425..2426 "="
                Blankspace@2426..2427 " "
                Literal@2427..2428
                  IntLiteral@2427..2428 "0"
                Semicolon@2428..2429 ";"
              Blankspace@2429..2438 "\n        "
              VariableDeclaration@2438..2450
                Var@2438..2441 "var"
                Blankspace@2441..2442 " "
                Error@2442..2445
                  Reserved@2442..2445 "pub"
                Blankspace@2445..2446 " "
                Equal@2446..2447 "="
                Blankspace@2447..2448 " "
                Literal@2448..2449
                  IntLiteral@2448..2449 "0"
                Semicolon@2449..2450 ";"
              Blankspace@2450..2459 "\n        "
              VariableDeclaration@2459..2474
                Var@2459..2462 "var"
                Blankspace@2462..2463 " "
                Error@2463..2469
                  Reserved@2463..2469 "public"
                Blankspace@2469..2470 " "
                Equal@2470..2471 "="
                Blankspace@2471..2472 " "
                Literal@2472..2473
                  IntLiteral@2472..2473 "0"
                Semicolon@2473..2474 ";"
              Blankspace@2474..2483 "\n        "
              VariableDeclaration@2483..2500
                Var@2483..2486 "var"
                Blankspace@2486..2487 " "
                Error@2487..2495
                  Reserved@2487..2495 "readonly"
                Blankspace@2495..2496 " "
                Equal@2496..2497 "="
                Blankspace@2497..2498 " "
                Literal@2498..2499
                  IntLiteral@2498..2499 "0"
                Semicolon@2499..2500 ";"
              Blankspace@2500..2509 "\n        "
              VariableDeclaration@2509..2521
                Var@2509..2512 "var"
                Blankspace@2512..2513 " "
                Error@2513..2516
                  Reserved@2513..2516 "ref"
                Blankspace@2516..2517 " "
                Equal@2517..2518 "="
                Blankspace@2518..2519 " "
                Literal@2519..2520
                  IntLiteral@2519..2520 "0"
                Semicolon@2520..2521 ";"
              Blankspace@2521..2530 "\n        "
              VariableDeclaration@2530..2549
                Var@2530..2533 "var"
                Blankspace@2533..2534 " "
                Error@2534..2544
                  Reserved@2534..2544 "regardless"
                Blankspace@2544..2545 " "
                Equal@2545..2546 "="
                Blankspace@2546..2547 " "
                Literal@2547..2548
                  IntLiteral@2547..2548 "0"
                Semicolon@2548..2549 ";"
              Blankspace@2549..2558 "\n        "
              VariableDeclaration@2558..2575
                Var@2558..2561 "var"
                Blankspace@2561..2562 " "
                Error@2562..2570
                  Reserved@2562..2570 "register"
                Blankspace@2570..2571 " "
                Equal@2571..2572 "="
                Blankspace@2572..2573 " "
                Literal@2573..2574
                  IntLiteral@2573..2574 "0"
                Semicolon@2574..2575 ";"
              Blankspace@2575..2584 "\n        "
              VariableDeclaration@2584..2609
                Var@2584..2587 "var"
                Blankspace@2587..2588 " "
                Error@2588..2604
                  Reserved@2588..2604 "reinterpret_cast"
                Blankspace@2604..2605 " "
                Equal@2605..2606 "="
                Blankspace@2606..2607 " "
                Literal@2607..2608
                  IntLiteral@2607..2608 "0"
                Semicolon@2608..2609 ";"
              Blankspace@2609..2618 "\n        "
              VariableDeclaration@2618..2634
                Var@2618..2621 "var"
                Blankspace@2621..2622 " "
                Error@2622..2629
                  Reserved@2622..2629 "require"
                Blankspace@2629..2630 " "
                Equal@2630..2631 "="
                Blankspace@2631..2632 " "
                Literal@2632..2633
                  IntLiteral@2632..2633 "0"
                Semicolon@2633..2634 ";"
              Blankspace@2634..2643 "\n        "
              VariableDeclaration@2643..2660
                Var@2643..2646 "var"
                Blankspace@2646..2647 " "
                Error@2647..2655
                  Reserved@2647..2655 "resource"
                Blankspace@2655..2656 " "
                Equal@2656..2657 "="
                Blankspace@2657..2658 " "
                Literal@2658..2659
                  IntLiteral@2658..2659 "0"
                Semicolon@2659..2660 ";"
              Blankspace@2660..2669 "\n        "
              VariableDeclaration@2669..2686
                Var@2669..2672 "var"
                Blankspace@2672..2673 " "
                Error@2673..2681
                  Reserved@2673..2681 "restrict"
                Blankspace@2681..2682 " "
                Equal@2682..2683 "="
                Blankspace@2683..2684 " "
                Literal@2684..2685
                  IntLiteral@2684..2685 "0"
                Semicolon@2685..2686 ";"
              Blankspace@2686..2695 "\n        "
              VariableDeclaration@2695..2708
                Var@2695..2698 "var"
                Blankspace@2698..2699 " "
                Error@2699..2703
                  Reserved@2699..2703 "self"
                Blankspace@2703..2704 " "
                Equal@2704..2705 "="
                Blankspace@2705..2706 " "
                Literal@2706..2707
                  IntLiteral@2706..2707 "0"
                Semicolon@2707..2708 ";"
              Blankspace@2708..2717 "\n        "
              VariableDeclaration@2717..2729
                Var@2717..2720 "var"
                Blankspace@2720..2721 " "
                Error@2721..2724
                  Reserved@2721..2724 "set"
                Blankspace@2724..2725 " "
                Equal@2725..2726 "="
                Blankspace@2726..2727 " "
                Literal@2727..2728
                  IntLiteral@2727..2728 "0"
                Semicolon@2728..2729 ";"
              Blankspace@2729..2738 "\n        "
              VariableDeclaration@2738..2753
                Var@2738..2741 "var"
                Blankspace@2741..2742 " "
                Error@2742..2748
                  Reserved@2742..2748 "shared"
                Blankspace@2748..2749 " "
                Equal@2749..2750 "="
                Blankspace@2750..2751 " "
                Literal@2751..2752
                  IntLiteral@2751..2752 "0"
                Semicolon@2752..2753 ";"
              Blankspace@2753..2762 "\n        "
              VariableDeclaration@2762..2777
                Var@2762..2765 "var"
                Blankspace@2765..2766 " "
                Error@2766..2772
                  Reserved@2766..2772 "sizeof"
                Blankspace@2772..2773 " "
                Equal@2773..2774 "="
                Blankspace@2774..2775 " "
                Literal@2775..2776
                  IntLiteral@2775..2776 "0"
                Semicolon@2776..2777 ";"
              Blankspace@2777..2786 "\n        "
              VariableDeclaration@2786..2801
                Var@2786..2789 "var"
                Blankspace@2789..2790 " "
                Error@2790..2796
                  Reserved@2790..2796 "smooth"
                Blankspace@2796..2797 " "
                Equal@2797..2798 "="
                Blankspace@2798..2799 " "
                Literal@2799..2800
                  IntLiteral@2799..2800 "0"
                Semicolon@2800..2801 ";"
              Blankspace@2801..2810 "\n        "
              VariableDeclaration@2810..2824
                Var@2810..2813 "var"
                Blankspace@2813..2814 " "
                Error@2814..2819
                  Reserved@2814..2819 "snorm"
                Blankspace@2819..2820 " "
                Equal@2820..2821 "="
                Blankspace@2821..2822 " "
                Literal@2822..2823
                  IntLiteral@2822..2823 "0"
                Semicolon@2823..2824 ";"
              Blankspace@2824..2833 "\n        "
              VariableDeclaration@2833..2848
                Var@2833..2836 "var"
                Blankspace@2836..2837 " "
                Error@2837..2843
                  Reserved@2837..2843 "static"
                Blankspace@2843..2844 " "
                Equal@2844..2845 "="
                Blankspace@2845..2846 " "
                Literal@2846..2847
                  IntLiteral@2846..2847 "0"
                Semicolon@2847..2848 ";"
              Blankspace@2848..2857 "\n        "
              VariableDeclaration@2857..2879
                Var@2857..2860 "var"
                Blankspace@2860..2861 " "
                Error@2861..2874
                  Reserved@2861..2874 "static_assert"
                Blankspace@2874..2875 " "
                Equal@2875..2876 "="
                Blankspace@2876..2877 " "
                Literal@2877..2878
                  IntLiteral@2877..2878 "0"
                Semicolon@2878..2879 ";"
              Blankspace@2879..2888 "\n        "
              VariableDeclaration@2888..2908
                Var@2888..2891 "var"
                Blankspace@2891..2892 " "
                Error@2892..2903
                  Reserved@2892..2903 "static_cast"
                Blankspace@2903..2904 " "
                Equal@2904..2905 "="
                Blankspace@2905..2906 " "
                Literal@2906..2907
                  IntLiteral@2906..2907 "0"
                Semicolon@2907..2908 ";"
              Blankspace@2908..2917 "\n        "
              VariableDeclaration@2917..2929
                Var@2917..2920 "var"
                Blankspace@2920..2921 " "
                Error@2921..2924
                  Reserved@2921..2924 "std"
                Blankspace@2924..2925 " "
                Equal@2925..2926 "="
                Blankspace@2926..2927 " "
                Literal@2927..2928
                  IntLiteral@2927..2928 "0"
                Semicolon@2928..2929 ";"
              Blankspace@2929..2938 "\n        "
              VariableDeclaration@2938..2957
                Var@2938..2941 "var"
                Blankspace@2941..2942 " "
                Error@2942..2952
                  Reserved@2942..2952 "subroutine"
                Blankspace@2952..2953 " "
                Equal@2953..2954 "="
                Blankspace@2954..2955 " "
                Literal@2955..2956
                  IntLiteral@2955..2956 "0"
                Semicolon@2956..2957 ";"
              Blankspace@2957..2966 "\n        "
              VariableDeclaration@2966..2980
                Var@2966..2969 "var"
                Blankspace@2969..2970 " "
                Error@2970..2975
                  Reserved@2970..2975 "super"
                Blankspace@2975..2976 " "
                Equal@2976..2977 "="
                Blankspace@2977..2978 " "
                Literal@2978..2979
                  IntLiteral@2978..2979 "0"
                Semicolon@2979..2980 ";"
              Blankspace@2980..2989 "\n        "
              VariableDeclaration@2989..3004
                Var@2989..2992 "var"
                Blankspace@2992..2993 " "
                Error@2993..2999
                  Reserved@2993..2999 "target"
                Blankspace@2999..3000 " "
                Equal@3000..3001 "="
                Blankspace@3001..3002 " "
                Literal@3002..3003
                  IntLiteral@3002..3003 "0"
                Semicolon@3003..3004 ";"
              Blankspace@3004..3013 "\n        "
              VariableDeclaration@3013..3030
                Var@3013..3016 "var"
                Blankspace@3016..3017 " "
                Error@3017..3025
                  Reserved@3017..3025 "template"
                Blankspace@3025..3026 " "
                Equal@3026..3027 "="
                Blankspace@3027..3028 " "
                Literal@3028..3029
                  IntLiteral@3028..3029 "0"
                Semicolon@3029..3030 ";"
              Blankspace@3030..3039 "\n        "
              VariableDeclaration@3039..3052
                Var@3039..3042 "var"
                Blankspace@3042..3043 " "
                Error@3043..3047
                  Reserved@3043..3047 "this"
                Blankspace@3047..3048 " "
                Equal@3048..3049 "="
                Blankspace@3049..3050 " "
                Literal@3050..3051
                  IntLiteral@3050..3051 "0"
                Semicolon@3051..3052 ";"
              Blankspace@3052..3061 "\n        "
              VariableDeclaration@3061..3082
                Var@3061..3064 "var"
                Blankspace@3064..3065 " "
                Error@3065..3077
                  Reserved@3065..3077 "thread_local"
                Blankspace@3077..3078 " "
                Equal@3078..3079 "="
                Blankspace@3079..3080 " "
                Literal@3080..3081
                  IntLiteral@3080..3081 "0"
                Semicolon@3081..3082 ";"
              Blankspace@3082..3091 "\n        "
              VariableDeclaration@3091..3105
                Var@3091..3094 "var"
                Blankspace@3094..3095 " "
                Error@3095..3100
                  Reserved@3095..3100 "throw"
                Blankspace@3100..3101 " "
                Equal@3101..3102 "="
                Blankspace@3102..3103 " "
                Literal@3103..3104
                  IntLiteral@3103..3104 "0"
                Semicolon@3104..3105 ";"
              Blankspace@3105..3114 "\n        "
              VariableDeclaration@3114..3128
                Var@3114..3117 "var"
                Blankspace@3117..3118 " "
                Error@3118..3123
                  Reserved@3118..3123 "trait"
                Blankspace@3123..3124 " "
                Equal@3124..3125 "="
                Blankspace@3125..3126 " "
                Literal@3126..3127
                  IntLiteral@3126..3127 "0"
                Semicolon@3127..3128 ";"
              Blankspace@3128..3137 "\n        "
              VariableDeclaration@3137..3149
                Var@3137..3140 "var"
                Blankspace@3140..3141 " "
                Error@3141..3144
                  Reserved@3141..3144 "try"
                Blankspace@3144..3145 " "
                Equal@3145..3146 "="
                Blankspace@3146..3147 " "
                Literal@3147..3148
                  IntLiteral@3147..3148 "0"
                Semicolon@3148..3149 ";"
              Blankspace@3149..3158 "\n        "
              VariableDeclaration@3158..3171
                Var@3158..3161 "var"
                Blankspace@3161..3162 " "
                Error@3162..3166
                  Reserved@3162..3166 "type"
                Blankspace@3166..3167 " "
                Equal@3167..3168 "="
                Blankspace@3168..3169 " "
                Literal@3169..3170
                  IntLiteral@3169..3170 "0"
                Semicolon@3170..3171 ";"
              Blankspace@3171..3180 "\n        "
              VariableDeclaration@3180..3196
                Var@3180..3183 "var"
                Blankspace@3183..3184 " "
                Error@3184..3191
                  Reserved@3184..3191 "typedef"
                Blankspace@3191..3192 " "
                Equal@3192..3193 "="
                Blankspace@3193..3194 " "
                Literal@3194..3195
                  IntLiteral@3194..3195 "0"
                Semicolon@3195..3196 ";"
              Blankspace@3196..3205 "\n        "
              VariableDeclaration@3205..3220
                Var@3205..3208 "var"
                Blankspace@3208..3209 " "
                Error@3209..3215
                  Reserved@3209..3215 "typeid"
                Blankspace@3215..3216 " "
                Equal@3216..3217 "="
                Blankspace@3217..3218 " "
                Literal@3218..3219
                  IntLiteral@3218..3219 "0"
                Semicolon@3219..3220 ";"
              Blankspace@3220..3229 "\n        "
              VariableDeclaration@3229..3246
                Var@3229..3232 "var"
                Blankspace@3232..3233 " "
                Error@3233..3241
                  Reserved@3233..3241 "typename"
                Blankspace@3241..3242 " "
                Equal@3242..3243 "="
                Blankspace@3243..3244 " "
                Literal@3244..3245
                  IntLiteral@3244..3245 "0"
                Semicolon@3245..3246 ";"
              Blankspace@3246..3255 "\n        "
              VariableDeclaration@3255..3270
                Var@3255..3258 "var"
                Blankspace@3258..3259 " "
                Error@3259..3265
                  Reserved@3259..3265 "typeof"
                Blankspace@3265..3266 " "
                Equal@3266..3267 "="
                Blankspace@3267..3268 " "
                Literal@3268..3269
                  IntLiteral@3268..3269 "0"
                Semicolon@3269..3270 ";"
              Blankspace@3270..3279 "\n        "
              VariableDeclaration@3279..3293
                Var@3279..3282 "var"
                Blankspace@3282..3283 " "
                Error@3283..3288
                  Reserved@3283..3288 "union"
                Blankspace@3288..3289 " "
                Equal@3289..3290 "="
                Blankspace@3290..3291 " "
                Literal@3291..3292
                  IntLiteral@3291..3292 "0"
                Semicolon@3292..3293 ";"
              Blankspace@3293..3302 "\n        "
              VariableDeclaration@3302..3317
                Var@3302..3305 "var"
                Blankspace@3305..3306 " "
                Error@3306..3312
                  Reserved@3306..3312 "unless"
                Blankspace@3312..3313 " "
                Equal@3313..3314 "="
                Blankspace@3314..3315 " "
                Literal@3315..3316
                  IntLiteral@3315..3316 "0"
                Semicolon@3316..3317 ";"
              Blankspace@3317..3326 "\n        "
              VariableDeclaration@3326..3340
                Var@3326..3329 "var"
                Blankspace@3329..3330 " "
                Error@3330..3335
                  Reserved@3330..3335 "unorm"
                Blankspace@3335..3336 " "
                Equal@3336..3337 "="
                Blankspace@3337..3338 " "
                Literal@3338..3339
                  IntLiteral@3338..3339 "0"
                Semicolon@3339..3340 ";"
              Blankspace@3340..3349 "\n        "
              VariableDeclaration@3349..3364
                Var@3349..3352 "var"
                Blankspace@3352..3353 " "
                Error@3353..3359
                  Reserved@3353..3359 "unsafe"
                Blankspace@3359..3360 " "
                Equal@3360..3361 "="
                Blankspace@3361..3362 " "
                Literal@3362..3363
                  IntLiteral@3362..3363 "0"
                Semicolon@3363..3364 ";"
              Blankspace@3364..3373 "\n        "
              VariableDeclaration@3373..3389
                Var@3373..3376 "var"
                Blankspace@3376..3377 " "
                Error@3377..3384
                  Reserved@3377..3384 "unsized"
                Blankspace@3384..3385 " "
                Equal@3385..3386 "="
                Blankspace@3386..3387 " "
                Literal@3387..3388
                  IntLiteral@3387..3388 "0"
                Semicolon@3388..3389 ";"
              Blankspace@3389..3398 "\n        "
              VariableDeclaration@3398..3410
                Var@3398..3401 "var"
                Blankspace@3401..3402 " "
                Error@3402..3405
                  Reserved@3402..3405 "use"
                Blankspace@3405..3406 " "
                Equal@3406..3407 "="
                Blankspace@3407..3408 " "
                Literal@3408..3409
                  IntLiteral@3408..3409 "0"
                Semicolon@3409..3410 ";"
              Blankspace@3410..3419 "\n        "
              VariableDeclaration@3419..3433
                Var@3419..3422 "var"
                Blankspace@3422..3423 " "
                Error@3423..3428
                  Reserved@3423..3428 "using"
                Blankspace@3428..3429 " "
                Equal@3429..3430 "="
                Blankspace@3430..3431 " "
                Literal@3431..3432
                  IntLiteral@3431..3432 "0"
                Semicolon@3432..3433 ";"
              Blankspace@3433..3442 "\n        "
              VariableDeclaration@3442..3458
                Var@3442..3445 "var"
                Blankspace@3445..3446 " "
                Error@3446..3453
                  Reserved@3446..3453 "varying"
                Blankspace@3453..3454 " "
                Equal@3454..3455 "="
                Blankspace@3455..3456 " "
                Literal@3456..3457
                  IntLiteral@3456..3457 "0"
                Semicolon@3457..3458 ";"
              Blankspace@3458..3467 "\n        "
              VariableDeclaration@3467..3483
                Var@3467..3470 "var"
                Blankspace@3470..3471 " "
                Error@3471..3478
                  Reserved@3471..3478 "virtual"
                Blankspace@3478..3479 " "
                Equal@3479..3480 "="
                Blankspace@3480..3481 " "
                Literal@3481..3482
                  IntLiteral@3481..3482 "0"
                Semicolon@3482..3483 ";"
              Blankspace@3483..3492 "\n        "
              VariableDeclaration@3492..3509
                Var@3492..3495 "var"
                Blankspace@3495..3496 " "
                Error@3496..3504
                  Reserved@3496..3504 "volatile"
                Blankspace@3504..3505 " "
                Equal@3505..3506 "="
                Blankspace@3506..3507 " "
                Literal@3507..3508
                  IntLiteral@3507..3508 "0"
                Semicolon@3508..3509 ";"
              Blankspace@3509..3518 "\n        "
              VariableDeclaration@3518..3531
                Var@3518..3521 "var"
                Blankspace@3521..3522 " "
                Error@3522..3526
                  Reserved@3522..3526 "wgsl"
                Blankspace@3526..3527 " "
                Equal@3527..3528 "="
                Blankspace@3528..3529 " "
                Literal@3529..3530
                  IntLiteral@3529..3530 "0"
                Semicolon@3530..3531 ";"
              Blankspace@3531..3540 "\n        "
              VariableDeclaration@3540..3554
                Var@3540..3543 "var"
                Blankspace@3543..3544 " "
                Error@3544..3549
                  Reserved@3544..3549 "where"
                Blankspace@3549..3550 " "
                Equal@3550..3551 "="
                Blankspace@3551..3552 " "
                Literal@3552..3553
                  IntLiteral@3552..3553 "0"
                Semicolon@3553..3554 ";"
              Blankspace@3554..3563 "\n        "
              VariableDeclaration@3563..3576
                Var@3563..3566 "var"
                Blankspace@3566..3567 " "
                Error@3567..3571
                  Reserved@3567..3571 "with"
                Blankspace@3571..3572 " "
                Equal@3572..3573 "="
                Blankspace@3573..3574 " "
                Literal@3574..3575
                  IntLiteral@3574..3575 "0"
                Semicolon@3575..3576 ";"
              Blankspace@3576..3585 "\n        "
              VariableDeclaration@3585..3603
                Var@3585..3588 "var"
                Blankspace@3588..3589 " "
                Error@3589..3598
                  Reserved@3589..3598 "writeonly"
                Blankspace@3598..3599 " "
                Equal@3599..3600 "="
                Blankspace@3600..3601 " "
                Literal@3601..3602
                  IntLiteral@3601..3602 "0"
                Semicolon@3602..3603 ";"
              Blankspace@3603..3612 "\n        "
              VariableDeclaration@3612..3626
                Var@3612..3615 "var"
                Blankspace@3615..3616 " "
                Error@3616..3621
                  Reserved@3616..3621 "yield"
                Blankspace@3621..3622 " "
                Equal@3622..3623 "="
                Blankspace@3623..3624 " "
                Literal@3624..3625
                  IntLiteral@3624..3625 "0"
                Semicolon@3625..3626 ";"
              Blankspace@3626..3635 "\n        "

            error at 156..159: import statements are not allowed in WGSL mode
            error at 1437..1444: import statements are not allowed in WGSL mode
            error at 2158..2166: import statements are not allowed in WGSL mode
            error at 2969..2975: import statements are not allowed in WGSL mode
            error at 13..17: 'NULL' is a reserved word in WGSL
            error at 18..19: invalid syntax, expected: <identifier>
            error at 35..39: 'Self' is a reserved word in WGSL
            error at 40..41: invalid syntax, expected: <identifier>
            error at 57..65: 'abstract' is a reserved word in WGSL
            error at 66..67: invalid syntax, expected: <identifier>
            error at 83..89: 'active' is a reserved word in WGSL
            error at 90..91: invalid syntax, expected: <identifier>
            error at 107..114: 'alignas' is a reserved word in WGSL
            error at 115..116: invalid syntax, expected: <identifier>
            error at 132..139: 'alignof' is a reserved word in WGSL
            error at 140..141: invalid syntax, expected: <identifier>
            error at 157..159: 'as' is a reserved word in WGSL
            error at 160..161: invalid syntax, expected: <identifier>
            error at 177..180: 'asm' is a reserved word in WGSL
            error at 181..182: invalid syntax, expected: <identifier>
            error at 198..210: 'asm_fragment' is a reserved word in WGSL
            error at 211..212: invalid syntax, expected: <identifier>
            error at 228..233: 'async' is a reserved word in WGSL
            error at 234..235: invalid syntax, expected: <identifier>
            error at 251..260: 'attribute' is a reserved word in WGSL
            error at 261..262: invalid syntax, expected: <identifier>
            error at 278..282: 'auto' is a reserved word in WGSL
            error at 283..284: invalid syntax, expected: <identifier>
            error at 300..305: 'await' is a reserved word in WGSL
            error at 306..307: invalid syntax, expected: <identifier>
            error at 323..329: 'become' is a reserved word in WGSL
            error at 330..331: invalid syntax, expected: <identifier>
            error at 347..351: 'cast' is a reserved word in WGSL
            error at 352..353: invalid syntax, expected: <identifier>
            error at 369..374: 'catch' is a reserved word in WGSL
            error at 375..376: invalid syntax, expected: <identifier>
            error at 392..397: 'class' is a reserved word in WGSL
            error at 398..399: invalid syntax, expected: <identifier>
            error at 415..423: 'co_await' is a reserved word in WGSL
            error at 424..425: invalid syntax, expected: <identifier>
            error at 441..450: 'co_return' is a reserved word in WGSL
            error at 451..452: invalid syntax, expected: <identifier>
            error at 468..476: 'co_yield' is a reserved word in WGSL
            error at 477..478: invalid syntax, expected: <identifier>
            error at 494..502: 'coherent' is a reserved word in WGSL
            error at 503..504: invalid syntax, expected: <identifier>
            error at 520..532: 'column_major' is a reserved word in WGSL
            error at 533..534: invalid syntax, expected: <identifier>
            error at 550..556: 'common' is a reserved word in WGSL
            error at 557..558: invalid syntax, expected: <identifier>
            error at 574..581: 'compile' is a reserved word in WGSL
            error at 582..583: invalid syntax, expected: <identifier>
            error at 599..615: 'compile_fragment' is a reserved word in WGSL
            error at 616..617: invalid syntax, expected: <identifier>
            error at 633..640: 'concept' is a reserved word in WGSL
            error at 641..642: invalid syntax, expected: <identifier>
            error at 658..668: 'const_cast' is a reserved word in WGSL
            error at 669..670: invalid syntax, expected: <identifier>
            error at 686..695: 'consteval' is a reserved word in WGSL
            error at 696..697: invalid syntax, expected: <identifier>
            error at 713..722: 'constexpr' is a reserved word in WGSL
            error at 723..724: invalid syntax, expected: <identifier>
            error at 740..749: 'constinit' is a reserved word in WGSL
            error at 750..751: invalid syntax, expected: <identifier>
            error at 767..772: 'crate' is a reserved word in WGSL
            error at 773..774: invalid syntax, expected: <identifier>
            error at 790..798: 'debugger' is a reserved word in WGSL
            error at 799..800: invalid syntax, expected: <identifier>
            error at 816..824: 'decltype' is a reserved word in WGSL
            error at 825..826: invalid syntax, expected: <identifier>
            error at 842..848: 'delete' is a reserved word in WGSL
            error at 849..850: invalid syntax, expected: <identifier>
            error at 866..872: 'demote' is a reserved word in WGSL
            error at 873..874: invalid syntax, expected: <identifier>
            error at 890..906: 'demote_to_helper' is a reserved word in WGSL
            error at 907..908: invalid syntax, expected: <identifier>
            error at 924..926: 'do' is a reserved word in WGSL
            error at 927..928: invalid syntax, expected: <identifier>
            error at 944..956: 'dynamic_cast' is a reserved word in WGSL
            error at 957..958: invalid syntax, expected: <identifier>
            error at 974..978: 'enum' is a reserved word in WGSL
            error at 979..980: invalid syntax, expected: <identifier>
            error at 996..1004: 'explicit' is a reserved word in WGSL
            error at 1005..1006: invalid syntax, expected: <identifier>
            error at 1022..1028: 'export' is a reserved word in WGSL
            error at 1029..1030: invalid syntax, expected: <identifier>
            error at 1046..1053: 'extends' is a reserved word in WGSL
            error at 1054..1055: invalid syntax, expected: <identifier>
            error at 1071..1077: 'extern' is a reserved word in WGSL
            error at 1078..1079: invalid syntax, expected: <identifier>
            error at 1095..1103: 'external' is a reserved word in WGSL
            error at 1104..1105: invalid syntax, expected: <identifier>
            error at 1121..1132: 'fallthrough' is a reserved word in WGSL
            error at 1133..1134: invalid syntax, expected: <identifier>
            error at 1150..1156: 'filter' is a reserved word in WGSL
            error at 1157..1158: invalid syntax, expected: <identifier>
            error at 1174..1179: 'final' is a reserved word in WGSL
            error at 1180..1181: invalid syntax, expected: <identifier>
            error at 1197..1204: 'finally' is a reserved word in WGSL
            error at 1205..1206: invalid syntax, expected: <identifier>
            error at 1222..1228: 'friend' is a reserved word in WGSL
            error at 1229..1230: invalid syntax, expected: <identifier>
            error at 1246..1250: 'from' is a reserved word in WGSL
            error at 1251..1252: invalid syntax, expected: <identifier>
            error at 1268..1275: 'fxgroup' is a reserved word in WGSL
            error at 1276..1277: invalid syntax, expected: <identifier>
            error at 1293..1296: 'get' is a reserved word in WGSL
            error at 1297..1298: invalid syntax, expected: <identifier>
            error at 1314..1318: 'goto' is a reserved word in WGSL
            error at 1319..1320: invalid syntax, expected: <identifier>
            error at 1336..1347: 'groupshared' is a reserved word in WGSL
            error at 1348..1349: invalid syntax, expected: <identifier>
            error at 1365..1370: 'highp' is a reserved word in WGSL
            error at 1371..1372: invalid syntax, expected: <identifier>
            error at 1388..1392: 'impl' is a reserved word in WGSL
            error at 1393..1394: invalid syntax, expected: <identifier>
            error at 1410..1420: 'implements' is a reserved word in WGSL
            error at 1421..1422: invalid syntax, expected: <identifier>
            error at 1438..1444: 'import' is a reserved word in WGSL
            error at 1445..1446: invalid syntax, expected: <identifier>
            error at 1462..1468: 'inline' is a reserved word in WGSL
            error at 1469..1470: invalid syntax, expected: <identifier>
            error at 1486..1496: 'instanceof' is a reserved word in WGSL
            error at 1497..1498: invalid syntax, expected: <identifier>
            error at 1514..1523: 'interface' is a reserved word in WGSL
            error at 1524..1525: invalid syntax, expected: <identifier>
            error at 1541..1547: 'layout' is a reserved word in WGSL
            error at 1548..1549: invalid syntax, expected: <identifier>
            error at 1565..1569: 'lowp' is a reserved word in WGSL
            error at 1570..1571: invalid syntax, expected: <identifier>
            error at 1587..1592: 'macro' is a reserved word in WGSL
            error at 1593..1594: invalid syntax, expected: <identifier>
            error at 1610..1621: 'macro_rules' is a reserved word in WGSL
            error at 1622..1623: invalid syntax, expected: <identifier>
            error at 1639..1644: 'match' is a reserved word in WGSL
            error at 1645..1646: invalid syntax, expected: <identifier>
            error at 1662..1669: 'mediump' is a reserved word in WGSL
            error at 1670..1671: invalid syntax, expected: <identifier>
            error at 1687..1691: 'meta' is a reserved word in WGSL
            error at 1692..1693: invalid syntax, expected: <identifier>
            error at 1709..1712: 'mod' is a reserved word in WGSL
            error at 1713..1714: invalid syntax, expected: <identifier>
            error at 1730..1736: 'module' is a reserved word in WGSL
            error at 1737..1738: invalid syntax, expected: <identifier>
            error at 1754..1758: 'move' is a reserved word in WGSL
            error at 1759..1760: invalid syntax, expected: <identifier>
            error at 1776..1779: 'mut' is a reserved word in WGSL
            error at 1780..1781: invalid syntax, expected: <identifier>
            error at 1797..1804: 'mutable' is a reserved word in WGSL
            error at 1805..1806: invalid syntax, expected: <identifier>
            error at 1822..1831: 'namespace' is a reserved word in WGSL
            error at 1832..1833: invalid syntax, expected: <identifier>
            error at 1849..1852: 'new' is a reserved word in WGSL
            error at 1853..1854: invalid syntax, expected: <identifier>
            error at 1870..1873: 'nil' is a reserved word in WGSL
            error at 1874..1875: invalid syntax, expected: <identifier>
            error at 1891..1899: 'noexcept' is a reserved word in WGSL
            error at 1900..1901: invalid syntax, expected: <identifier>
            error at 1917..1925: 'noinline' is a reserved word in WGSL
            error at 1926..1927: invalid syntax, expected: <identifier>
            error at 1943..1958: 'nointerpolation' is a reserved word in WGSL
            error at 1959..1960: invalid syntax, expected: <identifier>
            error at 1976..1988: 'non_coherent' is a reserved word in WGSL
            error at 1989..1990: invalid syntax, expected: <identifier>
            error at 2006..2017: 'noncoherent' is a reserved word in WGSL
            error at 2018..2019: invalid syntax, expected: <identifier>
            error at 2035..2048: 'noperspective' is a reserved word in WGSL
            error at 2049..2050: invalid syntax, expected: <identifier>
            error at 2066..2070: 'null' is a reserved word in WGSL
            error at 2071..2072: invalid syntax, expected: <identifier>
            error at 2088..2095: 'nullptr' is a reserved word in WGSL
            error at 2096..2097: invalid syntax, expected: <identifier>
            error at 2113..2115: 'of' is a reserved word in WGSL
            error at 2116..2117: invalid syntax, expected: <identifier>
            error at 2133..2141: 'operator' is a reserved word in WGSL
            error at 2142..2143: invalid syntax, expected: <identifier>
            error at 2159..2166: 'package' is a reserved word in WGSL
            error at 2167..2168: invalid syntax, expected: <identifier>
            error at 2184..2194: 'packoffset' is a reserved word in WGSL
            error at 2195..2196: invalid syntax, expected: <identifier>
            error at 2212..2221: 'partition' is a reserved word in WGSL
            error at 2222..2223: invalid syntax, expected: <identifier>
            error at 2239..2243: 'pass' is a reserved word in WGSL
            error at 2244..2245: invalid syntax, expected: <identifier>
            error at 2261..2266: 'patch' is a reserved word in WGSL
            error at 2267..2268: invalid syntax, expected: <identifier>
            error at 2284..2297: 'pixelfragment' is a reserved word in WGSL
            error at 2298..2299: invalid syntax, expected: <identifier>
            error at 2315..2322: 'precise' is a reserved word in WGSL
            error at 2323..2324: invalid syntax, expected: <identifier>
            error at 2340..2349: 'precision' is a reserved word in WGSL
            error at 2350..2351: invalid syntax, expected: <identifier>
            error at 2367..2375: 'premerge' is a reserved word in WGSL
            error at 2376..2377: invalid syntax, expected: <identifier>
            error at 2393..2397: 'priv' is a reserved word in WGSL
            error at 2398..2399: invalid syntax, expected: <identifier>
            error at 2415..2424: 'protected' is a reserved word in WGSL
            error at 2425..2426: invalid syntax, expected: <identifier>
            error at 2442..2445: 'pub' is a reserved word in WGSL
            error at 2446..2447: invalid syntax, expected: <identifier>
            error at 2463..2469: 'public' is a reserved word in WGSL
            error at 2470..2471: invalid syntax, expected: <identifier>
            error at 2487..2495: 'readonly' is a reserved word in WGSL
            error at 2496..2497: invalid syntax, expected: <identifier>
            error at 2513..2516: 'ref' is a reserved word in WGSL
            error at 2517..2518: invalid syntax, expected: <identifier>
            error at 2534..2544: 'regardless' is a reserved word in WGSL
            error at 2545..2546: invalid syntax, expected: <identifier>
            error at 2562..2570: 'register' is a reserved word in WGSL
            error at 2571..2572: invalid syntax, expected: <identifier>
            error at 2588..2604: 'reinterpret_cast' is a reserved word in WGSL
            error at 2605..2606: invalid syntax, expected: <identifier>
            error at 2622..2629: 'require' is a reserved word in WGSL
            error at 2630..2631: invalid syntax, expected: <identifier>
            error at 2647..2655: 'resource' is a reserved word in WGSL
            error at 2656..2657: invalid syntax, expected: <identifier>
            error at 2673..2681: 'restrict' is a reserved word in WGSL
            error at 2682..2683: invalid syntax, expected: <identifier>
            error at 2699..2703: 'self' is a reserved word in WGSL
            error at 2704..2705: invalid syntax, expected: <identifier>
            error at 2721..2724: 'set' is a reserved word in WGSL
            error at 2725..2726: invalid syntax, expected: <identifier>
            error at 2742..2748: 'shared' is a reserved word in WGSL
            error at 2749..2750: invalid syntax, expected: <identifier>
            error at 2766..2772: 'sizeof' is a reserved word in WGSL
            error at 2773..2774: invalid syntax, expected: <identifier>
            error at 2790..2796: 'smooth' is a reserved word in WGSL
            error at 2797..2798: invalid syntax, expected: <identifier>
            error at 2814..2819: 'snorm' is a reserved word in WGSL
            error at 2820..2821: invalid syntax, expected: <identifier>
            error at 2837..2843: 'static' is a reserved word in WGSL
            error at 2844..2845: invalid syntax, expected: <identifier>
            error at 2861..2874: 'static_assert' is a reserved word in WGSL
            error at 2875..2876: invalid syntax, expected: <identifier>
            error at 2892..2903: 'static_cast' is a reserved word in WGSL
            error at 2904..2905: invalid syntax, expected: <identifier>
            error at 2921..2924: 'std' is a reserved word in WGSL
            error at 2925..2926: invalid syntax, expected: <identifier>
            error at 2942..2952: 'subroutine' is a reserved word in WGSL
            error at 2953..2954: invalid syntax, expected: <identifier>
            error at 2970..2975: 'super' is a reserved word in WGSL
            error at 2976..2977: invalid syntax, expected: <identifier>
            error at 2993..2999: 'target' is a reserved word in WGSL
            error at 3000..3001: invalid syntax, expected: <identifier>
            error at 3017..3025: 'template' is a reserved word in WGSL
            error at 3026..3027: invalid syntax, expected: <identifier>
            error at 3043..3047: 'this' is a reserved word in WGSL
            error at 3048..3049: invalid syntax, expected: <identifier>
            error at 3065..3077: 'thread_local' is a reserved word in WGSL
            error at 3078..3079: invalid syntax, expected: <identifier>
            error at 3095..3100: 'throw' is a reserved word in WGSL
            error at 3101..3102: invalid syntax, expected: <identifier>
            error at 3118..3123: 'trait' is a reserved word in WGSL
            error at 3124..3125: invalid syntax, expected: <identifier>
            error at 3141..3144: 'try' is a reserved word in WGSL
            error at 3145..3146: invalid syntax, expected: <identifier>
            error at 3162..3166: 'type' is a reserved word in WGSL
            error at 3167..3168: invalid syntax, expected: <identifier>
            error at 3184..3191: 'typedef' is a reserved word in WGSL
            error at 3192..3193: invalid syntax, expected: <identifier>
            error at 3209..3215: 'typeid' is a reserved word in WGSL
            error at 3216..3217: invalid syntax, expected: <identifier>
            error at 3233..3241: 'typename' is a reserved word in WGSL
            error at 3242..3243: invalid syntax, expected: <identifier>
            error at 3259..3265: 'typeof' is a reserved word in WGSL
            error at 3266..3267: invalid syntax, expected: <identifier>
            error at 3283..3288: 'union' is a reserved word in WGSL
            error at 3289..3290: invalid syntax, expected: <identifier>
            error at 3306..3312: 'unless' is a reserved word in WGSL
            error at 3313..3314: invalid syntax, expected: <identifier>
            error at 3330..3335: 'unorm' is a reserved word in WGSL
            error at 3336..3337: invalid syntax, expected: <identifier>
            error at 3353..3359: 'unsafe' is a reserved word in WGSL
            error at 3360..3361: invalid syntax, expected: <identifier>
            error at 3377..3384: 'unsized' is a reserved word in WGSL
            error at 3385..3386: invalid syntax, expected: <identifier>
            error at 3402..3405: 'use' is a reserved word in WGSL
            error at 3406..3407: invalid syntax, expected: <identifier>
            error at 3423..3428: 'using' is a reserved word in WGSL
            error at 3429..3430: invalid syntax, expected: <identifier>
            error at 3446..3453: 'varying' is a reserved word in WGSL
            error at 3454..3455: invalid syntax, expected: <identifier>
            error at 3471..3478: 'virtual' is a reserved word in WGSL
            error at 3479..3480: invalid syntax, expected: <identifier>
            error at 3496..3504: 'volatile' is a reserved word in WGSL
            error at 3505..3506: invalid syntax, expected: <identifier>
            error at 3522..3526: 'wgsl' is a reserved word in WGSL
            error at 3527..3528: invalid syntax, expected: <identifier>
            error at 3544..3549: 'where' is a reserved word in WGSL
            error at 3550..3551: invalid syntax, expected: <identifier>
            error at 3567..3571: 'with' is a reserved word in WGSL
            error at 3572..3573: invalid syntax, expected: <identifier>
            error at 3589..3598: 'writeonly' is a reserved word in WGSL
            error at 3599..3600: invalid syntax, expected: <identifier>
            error at 3616..3621: 'yield' is a reserved word in WGSL
            error at 3622..3623: invalid syntax, expected: <identifier>"#]],
    );
}

#[test]
fn keywords_do_not_parse() {
    check(
        "
        var alias=0;
        var break=0;
        var case=0;
        var const=0;
        var const_assert=0;
        var continue=0;
        var continuing=0;
        var default=0;
        var diagnostic=0;
        var discard=0;
        var else=0;
        var enable=0;
        var false=0;
        var fn=0;
        var for=0;
        var if=0;
        var let=0;
        var loop=0;
        var override=0;
        var requires=0;
        var return=0;
        var struct=0;
        var switch=0;
        var true=0;
        var var=0;
        var while=0;
        ",
        expect![[r#"
            SourceFile@0..573
              Blankspace@0..9 "\n        "
              VariableDeclaration@9..13
                Var@9..12 "var"
                Blankspace@12..13 " "
              TypeAliasDeclaration@13..21
                Alias@13..18 "alias"
                Equal@18..19 "="
                TypeSpecifier@19..20
                  Path@19..20
                    Error@19..20
                      IntLiteral@19..20 "0"
                Semicolon@20..21 ";"
              Blankspace@21..30 "\n        "
              VariableDeclaration@30..42
                Var@30..33 "var"
                Blankspace@33..34 " "
                Error@34..39
                  Break@34..39 "break"
                Equal@39..40 "="
                Literal@40..41
                  IntLiteral@40..41 "0"
                Semicolon@41..42 ";"
              Blankspace@42..51 "\n        "
              VariableDeclaration@51..62
                Var@51..54 "var"
                Blankspace@54..55 " "
                Error@55..59
                  Case@55..59 "case"
                Equal@59..60 "="
                Literal@60..61
                  IntLiteral@60..61 "0"
                Semicolon@61..62 ";"
              Blankspace@62..71 "\n        "
              VariableDeclaration@71..75
                Var@71..74 "var"
                Blankspace@74..75 " "
              ConstantDeclaration@75..83
                Const@75..80 "const"
                Equal@80..81 "="
                Literal@81..82
                  IntLiteral@81..82 "0"
                Semicolon@82..83 ";"
              Blankspace@83..92 "\n        "
              VariableDeclaration@92..96
                Var@92..95 "var"
                Blankspace@95..96 " "
              AssertStatement@96..108
                ConstantAssert@96..108 "const_assert"
              Error@108..110
                Equal@108..109 "="
                IntLiteral@109..110 "0"
              Semicolon@110..111 ";"
              Blankspace@111..120 "\n        "
              VariableDeclaration@120..135
                Var@120..123 "var"
                Blankspace@123..124 " "
                Error@124..132
                  Continue@124..132 "continue"
                Equal@132..133 "="
                Literal@133..134
                  IntLiteral@133..134 "0"
                Semicolon@134..135 ";"
              Blankspace@135..144 "\n        "
              VariableDeclaration@144..161
                Var@144..147 "var"
                Blankspace@147..148 " "
                Error@148..158
                  Continuing@148..158 "continuing"
                Equal@158..159 "="
                Literal@159..160
                  IntLiteral@159..160 "0"
                Semicolon@160..161 ";"
              Blankspace@161..170 "\n        "
              VariableDeclaration@170..184
                Var@170..173 "var"
                Blankspace@173..174 " "
                Error@174..181
                  Default@174..181 "default"
                Equal@181..182 "="
                Literal@182..183
                  IntLiteral@182..183 "0"
                Semicolon@183..184 ";"
              Blankspace@184..193 "\n        "
              VariableDeclaration@193..197
                Var@193..196 "var"
                Blankspace@196..197 " "
              DiagnosticDirective@197..210
                Diagnostic@197..207 "diagnostic"
                DiagnosticControl@207..209
                  DiagnosticRuleName@207..209
                    Error@207..209
                      Equal@207..208 "="
                      IntLiteral@208..209 "0"
                Semicolon@209..210 ";"
              Blankspace@210..219 "\n        "
              VariableDeclaration@219..233
                Var@219..222 "var"
                Blankspace@222..223 " "
                Error@223..230
                  Discard@223..230 "discard"
                Equal@230..231 "="
                Literal@231..232
                  IntLiteral@231..232 "0"
                Semicolon@232..233 ";"
              Blankspace@233..242 "\n        "
              VariableDeclaration@242..253
                Var@242..245 "var"
                Blankspace@245..246 " "
                Error@246..250
                  Else@246..250 "else"
                Equal@250..251 "="
                Literal@251..252
                  IntLiteral@251..252 "0"
                Semicolon@252..253 ";"
              Blankspace@253..262 "\n        "
              VariableDeclaration@262..266
                Var@262..265 "var"
                Blankspace@265..266 " "
              EnableDirective@266..275
                Enable@266..272 "enable"
                Error@272..274
                  Equal@272..273 "="
                  IntLiteral@273..274 "0"
                Semicolon@274..275 ";"
              Blankspace@275..284 "\n        "
              VariableDeclaration@284..296
                Var@284..287 "var"
                Blankspace@287..288 " "
                Error@288..293
                  False@288..293 "false"
                Equal@293..294 "="
                Literal@294..295
                  IntLiteral@294..295 "0"
                Semicolon@295..296 ";"
              Blankspace@296..305 "\n        "
              VariableDeclaration@305..309
                Var@305..308 "var"
                Blankspace@308..309 " "
              FunctionDeclaration@309..313
                Fn@309..311 "fn"
                FunctionParameters@311..313
                  Error@311..313
                    Equal@311..312 "="
                    IntLiteral@312..313 "0"
              Semicolon@313..314 ";"
              Blankspace@314..323 "\n        "
              VariableDeclaration@323..333
                Var@323..326 "var"
                Blankspace@326..327 " "
                Error@327..330
                  For@327..330 "for"
                Equal@330..331 "="
                Literal@331..332
                  IntLiteral@331..332 "0"
                Semicolon@332..333 ";"
              Blankspace@333..342 "\n        "
              VariableDeclaration@342..351
                Var@342..345 "var"
                Blankspace@345..346 " "
                Error@346..348
                  If@346..348 "if"
                Equal@348..349 "="
                Literal@349..350
                  IntLiteral@349..350 "0"
                Semicolon@350..351 ";"
              Blankspace@351..360 "\n        "
              VariableDeclaration@360..364
                Var@360..363 "var"
                Blankspace@363..364 " "
              Error@364..370
                Let@364..367 "let"
                Equal@367..368 "="
                Literal@368..369
                  IntLiteral@368..369 "0"
                Semicolon@369..370 ";"
              Blankspace@370..379 "\n        "
              VariableDeclaration@379..390
                Var@379..382 "var"
                Blankspace@382..383 " "
                Error@383..387
                  Loop@383..387 "loop"
                Equal@387..388 "="
                Literal@388..389
                  IntLiteral@388..389 "0"
                Semicolon@389..390 ";"
              Blankspace@390..399 "\n        "
              VariableDeclaration@399..403
                Var@399..402 "var"
                Blankspace@402..403 " "
              OverrideDeclaration@403..414
                Override@403..411 "override"
                Equal@411..412 "="
                Literal@412..413
                  IntLiteral@412..413 "0"
                Semicolon@413..414 ";"
              Blankspace@414..423 "\n        "
              VariableDeclaration@423..427
                Var@423..426 "var"
                Blankspace@426..427 " "
              RequiresDirective@427..438
                Requires@427..435 "requires"
                Error@435..437
                  Equal@435..436 "="
                  IntLiteral@436..437 "0"
                Semicolon@437..438 ";"
              Blankspace@438..447 "\n        "
              VariableDeclaration@447..460
                Var@447..450 "var"
                Blankspace@450..451 " "
                Error@451..457
                  Return@451..457 "return"
                Equal@457..458 "="
                Literal@458..459
                  IntLiteral@458..459 "0"
                Semicolon@459..460 ";"
              Blankspace@460..469 "\n        "
              VariableDeclaration@469..473
                Var@469..472 "var"
                Blankspace@472..473 " "
              StructDeclaration@473..479
                Struct@473..479 "struct"
              Error@479..481
                Equal@479..480 "="
                IntLiteral@480..481 "0"
              Semicolon@481..482 ";"
              Blankspace@482..491 "\n        "
              VariableDeclaration@491..504
                Var@491..494 "var"
                Blankspace@494..495 " "
                Error@495..501
                  Switch@495..501 "switch"
                Equal@501..502 "="
                Literal@502..503
                  IntLiteral@502..503 "0"
                Semicolon@503..504 ";"
              Blankspace@504..513 "\n        "
              VariableDeclaration@513..524
                Var@513..516 "var"
                Blankspace@516..517 " "
                Error@517..521
                  True@517..521 "true"
                Equal@521..522 "="
                Literal@522..523
                  IntLiteral@522..523 "0"
                Semicolon@523..524 ";"
              Blankspace@524..533 "\n        "
              VariableDeclaration@533..537
                Var@533..536 "var"
                Blankspace@536..537 " "
              VariableDeclaration@537..543
                Var@537..540 "var"
                Equal@540..541 "="
                Literal@541..542
                  IntLiteral@541..542 "0"
                Semicolon@542..543 ";"
              Blankspace@543..552 "\n        "
              VariableDeclaration@552..564
                Var@552..555 "var"
                Blankspace@555..556 " "
                Error@556..561
                  While@556..561 "while"
                Equal@561..562 "="
                Literal@562..563
                  IntLiteral@562..563 "0"
                Semicolon@563..564 ";"
              Blankspace@564..573 "\n        "

            error at 13..18: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 18..19: invalid syntax, expected: <identifier>
            error at 19..20: invalid syntax, expected one of: <identifier>, 'package', 'super'
            error at 34..39: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 39..40: invalid syntax, expected: <identifier>
            error at 55..59: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 59..60: invalid syntax, expected: <identifier>
            error at 75..80: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 80..81: invalid syntax, expected: <identifier>
            error at 96..108: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 108..109: invalid syntax, expected one of: '&', '!', 'false', <floating point literal>, <identifier>, <integer literal>, '-', 'package', '(', '*', 'super', '~', 'true'
            error at 124..132: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 132..133: invalid syntax, expected: <identifier>
            error at 148..158: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 158..159: invalid syntax, expected: <identifier>
            error at 174..181: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 181..182: invalid syntax, expected: <identifier>
            error at 197..207: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 207..208: invalid syntax, expected: '('
            error at 209..210: invalid syntax, expected one of: ',', ')'
            error at 223..230: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 230..231: invalid syntax, expected: <identifier>
            error at 246..250: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 250..251: invalid syntax, expected: <identifier>
            error at 266..272: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 272..273: invalid syntax, expected: <identifier>
            error at 288..293: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 293..294: invalid syntax, expected: <identifier>
            error at 309..311: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 311..312: invalid syntax, expected: <identifier>
            error at 313..314: invalid syntax, expected one of: '->', '@', '{'
            error at 327..330: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 330..331: invalid syntax, expected: <identifier>
            error at 346..348: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 348..349: invalid syntax, expected: <identifier>
            error at 364..367: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 367..368: invalid syntax, expected: <identifier>
            error at 364..370: global let declarations are not allowed
            error at 383..387: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 387..388: invalid syntax, expected: <identifier>
            error at 403..411: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 411..412: invalid syntax, expected: <identifier>
            error at 427..435: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 435..436: invalid syntax, expected: <identifier>
            error at 451..457: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 457..458: invalid syntax, expected: <identifier>
            error at 473..479: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 479..480: invalid syntax, expected: <identifier>
            error at 495..501: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 501..502: invalid syntax, expected: <identifier>
            error at 517..521: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 521..522: invalid syntax, expected: <identifier>
            error at 537..540: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 540..541: invalid syntax, expected: <identifier>
            error at 556..561: invalid syntax, expected one of: '@', '{', '}', ',', '=', <identifier>, ')', ';', <template start>
            error at 561..562: invalid syntax, expected: <identifier>
            error at 197..210: directives must come before any declarations
            error at 266..275: directives must come before any declarations
            error at 427..438: directives must come before any declarations"#]],
    );
}
