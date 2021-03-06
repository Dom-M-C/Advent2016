module Advent2016_6 
    (   firstAnswer
    ,   secondAnswer
    )   where 

import qualified Data.Map as Map
import Data.List

type ColumnIndex = Int
type ColumnString = String

data ColumnStrings = ColumnStrings (Map.Map ColumnIndex ColumnString)

buildColumnString :: Char -> Maybe ColumnString -> ColumnString
buildColumnString c Nothing = [c]
buildColumnString c (Just cs) = c:cs

buildRowMap :: String -> ColumnIndex -> Map.Map ColumnIndex ColumnString -> Map.Map ColumnIndex ColumnString 
buildRowMap [] _ m = m
buildRowMap (c:cs) idx m = 
    let
        columnString = buildColumnString c $ Map.lookup idx m
        updatedMap = (Map.insert idx columnString m)
        nextIndex = idx + 1
    in
        buildRowMap cs nextIndex updatedMap
        
buildColumnMap :: [String] -> Map.Map ColumnIndex ColumnString -> ColumnStrings 
buildColumnMap [] m = ColumnStrings m
buildColumnMap (row:rows) m = buildColumnMap rows (buildRowMap row 0 m)

showColumnStrings :: ColumnStrings -> String
showColumnStrings (ColumnStrings cmap)
    | cmap == Map.empty = "<empty>"
    | otherwise = mconcat . map columnPairToString $ Map.toList cmap

columnPairToString :: (ColumnIndex, ColumnString) -> String
columnPairToString (x, y) = "Position " <> (show x) <> ": " <> y <> "\n"

instance Show ColumnStrings where
    show = showColumnStrings         
    
countChars :: String -> Map.Map Char Int -> Map.Map Char Int
countChars [] m = m
countChars (c:cs) m =
    let
        lookupChar = Map.lookup c m 
        insertOrUpdate (Nothing) = Map.insert c 1 m
        insertOrUpdate (Just count) = Map.insert c (count + 1) m
    in
        countChars cs (insertOrUpdate lookupChar)

reduceColumnStringToMapCount :: ColumnStrings -> [Map.Map Char Int]
reduceColumnStringToMapCount (ColumnStrings m) = 
    let
        colStrings = map (snd) $ Map.toList m
        countMap = map (\x -> countChars x Map.empty)   colStrings        
    in
        countMap

mostCommonChar :: Map.Map Char Int -> Char
mostCommonChar = fst . head . reverse . sortBy sortTupBySnd . Map.toList

leastCommonChar :: Map.Map Char Int -> Char
leastCommonChar = fst . head . sortBy sortTupBySnd . Map.toList

stringOfMapToChar :: (Map.Map Char Int -> Char) -> ColumnStrings -> String
stringOfMapToChar f cs = map f $ reduceColumnStringToMapCount cs

getMaxPair tup@(x, y) = head . reverse

sortTupBySnd :: Ord b => (a, b) -> (a, b) -> Ordering
sortTupBySnd x y = compare (snd x) (snd y)

inputColumnStrings inp = buildColumnMap inp Map.empty

testAnswer = stringOfMapToChar mostCommonChar $ inputColumnStrings testInput

firstAnswer = stringOfMapToChar mostCommonChar $ inputColumnStrings partOneInput

secondTest = stringOfMapToChar leastCommonChar $ inputColumnStrings testInput

secondAnswer = stringOfMapToChar leastCommonChar $ inputColumnStrings partOneInput

testInput = 
    [   "eedadn"
    ,   "drvtee"
    ,   "eandsr"
    ,   "raavrd"
    ,   "atevrs"
    ,   "tsrnev"
    ,   "sdttsa"
    ,   "rasrtv"
    ,   "nssdts"
    ,   "ntnada"
    ,   "svetve"
    ,   "tesnvt"
    ,   "vntsnd"
    ,   "vrdear"
    ,   "dvrsen"
    ,   "enarar"
    ]

partOneInput =
    [   "jtfxgqec"
    ,   "zxoeuddn"
    ,   "anlfufma"
    ,   "dxuuyxkg"
    ,   "ttnewhlw"
    ,   "sjoyeiry"
    ,   "rgfwwdhw"
    ,   "qymxsllk"
    ,   "forftdvy"
    ,   "rzmnmewh"
    ,   "hogawihi"
    ,   "mtsyexba"
    ,   "mrjzqqfk"
    ,   "ypmkexpg"
    ,   "pjuyopgv"
    ,   "rtqquvaj"
    ,   "evubmlrq"
    ,   "bqlrtuce"
    ,   "ndidnbps"
    ,   "vqukosam"
    ,   "mzdyfkcd"
    ,   "rrbwdimb"
    ,   "uhnvxgly"
    ,   "aaimxpcv"
    ,   "acxvinqj"
    ,   "muaeikzy"
    ,   "lhzbosjd"
    ,   "fflqqiit"
    ,   "unfhzfrs"
    ,   "gmwoyvob"
    ,   "cculubmy"
    ,   "zqbugcwa"
    ,   "ijouicwt"
    ,   "bildjjww"
    ,   "ugksmnps"
    ,   "ivawibvu"
    ,   "igzteede"
    ,   "foehssxo"
    ,   "pkeevvlt"
    ,   "xumuixyw"
    ,   "okhhtycj"
    ,   "xhblffye"
    ,   "iqapgjqe"
    ,   "lkhpntum"
    ,   "wuzxgwow"
    ,   "bkkpfguu"
    ,   "bnqctsdi"
    ,   "cwncjrwn"
    ,   "eivhabsi"
    ,   "bwdicgfm"
    ,   "kowiourk"
    ,   "dhbzuztx"
    ,   "gibitfxo"
    ,   "wmrlhenb"
    ,   "wfzmjvwh"
    ,   "zddjirfg"
    ,   "fafhmiwf"
    ,   "ddhvufhg"
    ,   "qdwnlzqp"
    ,   "nhsnngut"
    ,   "uacmfgop"
    ,   "morcixux"
    ,   "sfdxrgqy"
    ,   "tezzvctv"
    ,   "dnnmtkfp"
    ,   "dygdzcib"
    ,   "efurreri"
    ,   "npvpklix"
    ,   "svpbdgyw"
    ,   "mcntltzd"
    ,   "inwkhxlx"
    ,   "sajfgeoi"
    ,   "nwkqrspt"
    ,   "qtzqsksv"
    ,   "mtncajjk"
    ,   "etarsvxr"
    ,   "eyaeeauy"
    ,   "gqnctylg"
    ,   "uerywmma"
    ,   "hjrxhtjb"
    ,   "zdsdyfzp"
    ,   "zhgrrhvd"
    ,   "yvxqyalf"
    ,   "rlgwftff"
    ,   "xczvgpzq"
    ,   "yydydclu"
    ,   "rzltbrro"
    ,   "jforpzau"
    ,   "zskadlfz"
    ,   "dqbqdsgv"
    ,   "bcwjltvc"
    ,   "byfoamgd"
    ,   "cpefdmso"
    ,   "ocuetyke"
    ,   "vlqrfnpp"
    ,   "ggikwydh"
    ,   "eakpyuov"
    ,   "osaguhlz"
    ,   "ylmrfvee"
    ,   "nvdvqpzm"
    ,   "pudbbuhh"
    ,   "bwmqdpyv"
    ,   "proscvgy"
    ,   "cetkcpjw"
    ,   "sbhcqeya"
    ,   "fgnyltmf"
    ,   "qcspgopp"
    ,   "bdhnemmy"
    ,   "tczkhihl"
    ,   "yduxunvr"
    ,   "dtxerncl"
    ,   "xnxeaayt"
    ,   "rvlcbgts"
    ,   "vpavzjqs"
    ,   "oueloufw"
    ,   "mubbhyna"
    ,   "nptmeppg"
    ,   "ojjfbuzz"
    ,   "lusboycs"
    ,   "gurmmorr"
    ,   "kefddaka"
    ,   "cpvpszit"
    ,   "bfvthzpm"
    ,   "owgcvdjo"
    ,   "simxphmv"
    ,   "rxedvjyw"
    ,   "hmeieuxr"
    ,   "vgqhcapz"
    ,   "vwtvbain"
    ,   "aobnhdsx"
    ,   "hkpshsjs"
    ,   "jxgegczu"
    ,   "xbsfxesk"
    ,   "pqhifeaj"
    ,   "triurorr"
    ,   "rnkufaxl"
    ,   "hmrqfoaw"
    ,   "veghzoxa"
    ,   "zbvgbpcm"
    ,   "rqrnbylj"
    ,   "txaawlta"
    ,   "uuksnfel"
    ,   "jqvycrvw"
    ,   "cdttmdpc"
    ,   "wojvbrzp"
    ,   "qvnuinon"
    ,   "gnpguyvh"
    ,   "cgbkpzbu"
    ,   "pdaqhlan"
    ,   "muiykslt"
    ,   "prvzlunm"
    ,   "whhcrchz"
    ,   "cahjhrkl"
    ,   "zifdgfpq"
    ,   "wanlienf"
    ,   "sfrnozvi"
    ,   "mwmykvyh"
    ,   "fbdfzgut"
    ,   "wfrviilb"
    ,   "ucaopfgo"
    ,   "fjhuikma"
    ,   "hdmizjdj"
    ,   "xngpfwvn"
    ,   "rueojtjg"
    ,   "xvtssxtx"
    ,   "vvcgzidf"
    ,   "xtehcxki"
    ,   "xksbfbso"
    ,   "osnzpqmy"
    ,   "isrnjkxh"
    ,   "utleakmz"
    ,   "dthmtbdt"
    ,   "plregxuh"
    ,   "amoeprsy"
    ,   "tmyhzhqd"
    ,   "csxqavbe"
    ,   "jmojlysw"
    ,   "slebxnbl"
    ,   "ldzryqmj"
    ,   "ajejyudk"
    ,   "ynhgnjhw"
    ,   "mdibxxxw"
    ,   "rvtcmesd"
    ,   "jmnwqddq"
    ,   "hppfoplc"
    ,   "nrcbjynz"
    ,   "kcqnjzue"
    ,   "mthvgjxm"
    ,   "ykztdbcv"
    ,   "etqqnhuz"
    ,   "tezkopgq"
    ,   "fwhwkqmz"
    ,   "fozpkzfy"
    ,   "hbbtlcog"
    ,   "hdvjqwyh"
    ,   "xuljsrvz"
    ,   "abskreoo"
    ,   "aedeydgc"
    ,   "dcyigvqf"
    ,   "ntpcvvgk"
    ,   "iiwgzkhl"
    ,   "zofhlqlx"
    ,   "veumtlae"
    ,   "qibdapwq"
    ,   "xpgpwirt"
    ,   "wvnnautq"
    ,   "wfhlgmdg"
    ,   "yqcrvdgx"
    ,   "srdufrbu"
    ,   "vycrvkpx"
    ,   "flwxzkim"
    ,   "enxayqxm"
    ,   "dgpntiaj"
    ,   "qedfutmp"
    ,   "vfdovine"
    ,   "dgrvjfjt"
    ,   "dqxxjahk"
    ,   "hnxpblyp"
    ,   "nnadwbsc"
    ,   "krmqqgwf"
    ,   "efykkzeb"
    ,   "lkrmrwqw"
    ,   "vfzayrwt"
    ,   "chopbnyf"
    ,   "vbydrtln"
    ,   "azmlestl"
    ,   "sqcyddvi"
    ,   "zdcubjok"
    ,   "afshwptc"
    ,   "sjgpuoch"
    ,   "bnfylydl"
    ,   "rsyxsbzi"
    ,   "psyuvyzx"
    ,   "npngqypd"
    ,   "xejayhdk"
    ,   "aqfmvjfi"
    ,   "tpffksph"
    ,   "uekwkjnj"
    ,   "ljsjimwm"
    ,   "hbgzjlig"
    ,   "ngssshxx"
    ,   "icitlosb"
    ,   "unxryqyt"
    ,   "nzpujfti"
    ,   "lupxnzhe"
    ,   "kxglfnic"
    ,   "ecewosbs"
    ,   "htlqxpiq"
    ,   "clqgnyfd"
    ,   "yyiozvar"
    ,   "mbvjgmyc"
    ,   "srhwhlin"
    ,   "casmlryr"
    ,   "ebuzskkp"
    ,   "iewhdqtr"
    ,   "oyidcobe"
    ,   "avptvltf"
    ,   "mfheqaxl"
    ,   "shqnezrq"
    ,   "xrpkzuvb"
    ,   "soxdjwba"
    ,   "aitmzlds"
    ,   "rpmpozpd"
    ,   "ccgxauky"
    ,   "gsstsjyx"
    ,   "bzeolqal"
    ,   "vfhddmuc"
    ,   "wfbbmqfv"
    ,   "pumxmnhj"
    ,   "qumdxkns"
    ,   "xymraott"
    ,   "uthlccig"
    ,   "ezpalags"
    ,   "giftxymr"
    ,   "ujjacleo"
    ,   "cgwgmktp"
    ,   "istetgdl"
    ,   "azedmaao"
    ,   "bnlfwyoq"
    ,   "orcwhbek"
    ,   "amswhkum"
    ,   "yxupesxu"
    ,   "mlzvqsrg"
    ,   "solkxzby"
    ,   "tbaxnjdu"
    ,   "xwbsiquk"
    ,   "hsftntsn"
    ,   "ajraaorz"
    ,   "mwmycrff"
    ,   "ymnbrbpj"
    ,   "uyfscatq"
    ,   "kzkgmbeh"
    ,   "libgpgnr"
    ,   "kxlgthxc"
    ,   "vzjbobyx"
    ,   "isqessab"
    ,   "ehursvof"
    ,   "guwrjnbi"
    ,   "xivkphwn"
    ,   "rurrmdmi"
    ,   "nqijeuzq"
    ,   "jambocej"
    ,   "qrtidktb"
    ,   "sbzvehmq"
    ,   "aikgzrsq"
    ,   "lgydnujf"
    ,   "twafyzry"
    ,   "nxhtklba"
    ,   "xhyaqyqe"
    ,   "xgvdfcrf"
    ,   "wdieppsd"
    ,   "iabrfmdm"
    ,   "doijaavc"
    ,   "oxydttkg"
    ,   "qsqiofwv"
    ,   "titrvjym"
    ,   "mwojqcku"
    ,   "tewiyhjx"
    ,   "jlqbksqd"
    ,   "knycvoks"
    ,   "tmcbnvhv"
    ,   "ekksoxmz"
    ,   "mgvommal"
    ,   "hrosnzeu"
    ,   "fzeymbek"
    ,   "evqxcukn"
    ,   "ilkpvdvl"
    ,   "rclpjbkb"
    ,   "tdpitlei"
    ,   "zvvzuucc"
    ,   "pzdgwnfz"
    ,   "mralxxlz"
    ,   "wywkawzh"
    ,   "hmazaakd"
    ,   "llltvbex"
    ,   "ihsmefpz"
    ,   "rzzgkjyz"
    ,   "srjqpeoq"
    ,   "jrczcdna"
    ,   "uuyskwop"
    ,   "yeuiaepa"
    ,   "vzppcwnn"
    ,   "oqhxixdo"
    ,   "xkwpfsij"
    ,   "cmsoiogl"
    ,   "ngbmaeue"
    ,   "lmqttyrj"
    ,   "yhgjxfmx"
    ,   "lwfgjnyp"
    ,   "ibbkjgra"
    ,   "gaxsotzr"
    ,   "paugisvs"
    ,   "pcqqauqi"
    ,   "pweuwnqs"
    ,   "jcbrscrj"
    ,   "ovtsgcnh"
    ,   "oscsgtqn"
    ,   "hkpwmhwk"
    ,   "pmdgwclk"
    ,   "owmskdhh"
    ,   "qutyussr"
    ,   "atdkvmzl"
    ,   "oqslriwe"
    ,   "wafjwfxp"
    ,   "ipcqlsxv"
    ,   "kzurbnoh"
    ,   "lfhfzwqo"
    ,   "ucybqwrj"
    ,   "tgnblzgm"
    ,   "lhwlniea"
    ,   "tlxymfbu"
    ,   "bcyvlkvt"
    ,   "glpacpjk"
    ,   "rjagzpnu"
    ,   "fyjpvhaq"
    ,   "cjtzwtdu"
    ,   "dkaqawts"
    ,   "pjoovtlv"
    ,   "xsnwqixw"
    ,   "swcftfed"
    ,   "cadigksp"
    ,   "fnsmxccx"
    ,   "cbxmdxvb"
    ,   "hpyqnpjq"
    ,   "jzpvphmo"
    ,   "kdkpubul"
    ,   "kiajwwta"
    ,   "uyeuctbe"
    ,   "yetyzqxw"
    ,   "fgeemnbl"
    ,   "brprbvgj"
    ,   "xszwwlea"
    ,   "ygunyguo"
    ,   "jwplrcbq"
    ,   "fejndxnx"
    ,   "oxsmkcqm"
    ,   "ldwkbpsk"
    ,   "cmzuxrst"
    ,   "jaoadiiu"
    ,   "oxcpkgbc"
    ,   "nyulhuci"
    ,   "bdwfqtkv"
    ,   "ehxvnzyd"
    ,   "cizuemsb"
    ,   "lbqyqduk"
    ,   "kqweswcd"
    ,   "tqnicuzh"
    ,   "utyaiaeu"
    ,   "osjdgvtj"
    ,   "qmrxcaoa"
    ,   "qiltxgvv"
    ,   "qklfgyss"
    ,   "lpjebmuo"
    ,   "bvebkous"
    ,   "yifrmeoa"
    ,   "jzgntlep"
    ,   "wadcknde"
    ,   "kaikclag"
    ,   "tucuhehr"
    ,   "bvwhuwzn"
    ,   "uvlecxgy"
    ,   "rzyxjhmo"
    ,   "dyyfwjgv"
    ,   "vocjkohi"
    ,   "ylyflktq"
    ,   "raltxpqg"
    ,   "eitypruw"
    ,   "pfbmopgm"
    ,   "qerushjt"
    ,   "xykophcv"
    ,   "amjhrlhi"
    ,   "uqkjhdhn"
    ,   "kkohprfw"
    ,   "hvsmtnfd"
    ,   "uxgiqmqc"
    ,   "npxwplcj"
    ,   "ltchgces"
    ,   "exiyyief"
    ,   "ysmvbqso"
    ,   "zpyvuhqz"
    ,   "lkvwronk"
    ,   "vxilskkl"
    ,   "cxfypwcd"
    ,   "jhrczkmf"
    ,   "rdedtejq"
    ,   "gmxcrlzi"
    ,   "jumwfmnn"
    ,   "gkynzdtd"
    ,   "dfdkxggc"
    ,   "yldclxhz"
    ,   "fsxvbwyj"
    ,   "ioiupzio"
    ,   "lxyqvncv"
    ,   "rsgsviny"
    ,   "osgcimej"
    ,   "tecqrgkq"
    ,   "tozohtwt"
    ,   "kmlowfrf"
    ,   "hhpiukqe"
    ,   "xlxlkjwf"
    ,   "ntvtoexx"
    ,   "zzvsvdow"
    ,   "yluidajg"
    ,   "vumkynvp"
    ,   "vaxipwwg"
    ,   "pqymmoif"
    ,   "sgjzogut"
    ,   "jppwszzn"
    ,   "gvvaibqu"
    ,   "lwjotuil"
    ,   "srflotab"
    ,   "ibnblmjm"
    ,   "kvcsdivb"
    ,   "wqrpzmvr"
    ,   "gcmqdezs"
    ,   "vrizdyfo"
    ,   "vtqnsjbf"
    ,   "jwocjmvb"
    ,   "fjkiiowl"
    ,   "ctjhmmrq"
    ,   "pcckqfki"
    ,   "wqolxgfg"
    ,   "gbsdyrbc"
    ,   "giqmfqwb"
    ,   "fodfpvyl"
    ,   "nxdzwvzz"
    ,   "hpnatltw"
    ,   "adjjyhjd"
    ,   "aoguhvmv"
    ,   "yyeanoir"
    ,   "baojaygs"
    ,   "ovkebbjb"
    ,   "pmykvfex"
    ,   "zeooykoa"
    ,   "uuozuxjb"
    ,   "kxxvbhbr"
    ,   "jxbchjlr"
    ,   "qhiwdonk"
    ,   "dnvfwwfh"
    ,   "kjfrlslh"
    ,   "wionbrdf"
    ,   "qgkjarob"
    ,   "kwplsxso"
    ,   "txgelygh"
    ,   "vlmziqwf"
    ,   "wbetqqkp"
    ,   "qfkocear"
    ,   "wrvonhyr"
    ,   "sbiqrcri"
    ,   "lnwzitce"
    ,   "bctyrwph"
    ,   "kallfwzc"
    ,   "zfqwanet"
    ,   "bevnljjr"
    ,   "kwqsktan"
    ,   "gjviqwlu"
    ,   "zflsnpig"
    ,   "wzaufqvr"
    ,   "uvxhutav"
    ,   "diejbica"
    ,   "ojciaexn"
    ,   "zyjoxrwi"
    ,   "djkodeiz"
    ,   "gsinkcqk"
    ,   "jkonssuq"
    ,   "eychyabp"
    ,   "fkcogwnr"
    ,   "kkioyrnn"
    ,   "inqxlztu"
    ,   "cqnbxxks"
    ,   "ipwmpdmm"
    ,   "moozfajm"
    ,   "irjaimrw"
    ,   "ojihmanb"
    ,   "hzoszxzc"
    ,   "ajjvxqqi"
    ,   "ohkfkijd"
    ,   "nlsahrpv"
    ,   "zizxtmxa"
    ,   "gjtnrurd"
    ,   "pyqghfuj"
    ,   "fltnnyfe"
    ,   "goxagvfp"
    ,   "nplhpkiy"
    ,   "dlwgyvby"
    ,   "fzrfhcgh"
    ,   "zaiuostp"
    ,   "jdjojfkw"
    ,   "thksqbjh"
    ,   "qopcwnht"
    ,   "ewkljwho"
    ,   "qguaeaac"
    ,   "wxzzxgcc"
    ,   "nlnuuhdu"
    ,   "ihtzrqay"
    ,   "nmtdbkhp"
    ,   "yasxhulm"
    ,   "drzjobfy"
    ,   "qpgcjdxn"
    ,   "aegbxmjb"
    ,   "bbuxsffr"
    ,   "zevjcgzn"
    ,   "pgbqezxk"
    ,   "qdlepjko"
    ,   "zbtzvicm"
    ,   "ssjdcggg"
    ,   "ugrtxalo"
    ,   "tsbvnppt"
    ,   "rboleppu"
    ,   "gywfqiwz"
    ,   "skgzeqhu"
    ,   "hzuggbcf"
    ,   "dkegaxap"
    ,   "zijcjrkm"
    ,   "jtfkeoog"
    ,   "fyvtrvig"
    ,   "gophbeoj"
    ,   "ieatnihe"
    ,   "vlaauxgz"
    ,   "mxnheqkz"
    ,   "mftwybny"
    ,   "ebawojuj"
    ,   "dyrvecbs"
    ,   "lrrcwang"
    ,   "qswijdeu"
    ,   "wkuszdax"
    ,   "ecaokzfc"
    ,   "pmbznspx"
    ,   "tjqrztdv"
    ,   "mwdxruge"
    ,   "whutfdqy"
    ,   "zpfwqvox"
    ,   "fkqapoid"
    ,   "bodleqbn"
    ,   "kpxiuodk"
    ,   "johmsncc"
    ,   "enhamlol"
    ,   "yhtydoss"
    ]