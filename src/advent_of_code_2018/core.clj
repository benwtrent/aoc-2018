(ns advent-of-code-2018.core)

;;; https://adventofcode.com/2018

;;https://adventofcode.com/2018/day/1
(defn day1-p1 [freq]
  (reduce + freq))

(def day1-input [-16 12 -6 -16 4 19 -10 20 -16 -6 3 -12 -7 6 -12 -7 -15 -2 11 5 5 -9 -14 -3 -4 16 11 7 17 -4 17 1 -9 -6 -14 -8 3 -13 14 -8 -5 16 18 -4 10 16 -4 9 14 8 -19 -11 19 -13 8 -4 -6 -7 4 6 6 7 6 -3 10 13 -19 5 8 16 3 11 7 2 8 5 -8 10 6 -2 -16 -9 -1 -11 -12 -8 17 -16 -5 16 -9 13 3 6 -16 4 -6 -19 5 -6 7 8 20 -1 6 -3 16 -17 10 1 20 -15 13 10 15 -9 6 11 14 8 -3 5 -14 2 -14 -19 1 -5 1 18 1 -13 11 10 -19 -13 7 -16 -5 13 16 18 -2 6 -11 9 11 6 -7 -17 -18 6 3 19 20 4 15 11 -13 12 -7 10 -5 -14 -16 11 10 2 13 7 9 -18 -5 -17 14 16 -3 9 6 -17 16 6 14 12 11 7 17 11 -15 8 17 17 2 -15 -16 15 15 -2 16 -10 2 5 -16 8 -19 5 -2 7 -16 10 -9 -11 13 11 4 -3 15 -1 10 18 -4 -1 -12 6 15 -3 -17 3 21 2 -18 -15 -21 -16 5 -20 7 -5 -16 18 -19 14 10 -19 -15 -14 13 -15 6 -13 17 4 15 -7 -13 21 17 -8 -4 9 -11 4 -1 -15 -20 -14 -9 -11 -17 -11 -12 -4 3 -10 -13 18 -14 10 18 6 -13 9 -1 -4 8 6 25 -11 -11 19 15 15 -2 -16 11 8 -13 -4 -5 2 5 20 -14 -18 17 -4 12 8 -11 17 13 8 -19 -1 11 29 5 26 14 -1 10 -16 11 12 -4 -15 -16 32 -8 5 12 -4 21 -7 -7 -31 -5 -15 -16 9 -36 -11 -25 16 12 14 -72 7 4 15 15 16 39 73 20 -14 6 13 -2 1 -28 75 -14 15 8 -10 -3 -16 -6 -19 -5 -24 -24 106 14 -2 19 -8 -16 -13 33 20 -2 -2 24 -2 -11 8 4 9 32 -9 19 -2 -22 -6 50 -17 10 6 23 10 3 88 112 11 18 12 55 9 29 13 -88 6 423 55676 1 -8 -16 -16 19 7 -5 4 -3 -16 9 3 -7 -10 -8 -3 -4 8 -7 -15 2 -1 8 3 4 -3 13 -19 -15 -11 10 3 -9 -19 -1 -4 19 13 -10 14 9 14 15 -8 -24 -15 11 -1 -16 -15 -4 -18 -5 10 -8 -3 2 13 14 7 12 -9 17 -9 -9 -21 6 17 -24 -6 10 13 -20 -15 -14 -11 -3 -8 -1 15 9 -17 -18 4 6 -11 -19 -7 18 -4 -12 -5 -18 -3 10 -4 -10 -11 6 11 6 8 12 8 -1 19 14 -1 -3 19 -5 15 3 1 22 -19 -8 -2 -9 13 1 -7 18 16 4 6 19 -18 2 3 34 -14 8 7 12 9 -15 24 7 11 -4 -13 16 7 -13 -15 -12 20 21 -4 21 19 19 10 -7 -9 11 -6 5 5 -13 -17 8 4 -13 -8 14 11 7 -9 -17 -3 -5 -16 9 -2 -8 -3 10 16 18 5 15 14 2 15 -8 1 15 11 18 -5 3 -13 -9 -19 15 -7 -14 -18 12 -1 16 -8 9 16 20 -16 11 1 14 -16 -4 3 14 10 12 -16 -4 14 -19 -12 -20 17 -16 -6 15 -20 -16 -17 10 13 -14 18 -6 3 -10 -3 4 13 -16 -7 31 11 1 1 13 -5 -5 21 18 20 -9 -3 -9 -11 8 -1 -4 14 14 -15 -6 14 1 2 14 21 8 -15 20 -10 -20 -5 -10 -17 -15 16 -8 16 15 12 11 -3 21 -17 6 -15 1 -19 8 21 -16 -19 8 7 -8 3 -20 -39 -18 14 -12 -29 3 -14 -10 -9 -3 -22 -8 1 3 -5 -1 -19 -25 9 -7 -14 19 19 -27 -6 27 10 29 19 -23 17 -10 5 13 -15 -25 13 3 14 -37 6 18 18 89 57 -17 39 -2 27 13 -18 -17 9 1 14 -4 10 -19 18 7 13 8 7 11 -7 -9 -3 -4 9 24 -3 -16 -23 7 -16 -1 -15 -12 19 -2 14 -21 -2 -12 -14 -62 22 7 66 26 -21 61 5 -6 11 -7 18 67 38 18 30 -18 -120 24 -18 -16 31 -102 11 149 96 9 -40 69 13 757 55505 6 11 13 -16 14 -13 -2 -14 -19 18 -14 -18 1 11 19 -6 -2 -13 3 -14 -2 6 4 4 -18 16 -13 -16 8 10 -9 1 -19 14 -15 -10 8 -10 -18 -3 -7 3 2 10 -18 -2 -18 17 -13 -9 16 -3 -19 -7 -6 -10 6 -12 17 -18 11 5 -2 -4 -15 -2 -19 -9 15 -4 -17 16 17 13 -7 -9 10 -5 -11 4 -16 -19 -13 -8 -8 18 -6 19 2 16 -11 18 3 -14 2 7 19 -8 24 -9 17 9 10 -8 -4 -8 24 19 -6 4 13 4 18 -8 12 -17 -17 -2 -17 -11 -2 5 -6 2 8 -5 -16 -17 5 -1 -6 -12 -9 -14 -17 5 -2 -17 -11 9 -12 4 -8 2 9 18 19 20 -23 -5 6 21 12 28 23 -4 10 16 15 6 9 -10 -1 18 -21 -10 20 -15 -9 21 5 -15 -15 12 29 -12 21 -2 -8 4 5 -11 19 -14 -113326])

(day1-p1 day1-input)                                        ;416

(defn day1-p2 [freq]
  (let [size (count freq)]
    (loop [pos 0 seen #{} cur 0]
      (if (contains? seen cur)
        cur
        (recur (mod (+ pos 1) size) (conj seen cur) (+ cur (get freq pos)))))))

(day1-p2 day1-input)                                        ;56752

;Day 2
;;https://adventofcode.com/2018/day/2
(def day2-input ["kbqwtcvzgumhpwelrnaxydpfuj"
                 "kbqwtcvzgsmhpoelryaxydiqij"
                 "kbqwpcvzssmhpoelgnaxydifuj"
                 "kbqgtcvxgsmhpoalrnaxydifuj"
                 "kbqwtcvygsmhpoelrnaxydiaut"
                 "kbqwtcvjgsmhpoelrnawydzfuj"
                 "kbqftcvzgsmhpoeprnaxydifus"
                 "rbqwtcgzgsxhpoelrnaxydifuj"
                 "kbqwtlvzgvmhpoelrnaxkdifuj"
                 "kbqwtcvzgsmhpolqrnaxydifub"
                 "kbqbtcqzgsmhposlrnaxydifuj"
                 "kbqwmcvzgswhpoelxnaxydifuj"
                 "kbqwtyvzgsmhkoelrnsxydifuj"
                 "khqwtcvzgsmhqoelinaxydifuj"
                 "koqwtcvzcsmhpoelrnaxydizuj"
                 "kbqwtcvzlsmhpoezrnaxydmfuj"
                 "kbqwtcvzdsmhpoelrjaxydifij"
                 "kbqwtcvzgsmhpoelrncxyjifuk"
                 "kbtwtcvzgsmhpoelonaxydiwuj"
                 "kbqwfcrzgsmhpoelrnaeydifuj"
                 "kbqutcvkgsmhpoelrnfxydifuj"
                 "kbqwtcvzgsmvvoelrnaxydihuj"
                 "kbqwtcvzhymhpoelrnaxydifyb"
                 "kbqctcvzgumhpoalrnaxydifuj"
                 "kuqktcvzgsmhpoelrnaxydieuj"
                 "kbqwtcvzgsmvpozlrnaxydifmj"
                 "kbqwtcvzgsmhpojlraaxydiouj"
                 "kbqwtcvzgmmhpoelknaxydizuj"
                 "kbwwtcvzgsmhpoefrnaxydifij"
                 "kbqwucvzgsmhpoelvnahydifuj"
                 "kbqwtcvzpsmhpgelrqaxydifuj"
                 "kblqtcvzgsmhpoeirnaxydifuj"
                 "kbqwtcvzgsmhpovlrnabydifum"
                 "kbqwwcvzgsmhpoelrnaoydnfuj"
                 "kyqwdcvzgsmhpoelrnaxfdifuj"
                 "kbqftcvzgsmxpoelknaxydifuj"
                 "kbqwtsvzksmhpoelqnaxydifuj"
                 "kbqwtcvzgsmhplelrnauydifux"
                 "kbqytcvzgsmhpkelrnaxydefuj"
                 "kbqwtcvzgsmjjoelrlaxydifuj"
                 "kbqvtcvzgsmhpoelnnaxydafuj"
                 "kbqwtcvzgsjhioelrnaxpdifuj"
                 "kbqptcvpgsmhpoelrnaxydiful"
                 "kbqwjcazgimhpoelrnaxydifuj"
                 "kbqxtcvzgwmhpaelrnaxydifuj"
                 "kbqwtcezgsmhqoelrnaxydifub"
                 "kbqwtcvzgsmhooelynaxydifuf"
                 "kbqwtwvzgsmkpoelrnaxrdifuj"
                 "nbqwtcvugsmhpoelrnzxydifuj"
                 "kbvwqcvzgsmhpoelsnaxydifuj"
                 "kbqwtcyzjsmhpoelrnaxymifuj"
                 "kbqwtcvzgsmhpoclrnaxykzfuj"
                 "kbbwtcvzgsmhyodlrnaxydifuj"
                 "kbwwtcvzgsmytoelrnaxydifuj"
                 "kbmwtcczgpmhpoelrnaxydifuj"
                 "ubqwtcvzgsmmpoblrnaxydifuj"
                 "kbqwtcvzgrmhpoelrnaxnrifuj"
                 "kbqwhcvzgsmhpoelynaaydifuj"
                 "kbqwtcvzgsmtpoelrcpxydifuj"
                 "kdqwtchzgsmhpoelrmaxydifuj"
                 "qbqrncvzgsmhpoelrnaxydifuj"
                 "kbqwtcvzghshpoelrnaxodifuj"
                 "kbqwhcvzgsmhpoelknaxydiwuj"
                 "ebqwtcvzgsmhpoelrotxydifuj"
                 "kbqwacvzusmhpoelryaxydifuj"
                 "kbqwtcvggsmhpoelrnaxygifyj"
                 "kbqwtcvzgsmhpoelrnaxycwfuo"
                 "kzqwzcvzgsmhpoelrxaxydifuj"
                 "khqwtcvzgsmhpoelrnaxldifyj"
                 "kbqwtbtzgsmhpoelrnaxydifud"
                 "gbqwtcvzgqmhpoelrnaxydifrj"
                 "kbqdtqvzgwmhpoelrnaxydifuj"
                 "kbqwscvzgsmhpoelrpaxypifuj"
                 "kmqwtcdzgsmhpoelenaxydifuj"
                 "klqwtcvvgsmhpoelrfaxydifuj"
                 "kbuwtcvzgsmhpoelrtaxyuifuj"
                 "kbqwtcvrgomhpoelrnaxydijuj"
                 "kbqwtgvzgsmhzoelrnpxydifuj"
                 "kbqltcvzgsmhooeljnaxydifuj"
                 "kbqwtcvzgbmxpoelrnaxydivuj"
                 "kbqdtcmzgsmhpoelrnaxydmfuj"
                 "kbqwtcazgsmhpoplrnacydifuj"
                 "kbqztcvegsmhpoelrnvxydifuj"
                 "kbqwtcvzgsmhpoecrnaxydzfsj"
                 "kbqwtcvzgsmepoelrnaqydifuf"
                 "kbqwtcqzgsmhpoelrnoxydivuj"
                 "kbqwtcvzgsmhpoeylnaxydhfuj"
                 "kbqwtcvfgsmhpoelrnaxgdifyj"
                 "kbqwtcvzgsmhnbelrnaxyfifuj"
                 "kbqwtcvzgsmhpoelrnaxbdffmj"
                 "kwqwtcvogtmhpoelrnaxydifuj"
                 "kdqwtcvzggyhpoelrnaxydifuj"
                 "kbqwtuvzgtmhpoelrnaxydifxj"
                 "kbqctdvzcsmhpoelrnaxydifuj"
                 "kbqwtcvzgsmhpoblrniyydifuj"
                 "kbqwucvzzsmhpoelrnvxydifuj"
                 "kbqwtcvzgslzpoelrnaxydiruj"
                 "kbqwtdmzgsmhpwelrnaxydifuj"
                 "kbqwtcvzgsmhpoilrnaxqiifuj"
                 "kbqwtcvzgsmhpgelrnaxydisnj"
                 "kbdwtqvzgsmhpoelrnaxydivuj"
                 "kbqvtdvzgsmhpoelrjaxydifuj"
                 "kfqwtcvzgsmhpoeurnyxydifuj"
                 "kbqwtcvzgsmhpoglrnaxqkifuj"
                 "kbqwtcvrgsmhpoelrnajydifnj"
                 "xbqwpcvzgjmhpoelrnaxydifuj"
                 "kbqwtcvzgsmhpoelrdaxvdihuj"
                 "kbuwtcvzssmhpoklrnaxydifuj"
                 "kbqwtcvzgqmhpoelrnzxydifbj"
                 "kbqwtcvzgsmhsoeoknaxydifuj"
                 "kfqltcvzgsmhpoelrnaxydifnj"
                 "qbqwtsvzgsmhpoelrnaxodifuj"
                 "kbqwwevzgsmypoelrnaxydifuj"
                 "kbqwtcuzgimhpoelrnaxydffuj"
                 "kxqwlcvzgsmhpoelrnaxyrifuj"
                 "nbqwtcvzgsmhpoelryaxyiifuj"
                 "kbqwtcvzgsmhhoxlreaxydifuj"
                 "mbqwtcvzfsmxpoelrnaxydifuj"
                 "kbqwttvzgsmhpoeqrnaxidifuj"
                 "kbqwtcvzgamhpielrnaxyiifuj"
                 "rfqwtcvzgsmhpoelrnaxydifun"
                 "kbpwtqvzgsmbpoelrnaxydifuj"
                 "kbqwtcvzgsmhpoqlroaxydifua"
                 "hbqwtcvzksmhpoelrnaxydbfuj"
                 "kaqutcvzgsmhpoelrnaxydiiuj"
                 "kbqctcvzgsnhpoelrcaxydifuj"
                 "kbqwtnvzgsmhpoelrnaxydqfoj"
                 "kbqwtcvzhsmhpoelrnaxydifyb"
                 "ubqwtcvcgsmhooelrnaxydifuj"
                 "kbqwtcvrgsmhpoelrnaxtdivuj"
                 "kbqwtcvzgsmhplelrnmxydifaj"
                 "ebqwlcvzghmhpoelrnaxydifuj"
                 "hbqwtcvzgsmhpoelrnaqyeifuj"
                 "kbqstcvzgsmeprelrnaxydifuj"
                 "kbqwtcvogsthpoelrnnxydifuj"
                 "ybqwtcvzgdmhpoelrnaxydufuj"
                 "kbqutcvzgsmhpoelrnaxydifgx"
                 "kbqwtcvzgsmhpozlunadydifuj"
                 "kkqwtcvzgsmhpuefrnaxydifuj"
                 "kbqrtcvzgsmhpoelrnaxcdifuq"
                 "kbqwtcvzjsmupoelrnaxydiluj"
                 "kbqwmcvzgsuhpoelrnaxydifhj"
                 "kbqwfcvzgsmhpoelrnaxydkzuj"
                 "kbqatcvzgsdhpoeyrnaxydifuj"
                 "kbtwtcvzusmhpoelrxaxydifuj"
                 "kbqwtcwzgsmhpoelrnaxysofuj"
                 "kbqqtcvmgsmhpoevrnaxydifuj"
                 "kbqwjcvzgsmhpoelrnaxydhuuj"
                 "mbdwtcvzgsmhpoelqnaxydifuj"
                 "kbqwtcvlgsmhpoelrdaxydifaj"
                 "kbqwtcvzgsmmpoelrlaxydnfuj"
                 "kbqwtchfggmhpoelrnaxydifuj"
                 "kbqqtcvzgsyhpoelrnaxyoifuj"
                 "knqwtcvzqsmupoelrnaxydifuj"
                 "kdqdtcvzgsmhpoelrnaxydmfuj"
                 "kbqwtcvzgsmhptelrnawyhifuj"
                 "kbqwtcvzgrmhpoeqrnaxydifuw"
                 "kbnxtcvzgsmhpoelrnauydifuj"
                 "kbqwacvsgsmhpoelrnaxydifgj"
                 "kbqwtcvzgsmhpperrnaxydifuc"
                 "gbqwtcvzgsqhxoelrnaxydifuj"
                 "kbqwtcvzgsmhpoeljgaxydifwj"
                 "kbqktcvzgsmhpotlrnatydifuj"
                 "bbqwtcvzgsmhpoilrnaxydjfuj"
                 "kbqwecvdgsmhpoelrnaxypifuj"
                 "keqwtcvzgemhpotlrnaxydifuj"
                 "kbqptcvzgsmvpoelrnaxydixuj"
                 "kbqwbctzgsmhpoelrnaxydifup"
                 "kbqwtcvzgszhpbelrnzxydifuj"
                 "mbqwtcvtgsmhpoeyrnaxydifuj"
                 "kbqwtcvzgsmhqcelrhaxydifuj"
                 "kbqotcvzgsmhooelrnazydifuj"
                 "kbqwtcvzgsmhpoelmpaxyiifuj"
                 "kbqwtcvwgsmypoclrnaxydifuj"
                 "kbqwtcvsgskhpoelrnaxykifuj"
                 "kbqwtcvzgszvpoelrnwxydifuj"
                 "kbqwtcvzgsmhpoejonaxydrfuj"
                 "kbqwtcvzgsmhkoelrnazyqifuj"
                 "kbzwtzvzgsmhptelrnaxydifuj"
                 "kbqwtcdzgsmhptelrnaxydiduj"
                 "kbqwtcvzgamhpoelrnakyzifuj"
                 "kbqwtcvzgsmhpoeonnaxydifxj"
                 "kbqwtcvzgsmhpoeranaxydifej"
                 "kbqwscvzgsmhpoelunaxydimuj"
                 "cbqwtcvzgsmhpoelrdaxydefuj"
                 "vbqwtcjzgsmhpoelrnaxydifua"
                 "kmqwtcvzksmhpoeljnaxydifuj"
                 "kbqwtcvzgsmppojlrnasydifuj"
                 "kaqwtcvfgsmhpoelrnaxydiauj"
                 "khqwccvzgsmhpoelrnaxydifud"
                 "vbqwtcvzrsmhpoelrhaxydifuj"
                 "kuqwtcvzgsmhpoelgnaiydifuj"
                 "kbqwtcvzdsmhpbelvnaxydifuj"
                 "kbowtcvzgnmhpoelrfaxydifuj"
                 "kbqwtcvsgsmhfoejrnaxydifuj"
                 "kbqwtcvzgskhtoelrnxxydifuj"
                 "kbqwtcvzgtmhpoevrnaxydivuj"
                 "bbqptcgzgsmhpoelrnaxydifuj"
                 "kbqwtpvzgsmnpoelhnaxydifuj"
                 "kbqwtovzgsmmpoelrnaxydifuw"
                 "kbqwtcvzgsihpwelrnaxydsfuj"
                 "kbqwtcvzggmhpollrnaxydifsj"
                 "kbqwtcjzgsmhpoelrnaxyxifub"
                 "ebqwtcvzgsmzpoelrnaaydifuj"
                 "kbqwtcvzusmhpoelrnqxydijuj"
                 "obqwtcvzgsghpoelrnaxydifkj"
                 "kbrwtcvzmdmhpoelrnaxydifuj"
                 "kbqwtcvzxsmhpoblrnhxydifuj"
                 "kbqwacvzgsahpoelrnaxydiguj"
                 "kyqwtcvzgsmipoelrnlxydifuj"
                 "kbbwtcvzgsmhboelpnaxydifuj"
                 "kbqwtcvzgsmhpoelrnaxhdosuj"
                 "kbqwtgvzgxmhpoelrnaxyrifuj"
                 "pbqwtsvzgsmhpoelrnabydifuj"
                 "kbqrtcvzgsmhpsblrnaxydifuj"
                 "kbqwtcvzgsmhpoexrnaaycifuj"
                 "kbqxtcvzgsjhkoelrnaxydifuj"
                 "kbqwtcvzgsmhpxelrnaxydifby"
                 "lbxwtcvzgsmdpoelrnaxydifuj"
                 "kbqwtcczgsmhpoklrnzxydifuj"
                 "zbqwtcvzgsmhpoelrbaxydifui"
                 "krqwtcvzbsmhpoelrjaxydifuj"
                 "kbkwtcvzgsmhpoelrnaxydiacj"
                 "kbqwtcvzgszhpseprnaxydifuj"
                 "kbxwtcvzxsmhpoesrnaxydifuj"
                 "kbqwdcvzgsmhpoelrbaxygifuj"
                 "kbqwthkzgsmhhoelrnaxydifuj"
                 "klqwtchzgamhpoelrnaxydifuj"
                 "obqwtcvzgsvcpoelrnaxydifuj"
                 "kblwtcvzgsmhpoelrnanydifuw"
                 "kbqwtrvzgsmhpoelynaxydifug"
                 "kbqwtcvzgsmhcoelmnaxydkfuj"
                 "kbqwtcvzgsmhpotlqoaxydifuj"
                 "kaqatcvzgsmhpoelrnaxyiifuj"
                 "kbqttcvwgsmhpoelrnaxydifgj"
                 "kpqwtcvzgsmhpwelynaxydifuj"
                 "kbqwucvzgsmhpyelrnaxyxifuj"
                 "kbqwucvzgsmhprelrnaxyfifuj"
                 "kbqwthvzgsmhphelrnaxylifuj"
                 "kbqwtcvzosmhdoelrnaxwdifuj"
                 "kbqwtxvsgsphpoelrnaxydifuj"
                 "koqwtcvfghmhpoelrnaxydifuj"
                 "kbtwicvzpsmhpoelrnaxydifuj"
                 "kbawtcvzgsmhmoelrnaxyiifuj"
                 "kbqwtcvzgslhpbelrnaxydifuk"
                 "kbqttcvzgsmypoelrnaxydifua"
                 "kbqwtcvrgqmhpnelrnaxydifuj"
                 "kbqwtcvzghmhpoekpnaxydifuj"
                 "kbqwtcvzgsmupoelrnaxidifui"
                 "kbqwtcvzgsmhpbelrnaxrdifux"])

(def day2-test-input ["abcdef"
                      "bababc"
                      "abbcde"
                      "abcccd"
                      "aabcdd"
                      "abcdee"
                      "ababab"])

(defn into-or-inc [m c]
  (update-in m [c] (fnil inc 0)))

(defn to-map-count [s]
  (let [arr (char-array s)]
    (reduce into-or-inc {} arr)))

(defn has-count [m c]
  (some #(= c %) (vals m)))

(defn has-two-count [m]
  (has-count m 2))

(defn has-three-count [m]
  (has-count m 3))

(defn day2-p1 [input]
  (let [m (map to-map-count input)
        two-counts (filter has-two-count m)
        three-counts (filter has-three-count m)]
    (* (count two-counts) (count three-counts))))

(day2-p1 day2-input)                                        ;7134

(def day2-test-input-2 ["abcde"
                        "fghij"
                        "klmno"
                        "pqrst"
                        "fguij"
                        "axcye"
                        "wvxyz"])

(defn diff-by-one [s1 s2]
  (loop [seq1 s1
         seq2 s2
         pos 0
         diff -1]
    (if (or (empty? seq1) (empty? seq2))
      diff
      (if (not= (first seq1) (first seq2))
        (if (not= -1 diff)
          nil
          (recur (rest seq1) (rest seq2) (+ pos 1) pos))
        (recur (rest seq1) (rest seq2) (+ pos 1) diff)))))

(defn diff-by-ones [word words]
  (loop [other (first words)
         words (rest words)]
    (let [diff (diff-by-one word other)]
      (if (or (not (nil? diff)) (empty? words))
        diff
        (recur (first words) (rest words))))))

(defn day2-p2 [seqs]
    (loop [word (first seqs)
           words (rest seqs)]
      (let [diff (diff-by-ones word words)]
        (if (not (nil? diff))
          (str (subs word 0 diff) (subs word (+ 1 diff)))
          (recur (first words) (rest words))))))

(day2-p2 day2-input)                                        ;kbqwtcvzhmhpoelrnaxydifyb