(defn pow [a b]
  (reduce *' (repeat b a)))

;;;Problema 2
(defn even-fib [n]
  (->> [0 1]
    (iterate (fn [[a b]] [b (+ a b)]))
    (map first)
    (take (bigint n))
    (filter even?)
    (reduce +)))

(even-fib 34)

;;;Problem 4

(defn pal
  [s]
  (= (reverse (str s) ) (seq (str s))))

(apply max (filter pal
               (for
                   [a (range 100 1000)
                    b (range 100 1000)]
              (* a b))))

;;;Problem 5

(defn greatest-divisor
  [a b]
  (if (zero? b) a
    (recur b (mod a b))))

(defn lowest-divisor
  [a b]
  (/ (* a b) (greatest-divisor a b)))

(reduce #(lowest-divisor %1 %2) (range 1 21))

;;;Problem 6

(defn squared-gauss
  [n]
  (reduce + (map #(* % %) (range 1 (inc n)))))

(defn gauss1
  [n]
  (pow (/(* n (+ n 1))2) 2))

(- (gauss1 100) (squared-gauss 100))


;;;Problem 7

(defn is-prime
  [n]
  (empty?(filter #(= 0 (mod n %)) (range 2 n))))

(defn tillNPrime
  [n]
  (last(take n (filter #(is-prime %) (iterate inc 2)))))

(tillNPrime 10001)

;;;Problem 8

(defn digits-of [n]
  (map #(Integer/parseInt (str %)) (str n)))

(defn larget-product
  []
  (let [n 73167176531330624919225119674426574742355349194934
          96983520312774506326239578318016984801869478851843
          85861560789112949495459501737958331952853208805511
          12540698747158523863050715693290963295227443043557
          66896648950445244523161731856403098711121722383113
          62229893423380308135336276614282806444486645238749
          30358907296290491560440772390713810515859307960866
          70172427121883998797908792274921901699720888093776
          65727333001053367881220235421809751254540594752243
          52584907711670556013604839586446706324415722155397
          53697817977846174064955149290862569321978468622482
          83972241375657056057490261407972968652414535100474
          82166370484403199890008895243450658541227588666881
          16427171479924442928230863465674813919123162824586
          17866458359124566529476545682848912883142607690042
          24219022671055626321111109370544217506941658960408
          07198403850962455444362981230987879927244284909188
          84580156166097919133875499200524063689912560717606
          05886116467109405077541002256983155200055935729725
          71636269561882670428252483600823257530420752963450]
  (apply max(map #(apply * %) (partition 5 1 (digits-of n))))))

;;;Problem 10

(defn sum-primes
  "Returns the sum of primes below n"
  [n]
  (->>
   (iterate #(+ 1 %) 2)
   (filter is-prime ,)
   (take-while #(<= % n))
   (reduce + ,)))

(+ 2 (sum-primes 2000000))

;;;Problem 21

(defn divisors
  [n]
  (filter (comp zero? (partial rem n)) (range 1 (inc n))))


(divisors 28)

;;;Problem 13
(reduce + '(37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690))

;;;Problema 14

(defn collatz [n]
  (loop [a n cnt 0]
    (cond (= 1 a) {n,(inc cnt)}
          (even? a) (recur (/ a 2) (inc cnt))
          :else (recur (+ 1 (* 3 a)) (inc cnt)))))

(first (apply max-key  #(apply second %) (map collatz (range 1 1000001))))

;;;Problem 25

(defn index-of
  [item coll]
  (count (take-while(partial not= item)coll)))

(defn fibonacci []
  (map first (iterate (fn [[aN bN]] [bN (+ aN bN)]) [0N 1N])))

(index-of (first (drop-while #(< (count (str %)) 1000) (fibonacci))) (fibonacci))

;;;Problem 30
(defn digits
  [n]
  (map #(- (int %) 48) (str n)))

(defn fifth-power
  [n]
  (and (not (= n 1)) (= (apply + (map #(pow % 5) (digits n))) n)))

(reduce + (filter fifth-power (range 500000)))

;;;Problem 48

(list(reduce + (for
  [a (range 1 5)]
  (pow a a))))


