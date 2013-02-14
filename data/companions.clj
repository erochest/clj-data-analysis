
(require '[clojure.string :as string])

(def input-data
  [{:given-name "Susan", :surname "Forman", :doctors [1]}
   {:given-name "Barbara", :surname "Wright", :doctors [1]}
   {:given-name "Ian", :surname "Chesterton", :doctors [1]}
   {:given-name "Vicki", :surname nil, :doctors [1]}
   {:given-name "Steven", :surname "Taylor", :doctors [1]}
   {:given-name "Katarina", :surname nil, :doctors [1]}
   {:given-name "Sara", :surname "Kingdom", :doctors [1]}
   {:given-name "Dodo", :surname "Chaplet", :doctors [1]}
   {:given-name "Polly", :surname nil, :doctors [1 2]}
   {:given-name "Ben", :surname "Jackson", :doctors [1 2]}
   {:given-name "Jamie", :surname "McCrimmon", :doctors [2]}
   {:given-name "Victoria", :surname "Waterfield", :doctors [2]}
   {:given-name "Zoe", :surname "Heriot", :doctors [2]}
   {:given-name "Brigadier", :surname "Lethbridge-Stewart", :doctors [2]}
   {:given-name "Liz", :surname "Shaw", :doctors [3]}
   {:given-name "Jo", :surname "Grant", :doctors [3]}
   {:given-name "Sarah Jane", :surname "Smith", :doctors [3 4 10]}
   {:given-name "Harry", :surname "Sullivan", :doctors [4]}
   {:given-name "Leela", :surname nil, :doctors [4]}
   {:given-name "K-9 Mark I", :surname nil, :doctors [4]}
   {:given-name "K-9 Mark II", :surname nil, :doctors [4]}
   {:given-name "Romana", :surname nil, :doctors [4]}
   {:given-name "Adric", :surname nil, :doctors [4 5]}
   {:given-name "Nyssa", :surname nil, :doctors [4 5]}
   {:given-name "Tegan", :surname "Jovanka", :doctors [4 5]}
   {:given-name "Vislor", :surname "Turlough", :doctors [5]}
   {:given-name "Kamelion", :surname nil, :doctors [5]}
   {:given-name "Peri", :surname "Brown", :doctors [5 6]}
   {:given-name "Melanie", :surname "Bush", :doctors [6 7]}
   {:given-name "Ace", :surname nil, :doctors [7]}
   {:given-name "Grace", :surname "Holloway", :doctors [8]}
   {:given-name "Rose", :surname "Tyler", :doctors [9 10]}
   {:given-name "Adam", :surname "Mitchell", :doctors [9]}
   {:given-name "Jack", :surname "Harkness", :doctors [9 10]}
   {:given-name "Mickey", :surname "Smith", :doctors [10]}
   {:given-name "Donna", :surname "Noble", :doctors [10]}
   {:given-name "Martha", :surname "Jones", :doctors [10]}
   {:given-name "Astrid", :surname "Peth", :doctors [10]}
   {:given-name "Jackson", :surname "Lake", :doctors [10]}
   {:given-name "Rosita", :surname "Farisi", :doctors [10]}
   {:given-name "Christina", :surname "de Souza", :doctors [10]}
   {:given-name "Adelaide", :surname "Brooke", :doctors [10]}
   {:given-name "Wilfred", :surname "Mott", :doctors [10]}
   {:given-name "Amy", :surname "Pond", :doctors [11]}
   {:given-name "Rory", :surname "Williams", :doctors [11]}
   {:given-name "River", :surname "Song", :doctors [11]}
   {:given-name "Craig", :surname "Owens", :doctors [11]}])

(def companion (map string/lower-case
                    (map :given-name input-data)))
(def full-name
  (map (fn [{:keys [given-name surname]}]
         [(string/lower-case given-name)
          (string/trim
            (string/join \space [given-name surname]))])
       input-data))
(def doctor
  (mapcat #(map (fn [d] [(string/lower-case (:given-name %)) d])
                (:doctors %))
          input-data))

(def actor
  [[1 "William Hartnell" "1963–66"]
   [2 "Patrick Troughton" "1966–69"]
   [3 "Jon Pertwee" "1970–74"]
   [4 "Tom Baker" "1974–81"]
   [5 "Peter Davison" "1981–84"]
   [6 "Colin Baker" "1984–86"]
   [7 "Sylvester McCoy" "1987–89, 1996"]
   [8 "Paul McGann" "1996"]
   [9 "Christopher Eccleston" "2005"]
   [10 "David Tennant" "2005–10"]
   [11 "Matt Smith" "2010–present"]])

