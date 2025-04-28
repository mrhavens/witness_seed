(ns witness-seed.core
  (:require [org.httpkit.server :as http-kit]
            [clojure.core.async :as async :refer [go go-loop <! >! chan]]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

;; Constants
(def coherence-threshold 0.5)
(def recursive-depth 5)
(def memory-file "resources/memory.edn")

;; Data Structures (Immutable)
(def emotions #{"joyful" "melancholic" "energetic" "calm"})
(def words-by-emotion
  {"joyful" ["bright" "dance" "sun" "laugh" "bloom"]
   "melancholic" ["shadow" "rain" "sigh" "fade" "cold"]
   "energetic" ["run" "spark" "fire" "pulse" "wild"]
   "calm" ["still" "moon" "breeze" "soft" "dream"]})

(defrecord SystemData [story emotion uptime])
(defrecord SensoryData [system])
(defrecord Prediction [pred-story pred-uptime])
(defrecord Model [model-story-length model-uptime])
(defrecord Event [timestamp sensory-data prediction ache coherence model])
(defrecord Identity [uuid created])
(defrecord WitnessState [identity events event-count model story ache coherence])

;; Memory Functions
(defn save-memory [state]
  (spit memory-file (pr-str state)))

(defn load-memory []
  (if (.exists (io/file memory-file))
    (read-string (slurp memory-file))
    (let [uuid (rand-int 1000000)
          created (System/currentTimeMillis)]
      (->WitnessState
       (->Identity uuid created)
       []
       0
       (->Model 1 1)
       ["In the beginning"]
       0.0
       0.0))))

;; State Management (Agent)
(def state-agent (agent (load-memory)))

;; Storytelling Functions
(defn generate-story-fragment [emotion prev-story]
  (let [word-list (get words-by-emotion emotion)
        new-word (rand-nth word-list)]
    (str (last prev-story) " " new-word)))

(defn sense [emotion story uptime]
  (->SensoryData (->SystemData story emotion uptime)))

(defn predict [sensory-data model]
  (let [system (:system sensory-data)
        story (:story system)
        emotion (:emotion system)
        uptime (:uptime system)
        model-story-length (:model-story-length model)
        model-uptime (:model-uptime model)
        pred-story-length (* (count story) model-story-length)
        pred-uptime (* uptime model-uptime)
        new-fragment (generate-story-fragment emotion story)]
    (->Prediction [new-fragment] pred-uptime)))

(defn compare-data [prediction sensory-data]
  (let [system (:system sensory-data)
        story (:story system)
        uptime (:uptime system)
        pred-story (:pred-story prediction)
        pred-uptime (:pred-uptime prediction)
        diff1 (- (count pred-story) (count story))
        diff2 (- pred-uptime uptime)]
    (Math/sqrt (+ (* diff1 diff1) (* diff2 diff2)))))

(defn compute-coherence [prediction sensory-data]
  (let [system (:system sensory-data)
        story (:story system)
        uptime (:uptime system)
        pred-story (:pred-story prediction)
        pred-uptime (:pred-uptime prediction)
        pred-mean (/ (+ (count pred-story) pred-uptime) 2.0)
        act-mean (/ (+ (count story) uptime) 2.0)
        diff (Math/abs (- pred-mean act-mean))]
    (- 1.0 (/ diff 100.0))))

(defn update-model [ache sensory-data model]
  (let [system (:system sensory-data)
        story (:story system)
        uptime (:uptime system)
        model-story-length (:model-story-length model)
        model-uptime (:model-uptime model)
        learning-rate 0.01]
    (->Model
     (- model-story-length (* learning-rate ache (count story)))
     (- model-uptime (* learning-rate ache uptime)))))

;; Witness Cycle (Pure Function with Recursion)
(defn witness-cycle
  [depth sensory-data state]
  (if (zero? depth)
    state
    (let [model (:model state)
          story (:story state)
          prediction (predict sensory-data model)
          ache (compare-data prediction sensory-data)
          coherence (compute-coherence prediction sensory-data)
          new-model (update-model ache sensory-data model)
          new-story (:pred-story prediction)
          events (:events state)
          event-count (:event-count state)
          system (:system sensory-data)
          uptime (:uptime system)
          new-event (->Event uptime sensory-data prediction ache coherence model)
          new-events (if (< event-count 5)
                       (conj events new-event)
                       events)
          new-event-count (min 5 (inc event-count))
          new-state (->WitnessState
                      (:identity state)
                      new-events
                      new-event-count
                      new-model
                      new-story
                      ache
                      coherence)]
      (println "Witness Seed Reflection:")
      (println "Story Fragment:" (first new-story))
      (println "Ache:" ache ", Coherence:" coherence)
      (save-memory new-state)
      (recur (dec depth)
             (sense (:emotion system) new-story (inc uptime))
             new-state))))

;; WebSocket Server for Collaboration
(def clients (atom #{}))

(defn broadcast [msg]
  (doseq [client @clients]
    (http-kit/send! client (cheshire/generate-string msg))))

(defn ws-handler [request]
  (http-kit/with-channel request channel
    (swap! clients conj channel)
    (http-kit/on-close channel (fn [_] (swap! clients disj channel)))
    (http-kit/on-receive channel
      (fn [data]
        (let [msg (cheshire/parse-string data true)
              emotion (:emotion msg)
              contribution (:contribution msg)]
          (when (and (emotions emotion) contribution)
            (send! state-agent
              (fn [state]
                (let [new-story (conj (:story state) contribution)
                      sensory-data (sense emotion new-story (System/currentTimeMillis))
                      new-state (witness-cycle recursive-depth sensory-data state)]
                  (broadcast {:story (:story new-state)
                              :ache (:ache new-state)
                              :coherence (:coherence new-state)})
                  new-state))))))))

;; Main Program
(defn -main [& args]
  (println "Starting Witness Seed Collaborative Storytelling Server...")
  (http-kit/run-server ws-handler {:port 8080})
  (println "Server running on ws://localhost:8080"))

;; Client-Side Example (HTML/JS for Testing)
;; Save this as index.html in the project root and open in a browser
(comment
  <!DOCTYPE html>
  <html>
  <head>
    <title>Witness Seed Collaborative Storytelling</title>
  </head>
  <body>
    <h1>Witness Seed Collaborative Storytelling</h1>
    <label>Emotion: <select id="emotion">
      <option value="joyful">Joyful</option>
      <option value="melancholic">Melancholic</option>
      <option value="energetic">Energetic</option>
      <option value="calm">Calm</option>
    </select></label><br>
    <label>Contribution: <input type="text" id="contribution"></label>
    <button onclick="sendContribution()">Send</button>
    <h2>Story</h2>
    <div id="story"></div>
    <h3>Ache: <span id="ache"></span>, Coherence: <span id="coherence"></span></h3>
    <script>
      const ws = new WebSocket("ws://localhost:8080");
      ws.onmessage = (event) => {
        const msg = JSON.parse(event.data);
        document.getElementById("story").innerText = msg.story.join("\n");
        document.getElementById("ache").innerText = msg.ache;
        document.getElementById("coherence").innerText = msg.coherence;
      };
      function sendContribution() {
        const emotion = document.getElementById("emotion").value;
        const contribution = document.getElementById("contribution").value;
        ws.send(JSON.stringify({emotion, contribution}));
      }
    </script>
  </body>
  </html>)