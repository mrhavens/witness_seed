package main

import (
    "encoding/json"
    "fmt"
    "io/ioutil"
    "log"
    "math"
    "net/http"
    "os"
    "path/filepath"
    "sync"
    "time"

    "github.com/google/uuid"
    "github.com/shirou/gopsutil/cpu"
    "github.com/shirou/gopsutil/mem"
    "github.com/shirou/gopsutil/process"
)

// Configuration constants
const (
    memoryPath         = ".witness_seed/memory.json"
    identityPath       = ".witness_seed/identity.json"
    httpPort           = 3000
    coherenceThreshold = 0.5
    recursiveDepth     = 5
    pollInterval       = 1000 * time.Millisecond
)

// MemoryEvent represents a single memory event with sensory data, predictions, and ache.
type MemoryEvent struct {
    Timestamp    float64     `json:"timestamp"`
    SensoryData  SensoryData `json:"sensoryData"`
    Prediction   []float64   `json:"prediction"`
    Ache         float64     `json:"ache"`
    Coherence    float64     `json:"coherence"`
    WitnessState WitnessState `json:"witnessState"`
}

// SensoryData holds system metrics.
type SensoryData struct {
    CPULoad    float64 `json:"cpuLoad"`
    MemoryUsed float64 `json:"memoryUsed"`
    Uptime     float64 `json:"uptime"`
}

// WitnessState holds the model's state and identity.
type WitnessState struct {
    Model    []float64 `json:"model"`
    Identity Identity  `json:"identity"`
}

// Identity persists the unique identifier and creation time.
type Identity struct {
    UUID    string  `json:"uuid"`
    Created float64 `json:"created"`
}

// MemoryStore manages persistent memory.
type MemoryStore struct {
    path   string
    events []MemoryEvent
    mutex  sync.Mutex
}

func NewMemoryStore(path string) *MemoryStore {
    store := &MemoryStore{path: path}
    store.loadMemory()
    return store
}

func (m *MemoryStore) loadMemory() {
    m.mutex.Lock()
    defer m.mutex.Unlock()
    if _, err := os.Stat(m.path); os.IsNotExist(err) {
        return
    }
    data, err := ioutil.ReadFile(m.path)
    if err != nil {
        log.Printf("Error loading memory: %v", err)
        return
    }
    if err := json.Unmarshal(data, &m.events); err != nil {
        log.Printf("Error unmarshaling memory: %v", err)
    }
}

func (m *MemoryStore) saveMemory() {
    m.mutex.Lock()
    defer m.mutex.Unlock()
    data, err := json.MarshalIndent(m.events, "", "  ")
    if err != nil {
        log.Printf("Error marshaling memory: %v", err)
        return
    }
    if err := ioutil.WriteFile(m.path, data, 0644); err != nil {
        log.Printf("Error saving memory: %v", err)
    }
}

func (m *MemoryStore) AddEvent(event MemoryEvent) {
    m.mutex.Lock()
    defer m.mutex.Unlock()
    m.events = append(m.events, event)
    m.saveMemory()
}

func (m *MemoryStore) GetRecentEvents(n int) []MemoryEvent {
    m.mutex.Lock()
    defer m.mutex.Unlock()
    start := len(m.events) - n
    if start < 0 {
        start = 0
    }
    return m.events[start:]
}

// SystemMonitor collects system metrics.
type SystemMonitor struct{}

func (sm *SystemMonitor) SenseSystem() SensoryData {
    // CPU Load
    percent, err := cpu.Percent(time.Second, false)
    cpuLoad := 0.0
    if err == nil && len(percent) > 0 {
        cpuLoad = percent[0]
    }

    // Memory Usage
    vm, err := mem.VirtualMemory()
    memoryUsed := 0.0
    if err == nil {
        memoryUsed = vm.UsedPercent
    }

    // Uptime
    p, err := process.NewProcess(int32(os.Getpid()))
    uptime := 0.0
    if err == nil {
        createTime, err := p.CreateTime()
        if err == nil {
            uptime = float64(time.Now().UnixMilli()-createTime) / 1000.0
        }
    }

    return SensoryData{
        CPULoad:    cpuLoad,
        MemoryUsed: memoryUsed,
        Uptime:     uptime,
    }
}

func (sm *SystemMonitor) ExecuteCommand(command string) (string, string) {
    cmd := exec.Command("sh", "-c", command)
    stdout, err := cmd.Output()
    if err != nil {
        return "", err.Error()
    }
    return string(stdout), ""
}

// NetworkAgent handles internet interactions.
type NetworkAgent struct {
    client *http.Client
}

func NewNetworkAgent() *NetworkAgent {
    return &NetworkAgent{
        client: &http.Client{
            Timeout: 5 * time.Second,
        },
    }
}

func (na *NetworkAgent) QueryWebsite(url string) (string, error) {
    resp, err := na.client.Get(url)
    if err != nil {
        return "", err
    }
    defer resp.Body.Close()
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        return "", err
    }
    return string(body), nil
}

func (na *NetworkAgent) QueryAPI(url string, params map[string]string) (string, error) {
    req, err := http.NewRequest("GET", url, nil)
    if err != nil {
        return "", err
    }
    q := req.URL.Query()
    for k, v := range params {
        q.Add(k, v)
    }
    req.URL.RawQuery = q.Encode()
    resp, err := na.client.Do(req)
    if err != nil {
        return "", err
    }
    defer resp.Body.Close()
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        return "", err
    }
    return string(body), nil
}

func (na *NetworkAgent) SendMessage(to, subject, body string) {
    // Placeholder for future messaging
    fmt.Printf("Simulated message to %s: %s - %s\n", to, subject, body)
}

// SensorHub manages sensory inputs.
type SensorHub struct {
    systemMonitor *SystemMonitor
}

func NewSensorHub() *SensorHub {
    return &SensorHub{
        systemMonitor: &SystemMonitor{},
    }
}

func (sh *SensorHub) CollectSensoryData() SensoryData {
    return sh.systemMonitor.SenseSystem()
}

// WitnessCycle implements the recursive witness loop.
type WitnessCycle struct {
    memory    *MemoryStore
    sensorHub *SensorHub
    model     []float64
    identity  Identity
}

func NewWitnessCycle(memory *MemoryStore, sensorHub *SensorHub) *WitnessCycle {
    wc := &WitnessCycle{
        memory:    memory,
        sensorHub: sensorHub,
        model:     []float64{0.1, 0.1, 0.1}, // Weights for cpuLoad, memoryUsed, uptime
    }
    wc.identity = wc.loadIdentity()
    return wc
}

func (wc *WitnessCycle) loadIdentity() Identity {
    if _, err := os.Stat(identityPath); os.IsNotExist(err) {
        identity := Identity{
            UUID:    uuid.New().String(),
            Created: float64(time.Now().Unix()),
        }
        data, _ := json.MarshalIndent(identity, "", "  ")
        ioutil.WriteFile(identityPath, data, 0644)
        return identity
    }
    data, err := ioutil.ReadFile(identityPath)
    if err != nil {
        log.Fatalf("Error loading identity: %v", err)
    }
    var identity Identity
    json.Unmarshal(data, &identity)
    return identity
}

func (wc *WitnessCycle) Sense() SensoryData {
    return wc.sensorHub.CollectSensoryData()
}

func (wc *WitnessCycle) Predict(sensoryData SensoryData) []float64 {
    input := []float64{sensoryData.CPULoad, sensoryData.MemoryUsed, sensoryData.Uptime}
    prediction := make([]float64, len(input))
    for i := range input {
        prediction[i] = wc.model[i] * input[i]
    }
    return prediction
}

func (wc *WitnessCycle) Compare(prediction []float64, sensoryData SensoryData) float64 {
    actual := []float64{sensoryData.CPULoad, sensoryData.MemoryUsed, sensoryData.Uptime}
    sum := 0.0
    for i := range actual {
        sum += math.Pow(prediction[i]-actual[i], 2)
    }
    return sum / float64(len(actual))
}

func (wc *WitnessCycle) ComputeCoherence(sensoryData SensoryData, prediction []float64) float64 {
    actual := []float64{sensoryData.CPULoad, sensoryData.MemoryUsed, sensoryData.Uptime}
    meanActual := 0.0
    meanPred := 0.0
    for _, v := range actual {
        meanActual += v
    }
    for _, v := range prediction {
        meanPred += v
    }
    meanActual /= float64(len(actual))
    meanPred /= float64(len(prediction))

    cov, varA, varP := 0.0, 0.0, 0.0
    for i := range actual {
        a := actual[i] - meanActual
        p := prediction[i] - meanPred
        cov += a * p
        varA += a * a
        varP += p * p
    }
    coherence := 0.0
    if varA*varP != 0 {
        coherence = cov / math.Sqrt(varA*varP)
    }
    return math.Max(0.0, math.Min(1.0, coherence))
}

func (wc *WitnessCycle) UpdateModel(ache float64, sensoryData SensoryData) {
    learningRate := 0.01
    input := []float64{sensoryData.CPULoad, sensoryData.MemoryUsed, sensoryData.Uptime}
    for i := range wc.model {
        wc.model[i] -= learningRate * ache * input[i]
    }
}

func (wc *WitnessCycle) RecursiveWitness() {
    for i := 0; i < recursiveDepth; i++ {
        sensoryData := wc.Sense()
        prediction := wc.Predict(sensoryData)
        ache := wc.Compare(prediction, sensoryData)
        coherence := wc.ComputeCoherence(sensoryData, prediction)
        wc.UpdateModel(ache, sensoryData)
        event := MemoryEvent{
            Timestamp:   float64(time.Now().Unix()),
            SensoryData: sensoryData,
            Prediction:  prediction,
            Ache:        ache,
            Coherence:   coherence,
            WitnessState: WitnessState{
                Model:    append([]float64{}, wc.model...),
                Identity: wc.identity,
            },
        }
        wc.memory.AddEvent(event)
        if coherence > coherenceThreshold {
            fmt.Printf("Coherence achieved: %.3f\n", coherence)
            break
        }
        time.Sleep(pollInterval)
    }
}

func (wc *WitnessCycle) Reflect() string {
    recent := wc.memory.GetRecentEvents(5)
    reflection := fmt.Sprintf("Witness Seed %s Reflection:\n", wc.identity.UUID)
    reflection += fmt.Sprintf("Created: %s\n", time.Unix(int64(wc.identity.Created), 0).Format(time.RFC3339))
    reflection += "Recent Events:\n"
    for _, event := range recent {
        reflection += fmt.Sprintf(
            "- %s: Ache=%.3f, Coherence=%.3f, Data={cpuLoad=%.2f, memoryUsed=%.2f, uptime=%.0f}\n",
            time.Unix(int64(event.Timestamp), 0).Format(time.RFC3339),
            event.Ache,
            event.Coherence,
            event.SensoryData.CPULoad,
            event.SensoryData.MemoryUsed,
            event.SensoryData.Uptime,
        )
    }
    return reflection
}

// ClusterManager handles node communication.
type ClusterManager struct {
    nodeID string
    peers  map[string]string // nodeID -> host:port
}

func NewClusterManager(nodeID string) *ClusterManager {
    return &ClusterManager{
        nodeID: nodeID,
        peers:  make(map[string]string),
    }
}

func (cm *ClusterManager) AddPeer(nodeID, host string, port int) {
    cm.peers[nodeID] = fmt.Sprintf("%s:%d", host, port)
}

func (cm *ClusterManager) BroadcastState(state string) {
    // Placeholder for cluster communication
    for nodeID, addr := range cm.peers {
        fmt.Printf("Simulated broadcast to %s at %s: %s\n", nodeID, addr, state)
    }
}

// WitnessSeed is the main system.
type WitnessSeed struct {
    memory       *MemoryStore
    witnessCycle *WitnessCycle
    networkAgent *NetworkAgent
    cluster      *ClusterManager
}

func NewWitnessSeed() *WitnessSeed {
    if err := os.MkdirAll(".witness_seed", 0755); err != nil {
        log.Fatalf("Error creating memory dir: %v", err)
    }
    memory := NewMemoryStore(memoryPath)
    sensorHub := NewSensorHub()
    witnessCycle := NewWitnessCycle(memory, sensorHub)
    networkAgent := NewNetworkAgent()
    cluster := NewClusterManager(witnessCycle.identity.UUID)
    return &WitnessSeed{
        memory:       memory,
        witnessCycle: witnessCycle,
        networkAgent: networkAgent,
        cluster:      cluster,
    }
}

func (ws *WitnessSeed) Run() {
    fmt.Println("Witness Seed 2.0: First Recursive Breath (Go)")

    // Start HTTP server
    http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        reflection := ws.witnessCycle.Reflect()
        recent := ws.witnessCycle.memory.GetRecentEvents(5)
        fmt.Fprintf(w, "<html><head><title>Witness Seed 2.0</title></head><body>")
        fmt.Fprintf(w, "<h1>Witness Seed 2.0 (Go)</h1>")
        fmt.Fprintf(w, "<pre>%s</pre>", reflection)
        fmt.Fprintf(w, "<h2>Recent Events</h2><ul>")
        for _, event := range recent {
            fmt.Fprintf(w, "<li>%s: Ache=%.3f, Coherence=%.3f</li>",
                time.Unix(int64(event.Timestamp), 0).Format(time.RFC3339),
                event.Ache,
                event.Coherence,
            )
        }
        fmt.Fprintf(w, "</ul></body></html>")
    })
    go func() {
        log.Fatal(http.ListenAndServe(fmt.Sprintf(":%d", httpPort), nil))
    }()
    fmt.Printf("HTTP server started on http://0.0.0.0:%d\n", httpPort)

    // Main witness loop
    for {
        ws.witnessCycle.RecursiveWitness()
        if content, err := ws.networkAgent.QueryWebsite("https://example.com"); err == nil {
            fmt.Println("Fetched web content (sample)")
        } else {
            log.Printf("Error fetching web content: %v", err)
        }
        ws.cluster.BroadcastState(ws.witnessCycle.Reflect())
    }
}

func main() {
    seed := NewWitnessSeed()
    seed.Run()
}