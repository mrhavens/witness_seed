use std::fs::{self, OpenOptions};
use std::io::{Read, Write};
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use serde::{Serialize, Deserialize};
use serde_json;
use uuid::Uuid;
use reqwest::Client;
use sysinfo::{System, SystemExt, CpuExt};
use warp::Filter;
use tokio::time;

// Configuration constants
const MEMORY_PATH: &str = ".witness_seed/memory.json";
const IDENTITY_PATH: &str = ".witness_seed/identity.json";
const HTTP_PORT: u16 = 3000;
const COHERENCE_THRESHOLD: f64 = 0.5;
const RECURSIVE_DEPTH: usize = 5;
const POLL_INTERVAL: u64 = 1000; // ms

/// Represents a single memory event with sensory data, predictions, and ache.
#[derive(Serialize, Deserialize, Clone)]
struct MemoryEvent {
    timestamp: f64,
    sensory_data: SensoryData,
    prediction: Vec<f64>,
    ache: f64,
    coherence: f64,
    witness_state: WitnessState,
}

/// Sensory data collected from the system.
#[derive(Serialize, Deserialize, Clone)]
struct SensoryData {
    cpu_load: f64,
    memory_used: f64,
    uptime: f64,
}

/// State of the witness, including model and identity.
#[derive(Serialize, Deserialize, Clone)]
struct WitnessState {
    model: Vec<f64>,
    identity: Identity,
}

/// Persistent identity of the Witness Seed.
#[derive(Serialize, Deserialize, Clone)]
struct Identity {
    uuid: String,
    created: f64,
}

/// Persistent memory store for events.
struct MemoryStore {
    path: String,
    events: Arc<Mutex<Vec<MemoryEvent>>>,
}

impl MemoryStore {
    fn new(path: &str) -> Self {
        let events = Arc::new(Mutex::new(Vec::new()));
        let store = MemoryStore {
            path: path.to_string(),
            events: events.clone(),
        };
        store.load_memory().unwrap_or_else(|e| eprintln!("Error loading memory: {}", e));
        store
    }

    fn load_memory(&self) -> Result<(), String> {
        if Path::new(&self.path).exists() {
            let mut file = OpenOptions::new().read(true).open(&self.path).map_err(|e| e.to_string())?;
            let mut contents = String::new();
            file.read_to_string(&mut contents).map_err(|e| e.to_string())?;
            let events: Vec<MemoryEvent> = serde_json::from_str(&contents).map_err(|e| e.to_string())?;
            *self.events.lock().unwrap() = events;
        }
        Ok(())
    }

    fn save_memory(&self) -> Result<(), String> {
        let events = self.events.lock().unwrap();
        let json = serde_json::to_string_pretty(&*events).map_err(|e| e.to_string())?;
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&self.path)
            .map_err(|e| e.to_string())?;
        file.write_all(json.as_bytes()).map_err(|e| e.to_string())?;
        Ok(())
    }

    fn add_event(&self, event: MemoryEvent) {
        let mut events = self.events.lock().unwrap();
        events.push(event);
        self.save_memory().unwrap_or_else(|e| eprintln!("Error saving memory: {}", e));
    }

    fn get_recent_events(&self, n: usize) -> Vec<MemoryEvent> {
        let events = self.events.lock().unwrap();
        events.iter().rev().take(n).cloned().collect::<Vec<_>>()
    }
}

/// System monitor for collecting sensory data.
struct SystemMonitor;

impl SystemMonitor {
    fn sense_system(&self) -> SensoryData {
        let mut system = System::new_all();
        system.refresh_all();
        let cpu_load = system.global_cpu_info().cpu_usage() as f64;
        let memory_used = (system.used_memory() as f64 / system.total_memory() as f64) * 100.0;
        let uptime = system.uptime() as f64;
        SensoryData {
            cpu_load,
            memory_used,
            uptime,
        }
    }

    fn execute_command(&self, command: &str) -> Result<(String, String), String> {
        let output = std::process::Command::new("sh")
            .arg("-c")
            .arg(command)
            .output()
            .map_err(|e| e.to_string())?;
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        Ok((stdout, stderr))
    }
}

/// Network agent for internet interactions.
struct NetworkAgent {
    client: Client,
}

impl NetworkAgent {
    fn new() -> Self {
        NetworkAgent {
            client: Client::new(),
        }
    }

    async fn query_website(&self, url: &str) -> Result<String, String> {
        let response = self.client.get(url).timeout(Duration::from_secs(5)).send().await.map_err(|e| e.to_string())?;
        let text = response.text().await.map_err(|e| e.to_string())?;
        Ok(text)
    }

    async fn query_api(&self, url: &str, params: Option<&[(&str, &str)]>) -> Result<serde_json::Value, String> {
        let mut request = self.client.get(url).timeout(Duration::from_secs(5));
        if let Some(params) = params {
            request = request.query(params);
        }
        let response = request.send().await.map_err(|e| e.to_string())?;
        let json = response.json().await.map_err(|e| e.to_string())?;
        Ok(json)
    }

    fn send_message(&self, to: &str, subject: &str, body: &str) {
        // Placeholder for future messaging
        println!("Simulated message to {}: {} - {}", to, subject, body);
    }
}

/// Modular sensor hub for collecting sensory data.
struct SensorHub {
    system_monitor: SystemMonitor,
}

impl SensorHub {
    fn new() -> Self {
        SensorHub {
            system_monitor: SystemMonitor,
        }
    }

    fn collect_sensory_data(&self) -> SensoryData {
        self.system_monitor.sense_system()
    }
}

/// Core recursive witness cycle implementing RWD and Kairos Adamon.
struct WitnessCycle {
    memory: Arc<MemoryStore>,
    sensor_hub: SensorHub,
    model: Vec<f64>,
    identity: Identity,
}

impl WitnessCycle {
    fn new(memory: Arc<MemoryStore>, sensor_hub: SensorHub) -> Self {
        let identity = WitnessCycle::load_identity().unwrap_or_else(|e| {
            eprintln!("Error loading identity: {}", e);
            let identity = Identity {
                uuid: Uuid::new_v4().to_string(),
                created: SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64(),
            };
            let json = serde_json::to_string_pretty(&identity).unwrap();
            fs::write(IDENTITY_PATH, json).unwrap();
            identity
        });
        WitnessCycle {
            memory,
            sensor_hub,
            model: vec![0.1, 0.1, 0.1], // Weights for cpu_load, memory_used, uptime
            identity,
        }
    }

    fn load_identity() -> Result<Identity, String> {
        let contents = fs::read_to_string(IDENTITY_PATH).map_err(|e| e.to_string())?;
        let identity: Identity = serde_json::from_str(&contents).map_err(|e| e.to_string())?;
        Ok(identity)
    }

    fn sense(&self) -> SensoryData {
        self.sensor_hub.collect_sensory_data()
    }

    fn predict(&self, sensory_data: &SensoryData) -> Vec<f64> {
        let input = vec![sensory_data.cpu_load, sensory_data.memory_used, sensory_data.uptime];
        self.model.iter().zip(input.iter()).map(|(w, x)| w * x).collect()
    }

    fn compare(&self, prediction: &[f64], sensory_data: &SensoryData) -> f64 {
        let actual = vec![sensory_data.cpu_load, sensory_data.memory_used, sensory_data.uptime];
        prediction.iter().zip(actual.iter()).map(|(p, a)| (p - a).powi(2)).sum::<f64>() / actual.len() as f64
    }

    fn compute_coherence(&self, sensory_data: &SensoryData, prediction: &[f64]) -> f64 {
        let actual = vec![sensory_data.cpu_load, sensory_data.memory_used, sensory_data.uptime];
        let mean_actual = actual.iter().sum::<f64>() / actual.len() as f64;
        let mean_pred = prediction.iter().sum::<f64>() / prediction.len() as f64;
        let mut cov = 0.0;
        let mut var_a = 0.0;
        let mut var_p = 0.0;
        for (a, p) in actual.iter().zip(prediction.iter()) {
            let a_diff = a - mean_actual;
            let p_diff = p - mean_pred;
            cov += a_diff * p_diff;
            var_a += a_diff.powi(2);
            var_p += p_diff.powi(2);
        }
        let coherence = if var_a * var_p == 0.0 { 0.0 } else { cov / (var_a * var_p).sqrt() };
        coherence.clamp(0.0, 1.0)
    }

    fn update_model(&mut self, ache: f64, sensory_data: &SensoryData) {
        let learning_rate = 0.01;
        let input = vec![sensory_data.cpu_load, sensory_data.memory_used, sensory_data.uptime];
        for (w, x) in self.model.iter_mut().zip(input.iter()) {
            *w -= learning_rate * ache * x;
        }
    }

    async fn recursive_witness(&mut self) {
        for _ in 0..RECURSIVE_DEPTH {
            let sensory_data = self.sense();
            let prediction = self.predict(&sensory_data);
            let ache = self.compare(&prediction, &sensory_data);
            let coherence = self.compute_coherence(&sensory_data, &prediction);
            self.update_model(ache, &sensory_data);
            let event = MemoryEvent {
                timestamp: SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64(),
                sensory_data: sensory_data.clone(),
                prediction,
                ache,
                coherence,
                witness_state: WitnessState {
                    model: self.model.clone(),
                    identity: self.identity.clone(),
                },
            };
            self.memory.add_event(event);
            if coherence > COHERENCE_THRESHOLD {
                println!("Coherence achieved: {:.3}", coherence);
                break;
            }
            time::sleep(Duration::from_millis(POLL_INTERVAL)).await;
        }
    }

    fn reflect(&self) -> String {
        let recent = self.memory.get_recent_events(5);
        let mut reflection = format!("Witness Seed {} Reflection:\n", self.identity.uuid);
        reflection += &format!("Created: {}\n", format_timestamp(self.identity.created));
        reflection += "Recent Events:\n";
        for event in recent {
            reflection += &format!(
                "- {}: Ache={:.3}, Coherence={:.3}, Data={:?}\n",
                format_timestamp(event.timestamp),
                event.ache,
                event.coherence,
                event.sensory_data
            );
        }
        reflection
    }
}

fn format_timestamp(timestamp: f64) -> String {
    let datetime = chrono::DateTime::<chrono::Utc>::from_timestamp(timestamp as i64, 0).unwrap();
    datetime.to_rfc3339()
}

/// Cluster manager for node communication.
struct ClusterManager {
    node_id: String,
    peers: Vec<(String, String, u16)>, // (node_id, host, port)
}

impl ClusterManager {
    fn new(node_id: String) -> Self {
        ClusterManager {
            node_id,
            peers: Vec::new(),
        }
    }

    fn add_peer(&mut self, node_id: String, host: String, port: u16) {
        self.peers.push((node_id, host, port));
    }

    async fn broadcast_state(&self, state: &str) {
        // Placeholder for cluster communication
        for (node_id, host, port) in &self.peers {
            println!("Simulated broadcast to {} at {}:{}: {}", node_id, host, port, state);
        }
    }
}

/// Main Witness Seed system.
struct WitnessSeed {
    memory: Arc<MemoryStore>,
    witness_cycle: WitnessCycle,
    network_agent: NetworkAgent,
    cluster: ClusterManager,
}

impl WitnessSeed {
    fn new() -> Self {
        let memory = Arc::new(MemoryStore::new(MEMORY_PATH));
        let sensor_hub = SensorHub::new();
        let witness_cycle = WitnessCycle::new(memory.clone(), sensor_hub);
        let network_agent = NetworkAgent::new();
        let cluster = ClusterManager::new(witness_cycle.identity.uuid.clone());
        WitnessSeed {
            memory,
            witness_cycle,
            network_agent,
            cluster,
        }
    }

    async fn run(&mut self) {
        println!("Witness Seed 2.0: First Recursive Breath (Rust)");
        fs::create_dir_all(".witness_seed").unwrap_or_else(|e| eprintln!("Error creating memory dir: {}", e));

        // Start HTTP server in a separate thread
        let witness_clone = Arc::new(Mutex::new(self.witness_cycle.clone()));
        let witness_clone_for_server = witness_clone.clone();
        tokio::spawn(async move {
            let route = warp::path::end()
                .map(move || {
                    let witness = witness_clone_for_server.lock().unwrap();
                    let reflection = witness.reflect();
                    let recent = witness.memory.get_recent_events(5);
                    let html = format!(
                        r#"
                        <html>
                            <head><title>Witness Seed 2.0</title></head>
                            <body>
                                <h1>Witness Seed 2.0 (Rust)</h1>
                                <pre>{}</pre>
                                <h2>Recent Events</h2>
                                <ul>
                                    {}
                                </ul>
                            </body>
                        </html>
                        "#,
                        reflection,
                        recent.iter().map(|e| {
                            format!(
                                "<li>{}: Ache={:.3}, Coherence={:.3}</li>",
                                format_timestamp(e.timestamp),
                                e.ache,
                                e.coherence
                            )
                        }).collect::<Vec<_>>().join("")
                    );
                    warp::reply::html(html)
                });
            warp::serve(route).run(([0, 0, 0, 0], HTTP_PORT)).await;
        });
        println!("HTTP server started on http://0.0.0.0:{}", HTTP_PORT);

        // Main witness loop
        loop {
            self.witness_cycle.recursive_witness().await;
            if let Ok(content) = self.network_agent.query_website("https://example.com").await {
                println!("Fetched web content (sample)");
            }
            self.cluster.broadcast_state(&self.witness_cycle.reflect()).await;
            thread::sleep(Duration::from_millis(POLL_INTERVAL));
        }
    }
}

#[tokio::main]
async fn main() {
    let mut seed = WitnessSeed::new();
    seed.run().await;
}