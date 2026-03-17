import Foundation
import Combine

/// Connects to gravity-server's terminal socket, receives NDJSON broadcasts,
/// and publishes state for the SwiftUI menu bar.
public class GravityMonitor: ObservableObject {
    @Published public var connected = false
    @Published public var projects: [ProjectInfo] = []
    @Published public var inboxItems: [InboxInfo] = []
    @Published public var iconState: MenuBarIconState = .disconnected

    public let stateManager = MenuBarStateManager()

    private var socketFD: Int32 = -1
    private var readSource: DispatchSourceRead?
    private var reconnectTimer: Timer?
    private var buffer = Data()

    private let socketPath: String

    private var healthTimer: Timer?
    private var lastResponseTime = Date()

    public init() {
        if let envPath = ProcessInfo.processInfo.environment["GRAVITY_TERMINAL_SOCK"] {
            socketPath = envPath
        } else {
            socketPath = NSString(string: "~/.local/state/gravity-terminal.sock").expandingTildeInPath
        }

        stateManager.onStateChange = { [weak self] in
            self?.syncFromStateManager()
        }

        connect()
    }

    deinit {
        disconnect()
    }

    /// Sync @Published properties from state manager
    private func syncFromStateManager() {
        connected = stateManager.connected
        projects = stateManager.projects
        inboxItems = stateManager.inboxItems
        iconState = stateManager.iconState
    }

    // MARK: - Connection

    public func reconnect() {
        disconnect()
        connect()
    }

    private func connect() {
        socketFD = Darwin.socket(AF_UNIX, SOCK_STREAM, 0)
        guard socketFD >= 0 else {
            scheduleReconnect()
            return
        }

        // Prevent SIGPIPE on writes to broken sockets
        var nosigpipe: Int32 = 1
        setsockopt(socketFD, SOL_SOCKET, SO_NOSIGPIPE, &nosigpipe, socklen_t(MemoryLayout<Int32>.size))

        var addr = sockaddr_un()
        addr.sun_family = sa_family_t(AF_UNIX)

        let pathBytes = socketPath.utf8CString
        guard pathBytes.count <= MemoryLayout.size(ofValue: addr.sun_path) else {
            NSLog("gravity-menubar: socket path too long")
            Darwin.close(socketFD)
            socketFD = -1
            return
        }

        withUnsafeMutablePointer(to: &addr.sun_path) { sunPathPtr in
            sunPathPtr.withMemoryRebound(to: CChar.self, capacity: pathBytes.count) { dest in
                pathBytes.withUnsafeBufferPointer { src in
                    _ = memcpy(dest, src.baseAddress!, pathBytes.count)
                }
            }
        }

        let addrLen = socklen_t(MemoryLayout<sockaddr_un>.size)
        let result = withUnsafePointer(to: &addr) { addrPtr in
            addrPtr.withMemoryRebound(to: sockaddr.self, capacity: 1) { sockaddrPtr in
                Darwin.connect(socketFD, sockaddrPtr, addrLen)
            }
        }

        guard result == 0 else {
            NSLog("gravity-menubar: connect failed to %@, retrying in 5s", socketPath)
            Darwin.close(socketFD)
            socketFD = -1
            scheduleReconnect()
            return
        }

        NSLog("gravity-menubar: connected to %@", socketPath)

        DispatchQueue.main.async {
            self.stateManager.setConnected(true)
            self.lastResponseTime = Date()
            // Start periodic overview refresh (10s heartbeat) with timeout detection
            self.healthTimer?.invalidate()
            self.healthTimer = Timer.scheduledTimer(withTimeInterval: 10.0, repeats: true) { [weak self] _ in
                guard let self = self else { return }
                // If no response received in 30s, treat as dead connection
                if Date().timeIntervalSince(self.lastResponseTime) > 30.0 {
                    NSLog("gravity-menubar: heartbeat timeout (no response in 30s), reconnecting")
                    self.disconnect()
                    self.scheduleReconnect()
                    return
                }
                self.stateManager.clearJustFinishedIfStale()
                self.sendRequest(TerminalRequest(type: "request.overview"))
            }
        }

        startReading()
        sendRequest(TerminalRequest(type: "request.overview"))
        sendRequest(TerminalRequest(type: "request.resync"))
    }

    private func disconnect() {
        reconnectTimer?.invalidate()
        reconnectTimer = nil
        healthTimer?.invalidate()
        healthTimer = nil
        readSource?.cancel()
        readSource = nil
        if socketFD >= 0 {
            Darwin.close(socketFD)
            socketFD = -1
        }
        DispatchQueue.main.async {
            self.stateManager.resetOnDisconnect()
        }
    }

    private func scheduleReconnect() {
        DispatchQueue.main.async {
            self.stateManager.setConnected(false)
            self.reconnectTimer?.invalidate()
            self.reconnectTimer = Timer.scheduledTimer(withTimeInterval: 5.0, repeats: false) { [weak self] _ in
                NSLog("gravity-menubar: attempting reconnect...")
                self?.connect()
            }
        }
    }

    // MARK: - Reading

    private func startReading() {
        let source = DispatchSource.makeReadSource(fileDescriptor: socketFD, queue: .global(qos: .utility))
        source.setEventHandler { [weak self] in
            self?.readAvailableData()
        }
        source.setCancelHandler { [weak self] in
            guard let self = self else { return }
            if self.socketFD >= 0 {
                Darwin.close(self.socketFD)
                self.socketFD = -1
            }
        }
        readSource = source
        source.resume()
    }

    private func readAvailableData() {
        var buf = [UInt8](repeating: 0, count: 8192)
        let n = Darwin.read(socketFD, &buf, buf.count)
        if n <= 0 {
            // Connection closed or error — close FD first to prevent cancel handler race
            NSLog("gravity-menubar: disconnected (EOF/error), scheduling reconnect")
            let fd = socketFD
            socketFD = -1  // Mark closed before cancel handler can fire
            if fd >= 0 {
                Darwin.close(fd)
            }
            readSource?.cancel()
            readSource = nil
            healthTimer?.invalidate()
            healthTimer = nil
            DispatchQueue.main.async {
                self.stateManager.setConnected(false)
            }
            scheduleReconnect()
            return
        }

        buffer.append(contentsOf: buf[0..<n])
        processBuffer()
    }

    private func processBuffer() {
        // Split on newlines (NDJSON)
        while let newlineIndex = buffer.firstIndex(of: UInt8(ascii: "\n")) {
            let lineData = buffer[buffer.startIndex..<newlineIndex]
            buffer = Data(buffer[buffer.index(after: newlineIndex)...])

            guard !lineData.isEmpty else { continue }

            do {
                let msg = try JSONDecoder().decode(ServerMessage.self, from: lineData)
                DispatchQueue.main.async {
                    self.lastResponseTime = Date()
                    self.stateManager.handleMessage(msg)
                    // Drain pending requests
                    for request in self.stateManager.pendingRequests {
                        self.sendRequest(request)
                    }
                    self.stateManager.pendingRequests.removeAll()
                }
            } catch {
                NSLog("gravity-menubar: JSON decode error: \(error)")
            }
        }
    }

    // MARK: - Writing

    private func sendRequest(_ request: TerminalRequest) {
        guard socketFD >= 0 else { return }
        do {
            var data = try JSONEncoder().encode(request)
            data.append(UInt8(ascii: "\n"))
            let written = data.withUnsafeBytes { ptr -> Int in
                Darwin.write(socketFD, ptr.baseAddress!, ptr.count)
            }
            if written < 0 {
                NSLog("gravity-menubar: write failed (errno=%d), reconnecting", errno)
                disconnect()
                scheduleReconnect()
            }
        } catch {
            NSLog("gravity-menubar: encode error: \(error)")
        }
    }
}
