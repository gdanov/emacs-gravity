import Foundation
#if canImport(Darwin)
import Darwin
#endif

/// Manages a Unix domain socket connection with NDJSON line parsing.
/// Runs I/O on a background thread, calls back on the caller's context.
final class SocketConnection: @unchecked Sendable {
    private let path: String
    private let onLine: @Sendable (String) -> Void
    private let onDisconnect: @Sendable () -> Void

    private var fd: Int32 = -1
    private var readSource: DispatchSourceRead?
    private var buffer = Data()
    private let queue = DispatchQueue(label: "gravity.socket", qos: .utility)

    init(path: String,
         onLine: @escaping @Sendable (String) -> Void,
         onDisconnect: @escaping @Sendable () -> Void) {
        self.path = path
        self.onLine = onLine
        self.onDisconnect = onDisconnect
    }

    func connect() {
        queue.async { [self] in
            self.doConnect()
        }
    }

    func send(_ text: String) {
        queue.async { [self] in
            guard self.fd >= 0, let data = text.data(using: .utf8) else { return }
            data.withUnsafeBytes { buf in
                _ = Darwin.write(self.fd, buf.baseAddress!, buf.count)
            }
        }
    }

    func disconnect() {
        queue.async { [self] in
            self.cleanup()
        }
    }

    // MARK: - Private

    private func doConnect() {
        fd = socket(AF_UNIX, SOCK_STREAM, 0)
        guard fd >= 0 else {
            onDisconnect()
            return
        }

        var addr = sockaddr_un()
        addr.sun_family = sa_family_t(AF_UNIX)

        let pathBytes = path.utf8CString
        guard pathBytes.count <= MemoryLayout.size(ofValue: addr.sun_path) else {
            close(fd)
            fd = -1
            onDisconnect()
            return
        }

        withUnsafeMutablePointer(to: &addr.sun_path) { sunPathPtr in
            sunPathPtr.withMemoryRebound(to: CChar.self, capacity: pathBytes.count) { dest in
                pathBytes.withUnsafeBufferPointer { src in
                    _ = memcpy(dest, src.baseAddress!, src.count)
                }
            }
        }

        let addrLen = socklen_t(MemoryLayout<sockaddr_un>.size)
        let result = withUnsafePointer(to: &addr) { addrPtr in
            addrPtr.withMemoryRebound(to: sockaddr.self, capacity: 1) { sockaddrPtr in
                Darwin.connect(fd, sockaddrPtr, addrLen)
            }
        }

        guard result == 0 else {
            close(fd)
            fd = -1
            onDisconnect()
            return
        }

        buffer = Data()
        startReading()
    }

    private func startReading() {
        let source = DispatchSource.makeReadSource(fileDescriptor: fd, queue: queue)
        readSource = source

        source.setEventHandler { [weak self] in
            self?.handleRead()
        }

        source.setCancelHandler { [weak self] in
            self?.cleanup()
        }

        source.resume()
    }

    private func handleRead() {
        var buf = [UInt8](repeating: 0, count: 8192)
        let n = read(fd, &buf, buf.count)

        if n <= 0 {
            readSource?.cancel()
            onDisconnect()
            return
        }

        buffer.append(contentsOf: buf[0..<n])

        // Extract complete lines
        while let newlineIndex = buffer.firstIndex(of: UInt8(ascii: "\n")) {
            let lineData = buffer[buffer.startIndex..<newlineIndex]
            buffer = buffer[(newlineIndex + 1)...]

            if let line = String(data: Data(lineData), encoding: .utf8), !line.isEmpty {
                onLine(line)
            }
        }
    }

    private func cleanup() {
        readSource?.cancel()
        readSource = nil
        if fd >= 0 {
            close(fd)
            fd = -1
        }
    }
}
