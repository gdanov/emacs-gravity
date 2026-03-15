// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "GravityMenuBar",
    platforms: [.macOS(.v14)],
    targets: [
        .executableTarget(
            name: "GravityMenuBar",
            path: "Sources"
        )
    ]
)
