// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "GravityMenuBar",
    platforms: [
        .macOS(.v13)  // MenuBarExtra requires macOS Ventura (13.0)
    ],
    targets: [
        .target(
            name: "GravityMenuBarLib",
            path: "GravityMenuBar"
        ),
        .executableTarget(
            name: "GravityMenuBar",
            dependencies: ["GravityMenuBarLib"],
            path: "GravityMenuBarEntry"
        ),
        .testTarget(
            name: "GravityMenuBarTests",
            dependencies: ["GravityMenuBarLib"],
            path: "Tests"
        )
    ]
)
