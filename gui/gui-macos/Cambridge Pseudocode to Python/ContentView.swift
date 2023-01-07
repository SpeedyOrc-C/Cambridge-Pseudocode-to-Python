//
//  ContentView.swift
//  Cambridge Pseudocode to Python
//
//  Created by 陈湛明 on 2023/1/6.
//

import SwiftUI
import AppKit
import Foundation

struct ContentView: View {
    @State private var pseudocodePath: String?
    @State private var noChosenFileAlert = NSAlert()
    @State private var inputOpenPanel = NSOpenPanel()
    @State private var syntaxErrorAlert = NSAlert()
    @State private var outputSavePanel = NSSavePanel()

    var body: some View {
        VStack {
            Text("program-title")
                .font(.title)
                .multilineTextAlignment(.center)
                .padding(.bottom, 20)
            
            Button("choose-pseudocode-file") {
                pickPseudocodeSourceFile()
            }
                .padding(.bottom, 5.0)

            Button("translate") {
                translatePseudocodeSourceFile()
            }
                .padding(.top, 5.0)
        }
            .padding(.all, 20.0)
            .onAppear {
                noChosenFileAlert.messageText = NSLocalizedString("no-chosen-file", comment: "")
                noChosenFileAlert.informativeText = NSLocalizedString("translate-after-choose", comment: "")

                inputOpenPanel.message = NSLocalizedString("choose-pseudocode-file", comment: "")
                inputOpenPanel.canChooseFiles = true
                inputOpenPanel.canChooseDirectories = false

                syntaxErrorAlert.messageText = NSLocalizedString("syntax-error", comment: "")
                syntaxErrorAlert.informativeText = NSLocalizedString("contain-syntax-error", comment: "")

                outputSavePanel.prompt = NSLocalizedString("save", comment: "")
                outputSavePanel.message = NSLocalizedString("save-as-python", comment: "")
            }
    }


    private func pickPseudocodeSourceFile() {
        inputOpenPanel.begin { response in
            if response != .OK { return }

            let inputUrl: URL = inputOpenPanel.url!
            do { let _: Data = try Data(contentsOf: inputUrl) } catch { }
            self.pseudocodePath = inputUrl.absoluteString.replacingOccurrences(of: "file://", with: "")
        }
    }


    public func translatePseudocodeSourceFile() {
        if pseudocodePath == nil {
            noChosenFileAlert.runModal()
            return
        }

        let process = Process()
        process.executableURL = Bundle.main.url(
            forResource: "campseudo-to-py",
            withExtension: nil)
        process.arguments = [pseudocodePath!, "-e"]

        let outputPipe = Pipe()
        process.standardOutput = outputPipe

        do {
            try process.run()
            process.waitUntilExit()
        } catch { return }
        
        if process.terminationStatus != 0 {
            syntaxErrorAlert.runModal()
            return
        }

        let data = outputPipe.fileHandleForReading.readDataToEndOfFile()

        if #available(macOS 13.0, *) {
            outputSavePanel.nameFieldStringValue =
            URL(filePath: pseudocodePath!).lastPathComponent + ".py"
        } else {
            outputSavePanel.nameFieldStringValue =
            URL(string: pseudocodePath!)!.lastPathComponent + ".py"
        }
        outputSavePanel.begin { response in
            if response != .OK { return }
            do { try data.write(to: outputSavePanel.url!) } catch { }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
            .frame(width: 300, height: 180, alignment: .center)
    }
}
