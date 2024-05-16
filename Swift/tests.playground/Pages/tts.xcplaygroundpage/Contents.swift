import Cocoa
import AVFoundation

import PlaygroundSupport
PlaygroundPage.current.needsIndefiniteExecution = true

let voices = AVSpeechSynthesisVoice.speechVoices().map { v in (v.identifier, v.language) }.sorted  { ($0.1, $0.0) < ($1.1, $1.0) }
for (k, v) in voices {
    print( "\(k) speaks \(v)" )
}

let speaker = AVSpeechSynthesizer()
let dialogue = AVSpeechUtterance(string: "저는 중국 사람이에요.")
dialogue.voice = AVSpeechSynthesisVoice(identifier: "com.apple.voice.compact.ko-KR.Yuna")
speaker.speak(dialogue)
DispatchQueue.main.asyncAfter(deadline: .now() + .seconds(4), execute: {
    let dialog2 = AVSpeechUtterance(string: "私は中国人です。")
    dialog2.voice = AVSpeechSynthesisVoice(identifier: "com.apple.voice.compact.ja-JP.Kyoko")
    speaker.speak(dialog2)
    // Put your code which should be executed with a delay here
})

