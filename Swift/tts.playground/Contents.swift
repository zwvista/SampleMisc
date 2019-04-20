import Cocoa

import PlaygroundSupport
PlaygroundPage.current.needsIndefiniteExecution = true

let voices = NSSpeechSynthesizer.availableVoices.map { v in (v, NSSpeechSynthesizer.attributes(forVoice: v)[NSSpeechSynthesizer.VoiceAttributeKey.localeIdentifier] as! String) }.sorted  { ($0.1, $0.0.rawValue) < ($1.1, $1.0.rawValue) }
for (k, v) in voices {
    print( "\(k) speaks \(v)" )
}

let synth = NSSpeechSynthesizer()
synth.setVoice(NSSpeechSynthesizer.VoiceName(rawValue: "com.apple.speech.synthesis.voice.yuna"))
synth.startSpeaking("저는 중국 사람이에요.")
DispatchQueue.main.asyncAfter(deadline: .now() + .seconds(4), execute: {
    synth.setVoice(NSSpeechSynthesizer.VoiceName(rawValue: "com.apple.speech.synthesis.voice.kyoko"))
    synth.startSpeaking("私は中国人です。")
    // Put your code which should be executed with a delay here
})

