import UIKit
import RxSwift
import RxCocoa

class StopWatchViewController: UIViewController {
    let timerLabel: UILabel = {
        let label = UILabel()
        label.backgroundColor = .lightGray
        label.font = UIFont.italicSystemFont(ofSize: 40)
        label.textAlignment = NSTextAlignment.center
        return label
    }()

    let startStopButton: UIButton = {
        let button = UIButton()
        button.setTitleColor(.black, for: .normal)
        button.layer.cornerRadius = 50
        return button
    }()

    let resetButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = .lightGray
        button.setTitle("Reset", for: UIControl.State.normal)
        button.setTitleColor(.black, for: .normal)
        button.layer.cornerRadius = 50
        button.isHidden = true
        return button
    }()

    private var viewModel = StopWatchViewModel()
    private let disposeBag = DisposeBag()

    override func viewDidLoad() {
        super.viewDidLoad()
        setupSubView()
        bind()
        viewModel.isPauseTimer.accept(false)
    }
}

extension StopWatchViewController {
    func bind() {
        // if startStopButton is tapped
        // check the latest(current) value of isTimerWorking
        // and set isPauseTimer to the opposite value of isTimerWorking
        // isTimerWorking is just the driver of isPauseTimer
        // so isPauseTimer will be set to its own opposite value
        startStopButton.rx.tap.asSignal()
            .withLatestFrom(viewModel.isTimerWorking)
            .emit(onNext: { [weak self] isTimerWorking in
                self?.viewModel.isPauseTimer.accept(!isTimerWorking)
            })
            .disposed(by: disposeBag)

        // if resetButton is tapped
        // isResetButtonTapped will be set to a new Void value
        resetButton.rx.tap.asSignal()
            .emit(to: viewModel.isResetButtonTapped)
            .disposed(by: disposeBag)

        viewModel.isTimerWorking
            .drive(onNext: { [weak self] isWorking in
                if isWorking {
                    self?.startStopButton.backgroundColor = UIColor(red: 255/255, green: 110/255, blue: 134/255, alpha: 1)
                    self?.startStopButton.setTitle("Stop", for: UIControl.State.normal)
                } else {
                    self?.startStopButton.backgroundColor = UIColor(red: 173/255, green: 247/255, blue: 181/255, alpha: 1)
                    self?.startStopButton.setTitle("Start", for: UIControl.State.normal)
                }
            })
            .disposed(by: disposeBag)

        viewModel.timerText
            .drive(timerLabel.rx.text)
            .disposed(by: disposeBag)

        viewModel.isResetButtonHidden
            .drive(resetButton.rx.isHidden)
            .disposed(by: disposeBag)
    }

    func setupSubView() {
        view.backgroundColor = .white
        let view10Width = self.view.frame.size.width / 10
        let view20Height = self.view.frame.size.height / 20

        view.addSubview(timerLabel)
        timerLabel.translatesAutoresizingMaskIntoConstraints = false
        timerLabel.topAnchor.constraint(equalTo: view.topAnchor, constant: view20Height * 4).isActive = true
        timerLabel.centerXAnchor.constraint(equalTo: view.centerXAnchor).isActive = true
        timerLabel.widthAnchor.constraint(equalToConstant: view10Width * 7).isActive = true
        timerLabel.heightAnchor.constraint(equalToConstant: view20Height * 4).isActive = true

        view.addSubview(startStopButton)
        startStopButton.translatesAutoresizingMaskIntoConstraints = false
        startStopButton.topAnchor.constraint(equalTo: timerLabel.bottomAnchor,constant: view20Height * 3).isActive = true
        startStopButton.leadingAnchor.constraint(equalTo: view.leadingAnchor, constant: view10Width * 2).isActive = true
        startStopButton.widthAnchor.constraint(equalToConstant: view10Width * 2.5).isActive = true
        startStopButton.heightAnchor.constraint(equalToConstant: view20Height * 3).isActive = true

        view.addSubview(resetButton)
        resetButton.translatesAutoresizingMaskIntoConstraints = false
        resetButton.topAnchor.constraint(equalTo: timerLabel.bottomAnchor,constant: view20Height * 3).isActive = true
        resetButton.trailingAnchor.constraint(equalTo: view.trailingAnchor, constant: -(view10Width * 2)).isActive = true
        resetButton.widthAnchor.constraint(equalToConstant: view10Width * 2.5).isActive = true
        resetButton.heightAnchor.constraint(equalToConstant: view20Height * 3).isActive = true
    }
}
