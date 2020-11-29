import Foundation
import RxSwift
import RxCocoa

final class StopWatchViewModel {

    // MARK: - Input
    let isPauseTimer = PublishRelay<Bool>()
    var isResetButtonTapped = PublishRelay<Void>()

    // MARK: - Output
    let isTimerWorking: Driver<Bool>
    let timerText: Driver<String>
    let isResetButtonHidden: Driver<Bool>

    private let disposeBag = DisposeBag()
    private let totalTimeDuration = BehaviorRelay<Int>(value: 0)

    init() {
        isTimerWorking = isPauseTimer.asDriver(onErrorDriveWith: .empty())

        timerText = totalTimeDuration
            .map { String("\(Double($0) / 10)") }
            .asDriver(onErrorDriveWith: .empty())
        
        isResetButtonHidden = Observable.merge(isTimerWorking.asObservable(), isResetButtonTapped.map { _ in true }.asObservable())
            .skip(1)
            .asDriver(onErrorDriveWith: .empty())

        isTimerWorking.asObservable()
            .flatMapLatest { [weak self] isWorking -> Observable<Int> in
                if isWorking {
                    return Observable<Int>.interval(DispatchTimeInterval.milliseconds(100), scheduler: MainScheduler.instance)
                        .withLatestFrom(Observable<Int>.just(self?.totalTimeDuration.value ?? 0)) { ($0 + $1) }
                } else {
                    return Observable<Int>.just(self?.totalTimeDuration.value ?? 0)
                }
             }
            .bind(to: totalTimeDuration)
            .disposed(by: disposeBag)

        isResetButtonTapped.map { _ in 0 }
            .bind(to: totalTimeDuration)
            .disposed(by: disposeBag)
    }
}
