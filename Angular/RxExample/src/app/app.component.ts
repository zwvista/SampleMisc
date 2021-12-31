import { Component, ElementRef, ViewChild, AfterViewInit } from '@angular/core';
import { AggregateService } from './services/aggregate.service';
import { CombiningService } from './services/combining.service';
import { ConditionalService } from './services/conditional.service';
import { ConnectableService } from './services/connectable.service';
import { CreatingService } from './services/creating.service';
import { ErrorHandlingService } from './services/error-handling.service';
import { FilteringService } from './services/filtering.service';
import { PostService } from './services/post.service';
import { TransformingService } from './services/transforming.service';
import { ToService } from './services/to.service';
import { UtilityService } from './services/utility.service';
import { combineLatest, fromEvent, Observable } from 'rxjs';
import { map, pluck, startWith } from 'rxjs/operators';
import { MiscService } from './services/misc.service';
import {Post2Service} from './services/post2.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements AfterViewInit {

  @ViewChild('number1Ref', { static: true }) number1Ref: ElementRef;
  @ViewChild('number2Ref', { static: true }) number2Ref: ElementRef;
  @ViewChild('number3Ref', { static: true }) number3Ref: ElementRef;
  resultAsync: Observable<string>;

  number1 = '1';
  number2 = '2';
  number3 = '3';
  result: string;

  title = 'RxExample';
  constructor(public aggregateService: AggregateService,
              public combiningService: CombiningService,
              public conditionalService: ConditionalService,
              public connectableService: ConnectableService,
              public creatingService: CreatingService,
              public errorHandlingService: ErrorHandlingService,
              public filteringService: FilteringService,
              public postService: PostService,
              public post2Service: Post2Service,
              public transformingService: TransformingService,
              public toService: ToService,
              public utilityService: UtilityService,
              public miscService: MiscService) { }

  ngAfterViewInit() {
    const f = elemRef => fromEvent(elemRef.nativeElement, 'input')
      .pipe(pluck('target', 'value'), startWith((elemRef.nativeElement as HTMLInputElement).value));
    const g = s => Number(s) || 0;
    setTimeout(() => this.resultAsync = combineLatest(f(this.number1Ref), f(this.number2Ref), f(this.number3Ref))
      .pipe(map(results => String(g(results[0]) + g(results[1]) + g(results[2])))));
    this.onChangeNumber();
  }

  onChangeNumber() {
    const g = s => Number(s) || 0;
    setTimeout(() => this.result = String(g(this.number1) + g(this.number2) + g(this.number3)));
  }
}
