import { Component } from '@angular/core';
import { PostService } from './services/post.service';
import { CreatingService } from './services/creating.service';
import { TransformingService } from './services/transforming.service';
import {AggregateService} from './services/aggregate.service';
import {CombiningService} from './services/combining.service';
import {ConditionalService} from './services/conditional.service';
import {ConnectableService} from './services/connectable.service';
import {ErrorHandlingService} from './services/error-handling.service';
import {FilteringService} from './services/filtering.service';
import {UtilityService} from './services/utility.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'RxExample';
  constructor(public aggregateService: AggregateService,
              public combiningService: CombiningService,
              public conditionalService: ConditionalService,
              public connectableService: ConnectableService,
              public creatingService: CreatingService,
              public errorHandlingService: ErrorHandlingService,
              public filteringService: FilteringService,
              public postService: PostService,
              public transformingService: TransformingService,
              public utilityService: UtilityService) { }
}
