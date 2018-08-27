import { Component } from '@angular/core';
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
              public toService: ToService,
              public utilityService: UtilityService) { }
}
