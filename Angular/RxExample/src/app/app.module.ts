import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';
import { HttpClientModule } from '@angular/common/http';

import { AggregateService } from './services/aggregate.service';
import { CreatingService } from './services/creating.service';
import { CombiningService } from './services/combining.service';
import { ConditionalService } from './services/conditional.service';
import { ConnectableService } from './services/connectable.service';
import { PostService } from './services/post.service';
import { ErrorHandlingService } from './services/error-handling.service';
import { FilteringService } from './services/filtering.service';
import { TransformingService } from './services/transforming.service';
import { UtilityService } from './services/utility.service';

@NgModule({
  declarations: [
    AppComponent,
  ],
  imports: [
    BrowserModule,
    HttpClientModule,
  ],
  providers: [
    AggregateService,
    CombiningService,
    ConditionalService,
    ConnectableService,
    CreatingService,
    ErrorHandlingService,
    FilteringService,
    PostService,
    TransformingService,
    UtilityService,
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
