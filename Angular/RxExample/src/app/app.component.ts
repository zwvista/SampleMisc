import { Component } from '@angular/core';
import { PostService } from './services/post.service';
import { CreatingService } from './services/creating.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'RxExample';
  constructor(public postService: PostService,
              public creatingService: CreatingService) { }
}
