import { TestBed } from '@angular/core/testing';

import { Post2Service } from './post2.service';

describe('Post2Service', () => {
  let service: Post2Service;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(Post2Service);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
