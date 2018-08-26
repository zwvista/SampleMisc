import { TestBed, inject } from '@angular/core/testing';

import { TransformingService } from './transforming.service';

describe('TransformingService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [TransformingService]
    });
  });

  it('should be created', inject([TransformingService], (service: TransformingService) => {
    expect(service).toBeTruthy();
  }));
});
