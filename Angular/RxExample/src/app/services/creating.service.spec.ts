import { TestBed, inject } from '@angular/core/testing';

import { CreatingService } from './creating.service';

describe('CreatingService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [CreatingService]
    });
  });

  it('should be created', inject([CreatingService], (service: CreatingService) => {
    expect(service).toBeTruthy();
  }));
});
