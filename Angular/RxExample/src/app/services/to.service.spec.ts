import { TestBed, inject } from '@angular/core/testing';

import { ToService } from './to.service';

describe('ToService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ToService]
    });
  });

  it('should be created', inject([ToService], (service: ToService) => {
    expect(service).toBeTruthy();
  }));
});
