import { TestBed, inject } from '@angular/core/testing';

import { CombiningService } from './combining.service';

describe('CombiningService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [CombiningService]
    });
  });

  it('should be created', inject([CombiningService], (service: CombiningService) => {
    expect(service).toBeTruthy();
  }));
});
