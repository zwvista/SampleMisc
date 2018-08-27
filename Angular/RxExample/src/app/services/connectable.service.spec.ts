import { TestBed, inject } from '@angular/core/testing';

import { ConnectableService } from './connectable.service';

describe('ConnectableService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ConnectableService]
    });
  });

  it('should be created', inject([ConnectableService], (service: ConnectableService) => {
    expect(service).toBeTruthy();
  }));
});
