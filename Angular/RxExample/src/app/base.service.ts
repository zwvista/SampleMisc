import { HttpClient } from '@angular/common/http';

export class BaseService {
  protected baseUrl = 'http://jsonplaceholder.typicode.com/';

  constructor(
    protected http: HttpClient) { }


  protected log(message: any) {
    console.log(message);
  }

}
