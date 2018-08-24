import { Observable } from 'rxjs';
export {}; // export all

// https://stackoverflow.com/questions/30494708/how-can-i-create-a-class-that-extends-rx-observable-in-typescript/30500334
const RxObsConstructor = (<any> Observable);
RxObsConstructor.prototype.dump = function () {
  return this.subscribe(val => console.log('next: ' + val), err => console.log(err), () => console.log('complete'));
};
