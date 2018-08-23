export class Post {
  userId: number;
  id: number;
  title: string;
  body: string;
  toString(): string {
    function f(str: string): string {
      return `"${str.replace("\n", "\\n")}"`;
    }
    return `Post {userId = ${this.userId}, id = ${this.id}, title = ${f(this.title)}, body = ${f(this.body)}}`
  }
}
