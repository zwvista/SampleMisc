export class Post {
  userId: number;
  id: number;
  title: string;
  body: string;
  toString(): string {
    const f = (str: String) => `"${str.replace(/\n/g, '\\n')}"`;
    return `Post {userId = ${this.userId}, id = ${this.id}, title = ${f(this.title)}, body = ${f(this.body)}}`;
  }
}
