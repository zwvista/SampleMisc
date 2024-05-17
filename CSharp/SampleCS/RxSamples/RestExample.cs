using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

using System.Reactive.Linq;
using System.Reactive.Threading.Tasks;

using Newtonsoft.Json;
using RestSharp;
using RestSharp.Serializers.NewtonsoftJson;
using Refit;
using System.Reflection;

namespace RxSamples
{
    public class Post
    {
        [JsonProperty("userId")]
        public int UserId { get; set; }
        [JsonProperty("id")]
        public int Id { get; set; }
        [JsonProperty("title")]
        public string Title { get; set; } = "";
        [JsonProperty("body")]
        public string Body { get; set; } = "";

        public override string ToString() =>
            $"Post {{userId = {UserId}, id = {Id}, title = \"{Title}\", body = \"{Body.Replace("\n", "\\n")}\"}}";
    }

    public class PostDataStoreByTask
    {
        HttpClient client = new HttpClient
        {
            BaseAddress = new Uri("https://jsonplaceholder.typicode.com/")
        };

        public async Task<string> GetPostAsString(int id) =>
            await client.GetStringAsync($"posts/{id}");

        public async Task<Post> GetPostAsJson(int id)
        {
            var json = await client.GetStringAsync($"posts/{id}");
            var item = await Task.Run(() => JsonConvert.DeserializeObject<Post>(json)!);
            return item;
        }

        public async Task<IEnumerable<Post>> GetPosts(int n)
        {
            IEnumerable<Post> items = new List<Post>();
            var json = await client.GetStringAsync("posts");
            items = await Task.Run(() => JsonConvert.DeserializeObject<IEnumerable<Post>>(json)!);
            return items.Take(n);
        }

        private StringContent GetStringContent(Post item) =>
        new StringContent(JsonConvert.SerializeObject(item), Encoding.UTF8, "application/json");

        public async Task<Post> CreatePost(Post item)
        {
            var response = await client.PostAsync("posts", GetStringContent(item));
            var json = await response.Content.ReadAsStringAsync();
            var item2 = await Task.Run(() => JsonConvert.DeserializeObject<Post>(json)!);
            return item2;
        }

        public async Task<Post> UpdatePost(Post item)
        {
            var response = await client.PutAsync($"posts/{item.Id}", GetStringContent(item));
            var json = await response.Content.ReadAsStringAsync();
            var item2 = await Task.Run(() => JsonConvert.DeserializeObject<Post>(json)!);
            return item2;
        }

        public async Task<string> DeletePost(int id)
        {
            var response = await client.DeleteAsync($"posts/{id}");
            var json = await response.Content.ReadAsStringAsync();
            return json;
        }
    }

    public class PostDataStoreByRx
    {
        HttpClient client = new HttpClient
        {
            BaseAddress = new Uri("https://jsonplaceholder.typicode.com/")
        };

        public IObservable<string> GetPostAsString(int id) =>
            client.GetStringAsync($"posts/{id}").ToObservable();

        public IObservable<Post> GetPostAsJson(int id) =>
            client.GetStringAsync($"posts/{id}").ToObservable()
            .Select(json => JsonConvert.DeserializeObject<Post>(json)!);

        public IObservable<Post> GetPosts(int n) =>
            client.GetStringAsync("posts").ToObservable()
            .SelectMany(json => JsonConvert.DeserializeObject<IEnumerable<Post>>(json)!)
            .Take(n);

        private StringContent GetStringContent(Post item) =>
        new StringContent(JsonConvert.SerializeObject(item), Encoding.UTF8, "application/json");

        public IObservable<Post> CreatePost(Post item) =>
            client.PostAsync("posts", GetStringContent(item)).ToObservable()
            .SelectMany(response => response.Content.ReadAsStringAsync().ToObservable()
                .Select(json => JsonConvert.DeserializeObject<Post>(json)!));

        public IObservable<Post> UpdatePost(Post item) =>
            client.PutAsync($"posts/{item.Id}", GetStringContent(item)).ToObservable()
            .SelectMany(response => response.Content.ReadAsStringAsync().ToObservable()
                .Select(json => JsonConvert.DeserializeObject<Post>(json)!));

        public IObservable<string> DeletePost(int id) =>
            client.DeleteAsync($"posts/{id}").ToObservable()
            .SelectMany(response => response.Content.ReadAsStringAsync().ToObservable());
    }

    public class PostDataStoreByRestSharp
    {
        RestClient client = new RestClient("https://jsonplaceholder.typicode.com/", configureSerialization: s => s.UseNewtonsoftJson());
        public IObservable<string> GetPostAsString(int id)
        {
            var request = new RestRequest($"posts/{id}", Method.Get);
            return client.ExecuteAsync<string>(request).ToObservable().Select(o => o.Content!);
        }
        public IObservable<Post> GetPostAsJson(int id)
        {
            var request = new RestRequest($"posts/{id}", Method.Get);
            return client.ExecuteAsync<Post>(request).ToObservable().Select(o => o.Data!);
        }

        public IObservable<Post> GetPosts(int n)
        {
            var request = new RestRequest($"posts", Method.Get);
            return client.ExecuteAsync<IEnumerable<Post>>(request).ToObservable().SelectMany(o => o.Data!).Take(n);
        }

        public IObservable<Post> CreatePost(Post item)
        {
            var request = new RestRequest($"posts", Method.Post);
            request.AddJsonBody(item);
            return client.ExecuteAsync<Post>(request).ToObservable().Select(o => o.Data!);
        }

        public IObservable<Post> UpdatePost(Post item)
        {
            var request = new RestRequest($"posts/{item.Id}", Method.Put);
            request.AddJsonBody(item);
            return client.ExecuteAsync<Post>(request).ToObservable().Select(o => o.Data!);
        }

        public IObservable<string> DeletePost(int id)
        {
            var request = new RestRequest($"posts/{id}", Method.Delete);
            return client.ExecuteAsync<string>(request).ToObservable().Select(o => o.Content!);
        }
    }

    public class PostDataStoreByRefit
    {
        public interface IPost
        {
            [Get("/posts/{id}")]
            Task<string> GetPostAsString(int id);
            [Get("/posts/{id}")]
            Task<Post> GetPostAsJson(int id);
            [Get("/posts")]
            Task<List<Post>> GetPosts();
            [Post("/posts")]
            Task<Post> CreatePost([Body] Post item);
            [Put("/posts/{item.Id}")]
            Task<Post> UpdatePost([Body] Post item);
            [Delete("/posts/{id}")]
            Task<string> DeletePost(int id);
        }

        IPost client = RestService.For<IPost>("https://jsonplaceholder.typicode.com");
        public async Task<string> GetPostAsString(int id) =>
            await client.GetPostAsString(id);
        public async Task<Post> GetPostAsJson(int id) =>
            await client.GetPostAsJson(id);

        public async Task<IEnumerable<Post>> GetPosts(int n) =>
            (await client.GetPosts()).Take(n);

        public async Task<Post> CreatePost(Post item) =>
            await client.CreatePost(item);

        public async Task<Post> UpdatePost(Post item) =>
            await client.UpdatePost(item);

        public async Task<string> DeletePost(int id) =>
            await client.DeletePost(id);
    }

    public class RestExample
    {
        public static void Test()
        {
            TestByTask();
            TestByRx();
            TestByRestSharp();
            TestByRefit();
        }

        private static void TestByTask()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var dataStore = new PostDataStoreByTask();
            Console.WriteLine(dataStore.GetPostAsString(1).Result);
            Console.WriteLine(dataStore.GetPostAsJson(1).Result);
            dataStore.GetPosts(2).Result.ToList().ForEach(Console.WriteLine);
            Console.WriteLine(dataStore.CreatePost(new Post
            {
                UserId = 101,
                Id = 0,
                Title = "test title",
                Body = "test body"
            }).Result);
            Console.WriteLine(dataStore.UpdatePost(new Post
            {
                UserId = 101,
                Id = 1,
                Title = "test title",
                Body = "test body"
            }).Result);
            Console.WriteLine(dataStore.DeletePost(1).Result);
        }

        private static void TestByRx()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var dataStore = new PostDataStoreByRx();
            dataStore.GetPostAsString(1).Subscribe(Console.WriteLine);
            dataStore.GetPostAsJson(1).Subscribe(Console.WriteLine);
            dataStore.GetPosts(2).Do(Console.WriteLine).Subscribe();
            dataStore.CreatePost(new Post
            {
                UserId = 101,
                Id = 0,
                Title = "test title",
                Body = "test body"
            }).Subscribe(Console.WriteLine);
            dataStore.UpdatePost(new Post
            {
                UserId = 101,
                Id = 1,
                Title = "test title",
                Body = "test body"
            }).Subscribe(Console.WriteLine);
            dataStore.DeletePost(1).Subscribe(Console.WriteLine);
            Console.ReadKey();
        }

        private static void TestByRestSharp()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var dataStore = new PostDataStoreByRestSharp();
            dataStore.GetPostAsString(1).Subscribe(Console.WriteLine);
            dataStore.GetPostAsJson(1).Subscribe(Console.WriteLine);
            dataStore.GetPosts(2).Do(Console.WriteLine).Subscribe();
            dataStore.CreatePost(new Post
            {
                UserId = 101,
                Id = 0,
                Title = "test title",
                Body = "test body"
            }).Subscribe(Console.WriteLine);
            dataStore.UpdatePost(new Post
            {
                UserId = 101,
                Id = 1,
                Title = "test title",
                Body = "test body"
            }).Subscribe(Console.WriteLine);
            dataStore.DeletePost(1).Subscribe(Console.WriteLine);
            Console.ReadKey();
        }

        private static void TestByRefit()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var dataStore = new PostDataStoreByRefit();
            Console.WriteLine(dataStore.GetPostAsString(1).Result);
            Console.WriteLine(dataStore.GetPostAsJson(1).Result);
            dataStore.GetPosts(2).Result.ToList().ForEach(Console.WriteLine);
            Console.WriteLine(dataStore.CreatePost(new Post
            {
                UserId = 101,
                Id = 0,
                Title = "test title",
                Body = "test body"
            }).Result);
            Console.WriteLine(dataStore.UpdatePost(new Post
            {
                UserId = 101,
                Id = 1,
                Title = "test title",
                Body = "test body"
            }).Result);
            Console.WriteLine(dataStore.DeletePost(1).Result);
        }
    }
}
