using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

using System.Reactive.Linq;
using System.Reactive.Threading.Tasks;

using Newtonsoft.Json;
using Plugin.Connectivity;

namespace RxSamples
{
    public class Post
    {
        [JsonProperty("userId")]
        public int UserId { get; set; }
        [JsonProperty("id")]
        public int Id { get; set; }
        [JsonProperty("title")]
        public string Title { get; set; }
        [JsonProperty("body")]
        public string Body { get; set; }

        public override string ToString()
        {
            string f(string str) => $"\"{str.Replace("\n", "\\n")}\"";
            return $"Post {{userId = {UserId}, id = {Id}, title = {f(Title)}, body = {f(Body)}}}";
        }
    }

    public class PostDataStoreByTask
    {
        protected HttpClient client = new HttpClient
        {
            BaseAddress = new Uri("https://jsonplaceholder.typicode.com/")
        };

        public async Task<string> GetPostAsString(int id) =>
        !CrossConnectivity.Current.IsConnected ? null : await client.GetStringAsync($"posts/{id}");

        public async Task<Post> GetPostAsJson(int id)
        {
            if (!CrossConnectivity.Current.IsConnected) return null;

            var json = await client.GetStringAsync($"posts/{id}");
            var item = await Task.Run(() => JsonConvert.DeserializeObject<Post>(json));

            return item;
        }

        public async Task<IEnumerable<Post>> GetPosts(int n)
        {
            IEnumerable<Post> items = new List<Post>();
            if (CrossConnectivity.Current.IsConnected)
            {
                var json = await client.GetStringAsync("posts");
                items = await Task.Run(() => JsonConvert.DeserializeObject<IEnumerable<Post>>(json));
            }
            return items.Take(n);
        }

        private StringContent GetStringContent(Post item) =>
        new StringContent(JsonConvert.SerializeObject(item), Encoding.UTF8, "application/json");

        public async Task<Post> CreatePost(Post item)
        {
            if (item == null || !CrossConnectivity.Current.IsConnected) return null;

            var response = await client.PostAsync("posts", GetStringContent(item));
            if (!response.IsSuccessStatusCode) return null;

            var json = await response.Content.ReadAsStringAsync();
            var item2 = await Task.Run(() => JsonConvert.DeserializeObject<Post>(json));

            return item2;
        }

        public async Task<Post> UpdatePost(Post item)
        {
            if (item == null || !CrossConnectivity.Current.IsConnected) return null;

            var response = await client.PutAsync($"posts/{item.Id}", GetStringContent(item));
            if (!response.IsSuccessStatusCode) return null;

            var json = await response.Content.ReadAsStringAsync();
            var item2 = await Task.Run(() => JsonConvert.DeserializeObject<Post>(json));

            return item2;
        }

        public async Task<string> DeletePost(int id)
        {
            if (!CrossConnectivity.Current.IsConnected) return null;

            var response = await client.DeleteAsync($"posts/{id}");
            if (!response.IsSuccessStatusCode) return null;

            var json = await response.Content.ReadAsStringAsync();
            return json;
        }
    }

    public class PostDataStoreByRx
    {
        protected HttpClient client = new HttpClient
        {
            BaseAddress = new Uri("https://jsonplaceholder.typicode.com/")
        };

        public IObservable<string> GetPostAsString(int id) =>
        !CrossConnectivity.Current.IsConnected ? Observable.Empty<string>() :
            client.GetStringAsync($"posts/{id}").ToObservable();

        public IObservable<Post> GetPostAsJson(int id) =>
        !CrossConnectivity.Current.IsConnected? Observable.Empty<Post>() :
            client.GetStringAsync($"posts/{id}").ToObservable()
            .Select(json => JsonConvert.DeserializeObject<Post>(json));

        public IObservable<Post> GetPosts(int n) =>
        !CrossConnectivity.Current.IsConnected ? Observable.Empty<Post>() :
            client.GetStringAsync("posts").ToObservable()
            .SelectMany(json => JsonConvert.DeserializeObject<IEnumerable<Post>>(json))
            .Take(n);

        private StringContent GetStringContent(Post item) =>
        new StringContent(JsonConvert.SerializeObject(item), Encoding.UTF8, "application/json");

        public IObservable<Post> CreatePost(Post item) =>
        item == null || !CrossConnectivity.Current.IsConnected ? Observable.Empty<Post>() :
            client.PostAsync("posts", GetStringContent(item)).ToObservable()
            .SelectMany(response => !response.IsSuccessStatusCode ? Observable.Empty<Post>() :
                response.Content.ReadAsStringAsync().ToObservable()
                .Select(json => JsonConvert.DeserializeObject<Post>(json)));

        public IObservable<Post> UpdatePost(Post item) =>
        item == null || !CrossConnectivity.Current.IsConnected? Observable.Empty<Post>() :
            client.PutAsync($"posts/{item.Id}", GetStringContent(item)).ToObservable()
            .SelectMany(response => !response.IsSuccessStatusCode? Observable.Empty<Post>() :
                response.Content.ReadAsStringAsync().ToObservable()
                .Select(json => JsonConvert.DeserializeObject<Post>(json)));

        public IObservable<string> DeletePost(int id) =>
        !CrossConnectivity.Current.IsConnected ? Observable.Empty<string>() :
            client.DeleteAsync($"posts/{id}").ToObservable()
            .SelectMany(response => !response.IsSuccessStatusCode ? Observable.Empty<string>() :
                response.Content.ReadAsStringAsync().ToObservable());
    }

    public class RestExample
    {
        public static void Test()
        {
            {
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
            {
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
        }
    }
}
