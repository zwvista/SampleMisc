package rx

import io.reactivex.Observable
import retrofit2.Retrofit
import retrofit2.adapter.rxjava2.RxJava2CallAdapterFactory
import retrofit2.converter.gson.GsonConverterFactory
import retrofit2.converter.scalars.ScalarsConverterFactory
import retrofit2.http.*

data class Post(var userId: Int, var id: Int, var title: String, var body: String) {
    override fun toString(): String {
        fun f(str: String) = "\"${str.replace("\n", "\\n")}\""
        return "Post {userId = $userId, id = $id, title = ${f(title)}, body = ${f(body)}}"
    }
}

interface RestJsonPlaceHolder {
    @GET
    fun getPostAsString(@Url url: String): Observable<String>
    @GET("posts/{id}")
    fun getPostAsJson(@Path("id") id: Int): Observable<Post>
    @GET("posts")
    fun getPosts(): Observable<List<Post>>
    @FormUrlEncoded
    @POST("posts")
    fun createPost(@Field("userId") userId: Int,
                   @Field("id") id: Int,
                   @Field("title") title: String,
                   @Field("body") body: String): Observable<Post>
    @FormUrlEncoded
    @PUT("posts/{id}")
    fun updatePost(@Field("userId") userId: Int,
                   @Path("id") id: Int,
                   @Field("title") title: String,
                   @Field("body") body: String): Observable<Post>
    @DELETE("posts/{id}")
    fun deletePost(@Path("id") id: Int): Observable<String>
}

var retrofitJson = Retrofit.Builder().baseUrl("https://jsonplaceholder.typicode.com/")
    .addCallAdapterFactory(RxJava2CallAdapterFactory.create())
    .addConverterFactory(GsonConverterFactory.create())
    .build()
var retrofitString = Retrofit.Builder().baseUrl("https://jsonplaceholder.typicode.com/")
    .addCallAdapterFactory(RxJava2CallAdapterFactory.create())
    .addConverterFactory(ScalarsConverterFactory.create())
    .build()

fun getPostAsString(): Observable<String> =
    retrofitString.create(RestJsonPlaceHolder::class.java)
        .getPostAsString("posts/1")

fun getPostAsJson(): Observable<Post> =
    retrofitJson.create(RestJsonPlaceHolder::class.java)
        .getPostAsJson(1)

fun getPosts(n: Long): Observable<Post> =
    retrofitJson.create(RestJsonPlaceHolder::class.java)
        .getPosts().flatMapIterable { x -> x }.take(n)

fun createPost(): Observable<Post> =
    retrofitJson.create(RestJsonPlaceHolder::class.java)
        .createPost(101, 102, "test title", "test body")

fun updatePost(): Observable<Post> =
    retrofitJson.create(RestJsonPlaceHolder::class.java)
        .updatePost(101, 1, "test title", "test body")

fun deletePost(): Observable<String> =
    retrofitString.create(RestJsonPlaceHolder::class.java)
        .deletePost(1)

fun main(args: Array<String>) {
    getPostAsString().subscribe(::println)
    getPostAsJson().subscribe(::println)
    getPosts(2).subscribe(::println)
    createPost().subscribe(::println)
    updatePost().subscribe(::println)
    deletePost().subscribe(::println)
}
