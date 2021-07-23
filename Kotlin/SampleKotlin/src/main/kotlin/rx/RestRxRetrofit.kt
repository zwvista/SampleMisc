package rx

import Post
import io.reactivex.rxjava3.core.Observable
import io.reactivex.rxjava3.core.Single
import retrofit2.Retrofit
import retrofit2.adapter.rxjava3.RxJava3CallAdapterFactory
import retrofit2.converter.gson.GsonConverterFactory
import retrofit2.converter.scalars.ScalarsConverterFactory
import retrofit2.http.*

interface RestPost {
    @GET
    fun getPostAsString(@Url url: String): Single<String>
    @GET("posts/{id}")
    fun getPostAsJson(@Path("id") id: Int): Single<Post>
    @GET("posts")
    fun getPosts(): Single<List<Post>>
    @FormUrlEncoded
    @POST("posts")
    fun createPost(@Field("userId") userId: Int,
                   @Field("title") title: String,
                   @Field("body") body: String): Single<Post>
    @FormUrlEncoded
    @PUT("posts/{id}")
    fun updatePost(@Field("userId") userId: Int,
                   @Path("id") id: Int,
                   @Field("title") title: String,
                   @Field("body") body: String): Single<Post>
    @DELETE("posts/{id}")
    fun deletePost(@Path("id") id: Int): Single<String>
}

val retrofitJson: Retrofit = Retrofit.Builder()
    .baseUrl("https://jsonplaceholder.typicode.com/")
    .addCallAdapterFactory(RxJava3CallAdapterFactory.create())
    .addConverterFactory(GsonConverterFactory.create())
    .build()
val retrofitString: Retrofit = Retrofit.Builder()
    .baseUrl("https://jsonplaceholder.typicode.com/")
    .addCallAdapterFactory(RxJava3CallAdapterFactory.create())
    .addConverterFactory(ScalarsConverterFactory.create())
    .build()

fun getPostAsString(): Single<String> =
    retrofitString.create(RestPost::class.java)
        .getPostAsString("posts/1")

fun getPostAsJson(): Single<Post> =
    retrofitJson.create(RestPost::class.java)
        .getPostAsJson(1)

// https://stackoverflow.com/questions/29672705/convert-observablelistcar-to-a-sequence-of-observablecar-in-rxjava
fun getPosts(n: Long): Observable<Post> =
    retrofitJson.create(RestPost::class.java)
        .getPosts().flattenAsObservable { it }.take(n)

fun createPost(): Single<Post> =
    retrofitJson.create(RestPost::class.java)
        .createPost(101, "test title", "test body")

fun updatePost(): Single<Post> =
    retrofitJson.create(RestPost::class.java)
        .updatePost(101, 1, "test title", "test body")

fun deletePost(): Single<String> =
    retrofitString.create(RestPost::class.java)
        .deletePost(1)

fun main(args: Array<String>) {
    getPostAsString().subscribe(::println)
    getPostAsJson().subscribe(::println)
    getPosts(2).subscribe(::println)
    createPost().subscribe(::println)
    updatePost().subscribe(::println)
    deletePost().subscribe(::println)
}
