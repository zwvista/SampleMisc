package rx

import com.google.gson.annotations.Expose
import com.google.gson.annotations.SerializedName
import io.reactivex.Observable
import retrofit2.Retrofit
import retrofit2.adapter.rxjava2.RxJava2CallAdapterFactory
import retrofit2.converter.gson.GsonConverterFactory
import retrofit2.converter.scalars.ScalarsConverterFactory
import retrofit2.http.*

data class Post(@SerializedName("userId") @Expose val userId: Int,
                @SerializedName("id") @Expose val id: Int,
                @SerializedName("title") @Expose val title: String,
                @SerializedName("body") @Expose val body: String) {
    override fun toString() =
        "Post {userId = $userId, id = $id, title = \"$title\", body = \"${body.replace("\n", "\\n")}\"}"
}

interface RestPost {
    @GET
    fun getPostAsString(@Url url: String): Observable<String>
    @GET("posts/{id}")
    fun getPostAsJson(@Path("id") id: Int): Observable<Post>
    @GET("posts")
    fun getPosts(): Observable<List<Post>>
    @FormUrlEncoded
    @POST("posts")
    fun createPost(@Field("userId") userId: Int,
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

val retrofitJson: Retrofit = Retrofit.Builder()
    .baseUrl("https://jsonplaceholder.typicode.com/")
    .addCallAdapterFactory(RxJava2CallAdapterFactory.create())
    .addConverterFactory(GsonConverterFactory.create())
    .build()
val retrofitString: Retrofit = Retrofit.Builder()
    .baseUrl("https://jsonplaceholder.typicode.com/")
    .addCallAdapterFactory(RxJava2CallAdapterFactory.create())
    .addConverterFactory(ScalarsConverterFactory.create())
    .build()

fun getPostAsString(): Observable<String> =
    retrofitString.create(RestPost::class.java)
        .getPostAsString("posts/1")

fun getPostAsJson(): Observable<Post> =
    retrofitJson.create(RestPost::class.java)
        .getPostAsJson(1)

// https://stackoverflow.com/questions/29672705/convert-observablelistcar-to-a-sequence-of-observablecar-in-rxjava
fun getPosts(n: Long): Observable<Post> =
    retrofitJson.create(RestPost::class.java)
        .getPosts().flatMapIterable { x -> x }.take(n)

fun createPost(): Observable<Post> =
    retrofitJson.create(RestPost::class.java)
        .createPost(101, "test title", "test body")

fun updatePost(): Observable<Post> =
    retrofitJson.create(RestPost::class.java)
        .updatePost(101, 1, "test title", "test body")

fun deletePost(): Observable<String> =
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
