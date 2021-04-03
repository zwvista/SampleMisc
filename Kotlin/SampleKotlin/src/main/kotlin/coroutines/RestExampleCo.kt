package coroutines

import kotlinx.coroutines.runBlocking
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import retrofit2.converter.scalars.ScalarsConverterFactory
import retrofit2.http.*

data class Post(val userId: Int, val id: Int, val title: String, val body: String) {
    override fun toString() =
        "Post {userId = $userId, id = $id, title = \"$title\", body = \"${body.replace("\n", "\\n")}\"}"
}

interface RestPost {
    @GET
    suspend fun getPostAsString(@Url url: String): String
    @GET("posts/{id}")
    suspend fun getPostAsJson(@Path("id") id: Int): Post
    @GET("posts")
    suspend fun getPosts(): List<Post>
    @FormUrlEncoded
    @POST("posts")
    suspend fun createPost(@Field("userId") userId: Int,
                           @Field("title") title: String,
                           @Field("body") body: String): Post
    @FormUrlEncoded
    @PUT("posts/{id}")
    suspend fun updatePost(@Field("userId") userId: Int,
                           @Path("id") id: Int,
                           @Field("title") title: String,
                           @Field("body") body: String): Post
    @DELETE("posts/{id}")
    suspend fun deletePost(@Path("id") id: Int): String
}

val retrofitJson: Retrofit = Retrofit.Builder()
    .baseUrl("https://jsonplaceholder.typicode.com/")
    .addConverterFactory(GsonConverterFactory.create())
    .build()
val retrofitString: Retrofit = Retrofit.Builder()
    .baseUrl("https://jsonplaceholder.typicode.com/")
    .addConverterFactory(ScalarsConverterFactory.create())
    .build()

suspend fun getPostAsString(): String =
    retrofitString.create(RestPost::class.java)
        .getPostAsString("posts/1")

suspend fun getPostAsJson(): Post =
    retrofitJson.create(RestPost::class.java)
        .getPostAsJson(1)

suspend fun getPosts(n: Int): List<Post> =
    retrofitJson.create(RestPost::class.java)
        .getPosts().take(n)

suspend fun createPost(): Post =
    retrofitJson.create(RestPost::class.java)
        .createPost(101, "test title", "test body")

suspend fun updatePost(): Post =
    retrofitJson.create(RestPost::class.java)
        .updatePost(101, 1, "test title", "test body")

suspend fun deletePost(): String =
    retrofitString.create(RestPost::class.java)
        .deletePost(1)


fun main(args: Array<String>) = runBlocking {
    println(getPostAsString())
    println(getPostAsJson())
    println(getPosts(2))
    println(createPost())
    println(updatePost())
    println(deletePost())
}
