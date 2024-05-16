package coroutines

import Post
import com.github.kittinunf.fuel.Fuel
import com.github.kittinunf.fuel.core.ResponseDeserializable
import com.github.kittinunf.fuel.coroutines.awaitObject
import com.github.kittinunf.fuel.coroutines.awaitString
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import kotlinx.coroutines.runBlocking


private val home = "https://jsonplaceholder.typicode.com/"

class PostDeserializer : ResponseDeserializable<Post> {
    override fun deserialize(content: String): Post =
        Gson().fromJson(content, Post::class.java)
}

class PostListDeserializer : ResponseDeserializable<List<Post>> {
    override fun deserialize(content: String): List<Post> {
        // https://stackoverflow.com/questions/5554217/google-gson-deserialize-listclass-object-generic-type
        val listType = object : TypeToken<List<Post>>() {}.type
//        val listType = TypeToken.getParameterized(List::class.java, Post::class.java).type
        return Gson().fromJson(content, listType)
    }
}

suspend fun getPostAsString2(): String =
    Fuel.get("${home}posts/1").awaitString()

suspend fun getPostAsJson2(): Post =
    Fuel.get("${home}posts/1").awaitObject(PostDeserializer())

suspend fun getPosts2(n: Int): List<Post> =
    Fuel.get("${home}posts").awaitObject(PostListDeserializer()).take(n)

suspend fun createPost2(): Post =
    Fuel.post("${home}posts", listOf("userId" to 101, "title" to "test title", "body" to "test body"))
        .awaitObject(PostDeserializer())

suspend fun updatePost2(): Post =
    Fuel.put("${home}posts/1", listOf("userId" to 101, "id" to 1, "title" to "test title", "body" to "test body"))
        .awaitObject(PostDeserializer())

suspend fun deletePost2(): String =
    Fuel.delete("${home}posts/1").awaitString()

fun main(args: Array<String>) = runBlocking {
    println(getPostAsString2())
    println(getPostAsJson2())
    println(getPosts2(2))
    println(createPost2())
    println(updatePost2())
    println(deletePost2())
}
