package rx

import Post
import com.github.kittinunf.fuel.Fuel
import com.github.kittinunf.fuel.core.ResponseDeserializable
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import io.reactivex.rxjava3.core.Observable
import io.reactivex.rxjava3.core.Single
import io.reactivex.rxjava3.kotlin.subscribeBy


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

fun getPostAsString2(): Single<String> =
    Fuel.get("${home}posts/1").rxString().map { it.component1()!! }

fun getPostAsJson2(): Single<Post> =
    Fuel.get("${home}posts/1").rxObject(PostDeserializer()).map { it.component1()!! }

fun getPosts2(n: Int): Observable<Post> =
    Fuel.get("${home}posts").rxObject(PostListDeserializer()).flattenAsObservable { it.component1()!! }.take(n.toLong())

fun createPost2(): Single<Post> =
    Fuel.post("${home}posts", listOf("userId" to 101, "title" to "test title", "body" to "test body"))
        .rxObject(PostDeserializer()).map { it.component1()!! }

fun updatePost2(): Single<Post> =
    Fuel.put("${home}posts/1", listOf("userId" to 101, "id" to 1, "title" to "test title", "body" to "test body"))
        .rxObject(PostDeserializer()).map { it.component1()!! }

fun deletePost2(): Single<String> =
    Fuel.delete("${home}posts/1").rxString().map { it.component1()!! }

fun main(args: Array<String>) {
    getPostAsString2().subscribeBy { println(it) }
    getPostAsJson2().subscribeBy { println(it) }
    getPosts2(2).subscribeBy { println(it) }
    createPost2().subscribeBy { println(it) }
    updatePost2().subscribeBy { println(it) }
    deletePost2().subscribeBy { println(it) }
    readLine()
}
