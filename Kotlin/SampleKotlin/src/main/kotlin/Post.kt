data class Post(val userId: Int, val id: Int, val title: String, val body: String) {
    override fun toString() =
        "Post {userId = $userId, id = $id, title = \"$title\", body = \"${body.replace("\n", "\\n")}\"}"
}
