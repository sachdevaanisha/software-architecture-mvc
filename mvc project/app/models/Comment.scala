package models

case class Comment( var postId: Long,
                    var commentedBy: String,
                    var commentText: String,
                    var commentImage: String
                   )




