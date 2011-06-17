package models

import java.util.{List => JList, ArrayList}
import java.lang.{Long => JLong}

import siena._

import scala.collection.JavaConversions._

class Book(var authorId: JLong,
		   var title: String,
		   var subtitle: String = "",
		   var url: String = "",
		   var isbn10: String = "",
		   var isbn13: String = "") extends Model {
	
	def this() = this(null, null, "", "", "")
	
    @Id
	var id: JLong = null
    
   def shortUrl() : String = {
     if ((url != null) && (url.length >30)) {
       url.substring(0, 30) + "..."
     } else url
	}
	
	def authorname() : String = {
		val author = Author.getById(authorId)
		author.toString()
	}
	
	def authorAndTitle() : String = {
		authorname + " - " + title
	}
	
	def ratings(): JList[Rating] = {
		Rating.all.filter("bookId", id).fetch()
	}
    	
	def averageRating() : Int = {
		val count = ratings.size
		if (count > 0) {
		  val sum = ratings.map(r => r.rating).reduceLeft(_ + _)
		  sum / count
		}
		else 0 
	}
	
	def genRatingsJavascript() : String = {
		ratings.map(_.genRatingJavascript).reduceLeft(_ + _)
	}
	
	override def toString() = title
	
	def userRating(user: String) : Int = {
		val userRating = Rating.userRatingForBook(this.id, user)
		if (userRating != null) {
			userRating.rating
		} else 0
	}
}

object Book {
	
	def all : Query[Book] = {
        Model.all(classOf[Book])
    }
	
	def count : Int = {
        all.count
    }
	
	def getById(id: JLong) : Book = {
		all.filter("id", id).get()
	}
    
    def getByTitle(title: String) : JList[Book] = {
    	all.filter("title", title).fetch()
    }
    
    def getByTitleAndAuthor(title: String, authorId: JLong) : Book = {
    	all.filter("title", title).filter("authorId", authorId).get()
    }
    
    def getByAuthor(authorId: JLong) : JList[Book] = {
    	all.filter("authorId", authorId).fetch()
    }
    
    def exists(authorId: JLong, title: String) : Boolean = {
       getByTitleAndAuthor(title, authorId) != null	
    }
}