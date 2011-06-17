package models

import siena._
import java.util.{List => JList, ArrayList}
import java.lang.{Long => JLong}

class Rating(var bookId: JLong, var user: String, var rating: Int, var comment: String) extends Model {
	def this() = this(null, null, 0, "")
	
	@Id
	var id: JLong = null
	
	// rating date
	var date = new java.util.Date()
	
	def genRatingJavascript : String = {
		"$('#rating" + id + "').raty({ path: '/public/javascripts/img/', readOnly:  true, start: " +
		  rating + " });"
	}
	
	def isDeletable(currentUser: String) : Boolean = (currentUser == user)
  
}

object Rating {
	
	def all : Query[Rating] = {
        Model.all(classOf[Rating])
    }
	
	def count : Int = {
        all.count
    }
	
	def forBook(book : Book) : JList[Rating] = {
		all.filter("book", book).fetch()
	}
	
	def ratingsByUser(user : String) : JList[Rating] = {
		all.filter("user", user).fetch()
	}
	
	def userRatingForBook(bookId: JLong, user : String) : Rating = {
		all.filter("bookId", bookId).filter("user", user).get()
	}
	
	def getById(id: JLong) : Rating = {
		all.filter("id", id).get()
	}
	
	def exists(bookId: JLong, user: String) : Boolean = {
	  all.filter("user", user).filter("bookId", bookId).get != null
	}
}
 