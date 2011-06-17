package controllers


import models._

import play._
import play.data.binding._
import play.mvc._
import play.libs._
import play.cache._
import play.data.validation._
import play.mvc.ScalaController._

import java.lang.{Long => JLong}
import java.util.{List => JList}

import play.modules.gae._
import scala.collection.JavaConversions._

object Application extends Controller with Secure {
	
	def index() = { 
		Template
    }
}
object MyBooks extends Controller with Secure {
	private def getOrCreateUserData() : UserData = {
		if (!UserData.exists(connectedUser)) {
    		val data = new UserData(connectedUser)
    		data.insert()
    		data.get()
    		data
    	} else {
    		UserData.getByUser(connectedUser)
    	}
	}
	
	private def getMyBooks() = {
	  val userData = getOrCreateUserData()
      userData.bookInfoList()
	}
	
	private def getMyBooksFiltered(author: String, year: String) = {
	  var bookInfos = getMyBooks()
    	if ((author != null) && (author.length() > 0)) {
    		bookInfos = bookInfos.filter(_.book.authorname.contains(author))
    	}
    	if ((year != null) && (year.length() > 0)) {
    		bookInfos = bookInfos.filter(_.endYear == year)
    	}
    	bookInfos
	}
	
	def list(author: String, year: String) = {
      val bookInfos = getMyBooksFiltered(author, year)
      val id = UserData.getUserDataId(connectedUser)
      val currentYearCount = new java.text.SimpleDateFormat("yyyy").format(new java.util.Date()) + ": " + BookInfo.currentYearCount(id)
      val totalCount = BookInfo.count(id)
      Template(bookInfos, author, year,  totalCount, currentYearCount)
    }
	
	def myBooksFiltered(author: String, year: String) = { 
	  Action(list(author, year))
    }
    
    def add(bookId: JLong) = {
    	val userData = getOrCreateUserData()
    	val book = Book.getById(bookId)
    	Template(userData, book)
    }
    
    def edit(id: JLong) = {
    	val info = BookInfo.getById(id)
    	val book = Book.getById(info.bookId)
    	Template(book, info)
    }
    
    def delete(id: JLong) = {
    	val info = BookInfo.getById(id)
    	val book = Book.getById(info.bookId)
    	info.delete()
    	flash.success("Deleted entry")
    	Action(list("", ""))
    }
    
    def postEntry(bookId: JLong, endMonth: String, endYear: String, own: Boolean) = {
      val userData = getOrCreateUserData()
      
      if (BookInfo.exists(bookId, userData.id)) {
    	  flash.error("Entry exists already!")
      } else {
           val entry = new BookInfo(bookId, userData.id, endMonth, endYear, own)
        entry.insert()
        flash.success("Added")   
      }
      Action(Application.index())
    }
    
    def updateEntry(bookId: JLong, endMonth: String, endYear: String, own: Boolean) = {
      val userData = getOrCreateUserData()
      val bookInfo = BookInfo.getByBookIdAndUserDataId(bookId, userData.id)
      bookInfo.endMonth = endMonth
      bookInfo.endYear = endYear
      bookInfo.own = own
      bookInfo.update()
      flash.success("Updated")   
      Action(list("", ""))
    }
}

object Books extends Controller with Secure with MaxFetchSize {
	def list(offset: Int = 0) = { 
		val bookCount = Book.all.count() 
		val (prevOffset, nextOffset) = calculateOffsets(offset, bookCount)
		val books = Book.all.order("title").fetch(maxFetchSize, offset)
    	Template(books, bookCount, offset, prevOffset, nextOffset)
    }
	
	def show(id: JLong) = {
    	val book = Book.getById(id)
        Template(book)
    }
    
    private def splitAuthors() = {
      val authors = Author.all.order("lastname").fetch()
      val authorsA = authors.filter(a => List( 'A', 'B', 'C', 'D', 'E', 'F').contains(a.lastname.charAt(0).toUpper))
      val authorsG = authors.filter(a => List( 'G', 'H', 'I', 'J', 'K', 'L') .contains(a.lastname.charAt(0).toUpper))
      val authorsM = authors.filter(a => List( 'M', 'N', 'O', 'P', 'Q', 'R').contains(a.lastname.charAt(0).toUpper))
      val authorsS = authors.filter(a => List( 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'Ä', 'Ö', 'Å').contains(a.lastname.charAt(0).toUpper))
      (authorsA, authorsG, authorsM, authorsS)
    }
	
	def add() = {
	  val (authorsA, authorsG, authorsM, authorsS) = splitAuthors()	
      Template(authorsA, authorsG, authorsM, authorsS)	
    }
    
    def edit(id: JLong) = {
      val (authorsA, authorsG, authorsM, authorsS) = splitAuthors()
      val book = Book.getById(id)
	  Template(book, authorsA, authorsG, authorsM, authorsS)	
    }
    
    def updateBook(bookId: JLong, authorId: JLong, @Required title: String, subtitle: String, url: String, isbn10: String, isbn13: String) = {
    	if (!Validation.hasErrors) {
	    	val book = Book.getById(bookId)
	    	book.authorId = authorId
	    	book.title = title.trim
	    	book.subtitle = subtitle.trim
	    	book.url = url.trim
	    	book.isbn10 = isbn10.trim
	    	book.isbn13 = isbn13.trim
	    	book.update()
    	    flash.success("Updated book '%s'", book.authorAndTitle())
    	    Action(list())
    	} else {
    		flash.error("Required field 'title' is missing")
	 	    Action(edit(bookId))
    	}
    }
    
    def postBook(authorId: JLong, @Required title: String, subtitle: String, url: String, isbn10: String, isbn13: String) = {
	  if (!Validation.hasErrors) {
	 	  var b: Book = null
	      if (Book.exists(authorId, title.trim)) {
	    	  flash.error("Book '%s' exists already!", title)
	    	  b = Book.getByTitleAndAuthor(title, authorId)
	      } else {
	          b = new Book(authorId, title.trim)
	          b.url = url.trim
	          b.isbn10 = isbn10.trim
	          b.isbn13 = isbn13.trim
	          b.insert()
	          flash.success("Added book '%s'", title)  
	      }
	      
	      Action(show(b.id))
	  } else {
	 	 flash.error("Required field 'title' is missing")
	 	 Action(add())
	  }
    }
	
	def postRating(bookId : JLong, rating: Int, comment: String) = {
		if (Rating.exists(bookId, connectedUser)) {
		  flash.error("You have already rated this book")	
		} else {
		  val newRating = new Rating(bookId, connectedUser, rating, comment)
		  newRating.insert()
		}
      flash.success("Thanks for voting %s", connectedUser)  
      Action(show(bookId))
    }
	
	def deleteRating(bookId: JLong, ratingId: JLong) = {
	  val rating = Rating.getById(ratingId)
	  rating.delete()
	  flash.success("Rating removed")  
      Action(show(bookId))	
	}
	
}

object Authors extends Controller with Secure with MaxFetchSize {
  
  def list(offset: Int = 0) = { 
	    val authorCount = Author.all.count()
	    val (prevOffset, nextOffset) = calculateOffsets(offset, authorCount)
	    val authors = Author.all.order("lastname").fetch(maxFetchSize, offset)
    	Template(authors, authorCount, offset, prevOffset, nextOffset)
    }
	 
  def show(id: JLong) = {
    val author = Author.getById(id)
    val books = Book.getByAuthor(author.id)
    Template(author, books)
  }
    
  def add() = {
    Template()	
  }
    	 
	
	def postAuthor(@Required firstname: String, @Required lastname: String) = {
	  if (!Validation.hasErrors) {
	      if (Author.exists(firstname.trim, lastname.trim)) {
	    	  flash.error("Author %s %s exists already!", firstname, lastname)
	      } else {
	          new Author(firstname.trim, lastname.trim).insert()
	          flash.success("Added author %s %s", firstname, lastname)  
	      }
	      Action(list(0))
	  } else {
	 	flash.error("Fill required fields 'firstname' and 'lastname'!")
	 	Action(add)
	  }
      
    }
	
}

trait Secure {
	self: Controller =>
    
    @Before
    def checkConnected = {
    	if(!GAE.isLoggedIn) {
    	    Action(Authentication.login())
        } else {
        	renderArgs += "user" -> GAE.getUser().getEmail()
        	Continue
        }
    }
    
    @Util
    def connectedUser = renderArgs.get("user").asInstanceOf[String]
    
}

trait MaxFetchSize {
	val maxFetchSize = 40
	
	def calculateOffsets(offset: Int, count: Int) = {
		val prevOffset = if (offset == 0) offset else offset - maxFetchSize
		val nextOffset = if (offset + maxFetchSize < count) offset + maxFetchSize else offset
		(prevOffset, nextOffset)
	}
}

object Authentication extends Controller {
   def login() {
	    GAE.login("Application.index")
    }
    
    def logout() {
    	GAE.logout("Application.index")
    }
}


