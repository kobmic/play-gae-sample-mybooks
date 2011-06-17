package models

import java.util.{List => JList, ArrayList}
import java.lang.{Long => JLong}

import siena._

import scala.collection.JavaConversions._


class BookInfo(var bookId: JLong,
		       var userDataId: JLong,
		       var endMonth: String,
		       var endYear: String,
		       var own: Boolean)
		       extends Model {
  @Id
  var id: JLong = null
  def this() = this(null, null, null, null, true) 
  
  override def toString() = {
	  "[Id:" + id + ", userDataId:" + userDataId + ", bookId:" + bookId + "]"
  }
  
  def endDate() = { 
	  val sb = new StringBuilder  
	  if ((endYear != null) && (endYear.length > 0)) sb.append(endYear)
	  if ((endMonth != null) && (endMonth.length > 0)) sb.append("-").append(endMonth)
	 sb.toString
  }
}

object BookInfo {
	def all : Query[BookInfo] = {
        Model.all(classOf[BookInfo])
    }
	
	def count(userDataId: JLong) : Int = {
        all.filter("userDataId", userDataId).count
    }
	
	def getById(id: JLong) : BookInfo = {
		all.filter("id", id).get()
	}
	
	def getByUserDataId(id: JLong) : JList[BookInfo] = {
		all.filter("userDataId", id).order("endYear").order("endMonth").fetch()
	}
	
	def getByBookIdAndUserDataId(bookId: JLong, userDataId: JLong) : BookInfo = {
		all.filter("bookId", bookId).filter("userDataId", userDataId).get()
	}
	
	def exists(bookId: JLong, userDataId: JLong) : Boolean = {
       all.filter("bookId", bookId).filter("userDataId", userDataId).get() != null 	
    }
	
	def currentYearCount(userDataId: JLong) = {
		val currentYear = new java.text.SimpleDateFormat("yyyy").format(new java.util.Date())
		all.filter("userDataId", userDataId).filter("endYear", currentYear).count
	}
    
}

case class RichBookInfo(val bookInfo: BookInfo) {
  val id = bookInfo.id	
  val book = Book.getById(bookInfo.bookId)
  val userDataId = bookInfo.userDataId
  val endMonth = bookInfo.endMonth 
  var endYear = bookInfo.endYear 
  var own = bookInfo.own
  def endDate() = bookInfo.endDate()
}