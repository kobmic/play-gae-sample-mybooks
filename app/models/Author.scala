package models

import java.util.{List => JList, ArrayList}
import java.lang.{Long => JLong}

import siena._

class Author(var firstname: String, var lastname: String) extends Model {
	
	def this() = this(null, null)
	
	@Id
	var id: JLong = null
	
	def books: JList[Book] = {
		Book.all.filter("author", this).fetch()
	}
	
	override def toString() = {
		firstname + " " + lastname
	}

}

object Author {
	def all : Query[Author] = {
        Model.all(classOf[Author])
    }
	
	def count : Int = {
        all.count
    }
	
	def getById(id: JLong) : Author = {
		all.filter("id", id).get()
	}
    
    def getByLastname(lastname: String) : JList[Author] = {
    	all.filter("lastname", lastname).fetch()
    }
    
    def getByName(firstname: String, lastname: String) : Author = {
    	all.filter("firstname", firstname).filter("lastname",lastname).get()
    }
    
    def exists(firstname: String, lastname: String) : Boolean = {
       getByName(firstname, lastname) != null	
    }
}