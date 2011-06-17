package models

import java.util.{List => JList, ArrayList}
import java.lang.{Long => JLong}

import siena._

import scala.collection.JavaConversions._

class UserData(var user: String) extends Model {
	
	def this() = this(null) 
	
	@Id
	var id: JLong = null
	
	def readBooks(): JList[BookInfo] = {
		BookInfo.getByUserDataId(id)
	}
	
	def add(entry: BookInfo) {
		entry.insert()
	}
	
	def bookInfoList() = {
		readBooks.map(info => RichBookInfo(info))
	}
	
}

object UserData {
	
	def all : Query[UserData] = {
        Model.all(classOf[UserData])
    }
	
	def count : Int = all.count
	
	def getById(id: JLong) : UserData = {
		all.filter("id", id).get()
	}
    
    def getByUser(user: String) : UserData = {
    	all.filter("user", user).get()
    }
	
    def getUserDataId(user: String) : JLong = {
    	all.filter("user", user).get().id
    }
    
    def exists(user: String) : Boolean = {
    	getByUser(user) != null
    }
}

