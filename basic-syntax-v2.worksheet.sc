// Instead of interface, we use trait in Scala
trait AccountLoader {
  def loadAccount(accountId: Int): Account
}

// We can have multiple top level definitions in a single file
class Account(val id: Int, val ownerId: Int)

// The class definition is the primary constructor

// By default constructor parameters are private and final
class Accountvalidator(accountLoader: AccountLoader) {

  def userOwnsAccount(accountId: Int, userId: Int): Unit = {
    val account = loadAccount(accountId);

    return account.ownerId == userId;
  }

  private def loadAccount(accountId: Int): Account = {
    accountLoader.loadAccount(accountId);
  }
}

// Lets remove the semi colons
class Accountvalidator2(accountLoader: AccountLoader) {

  def userOwnsAccount(accountId: Int, userId: Int): Unit = {
    val account = loadAccount(accountId)

    return account.ownerId == userId
  }

  private def loadAccount(accountId: Int): Account = {
    accountLoader.loadAccount(accountId)
  }
}

// We don't need to use return keyword, the last expression is the return value
class Accountvalidator3(accountLoader: AccountLoader) {

  def userOwnsAccount(accountId: Int, userId: Int): Boolean = {
    val account = loadAccount(accountId)

    account.ownerId == userId
  }

  private def loadAccount(accountId: Int): Account = {
    accountLoader.loadAccount(accountId);
  }
}

// We can use type inference
class Accountvalidator4(accountLoader: AccountLoader) {

  // For public methods we should specify the return type for best practice  
  def userOwnsAccount(accountId: Int, userId: Int) = {
    val account = loadAccount(accountId)

    account.ownerId == userId
  }

  private def loadAccount(accountId: Int) = {
    accountLoader.loadAccount(accountId)
  }
}
