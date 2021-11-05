import ox.cads.locks.SimpleReentrantLock

object PS2Q2 {
  class Account {

    private var balance = 0
    private var preferredWithdrawalsPending = 0
    private val lock = new SimpleReentrantLock()

    private val waitForPreffered, withdrawalCondition = lock.newCondition


    def deposit(amount: Int) = lock.mutex {
      balance += amount
      withdrawalCondition.signal
    }

    def withdrawal(amount: Int, isPref: Boolean) = lock.mutex {

      if (isPref)
        preferredWithdrawalsPending += 1
      else
        waitForPreffered.await(preferredWithdrawalsPending == 0)

      withdrawalCondition.await(balance >= amount)
      balance -= amount
      preferredWithdrawalsPending -= 1
      if (isPref)
        waitForPreffered.signal
      withdrawalCondition.signal
    }
  }
}
