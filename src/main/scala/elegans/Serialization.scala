package elegans

object Serialization {
  import java.io.{ByteArrayInputStream,ByteArrayOutputStream,ObjectInputStream,ObjectOutputStream}

  private val encoding = "Latin1"

  def serialize(obj: Any): String = {
    val bos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bos)

    oos.writeObject(obj)
    oos.flush()
    bos.close()

    val array = bos.toByteArray
    val string = new String(array, encoding)

    string
  }

  def deserialize[A](serialized: String): A = {
    val bis = new ByteArrayInputStream(serialized.getBytes(encoding))
    val ois = new ObjectInputStream(bis)

    val recovered : A = ois.readObject().asInstanceOf[A]
    bis.close()
    
    recovered
  }
}
