package TravelCompanionScala {
package model {

import javax.persistence._
import java.util._
import org.hibernate.validator.constraints._
import java.util


/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 08.04.2010
 * Time: 16:09:06
 * To change this template use File | Settings | File Templates.
 */

@Entity
@Table(name = "members")
class Member() {

//   val toXML: NodeSeq = {
//     NodeSeq.Empty
//  }


  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  var id: Long = _

  @Column(name = "city")
  var city: String = ""

  @Column(name = "email")
  @NotEmpty@Email
  var email: String = ""

  @Column(name = "forename")
  var forename: String = ""

  @Column(unique = true, name = "name")
  @NotEmpty
  var name: String = ""

  @Column(name = "password")
  @NotEmpty
  var password: String = ""

  @Column(name = "street")
  var street: String = ""

  @Column(name = "surname")
  var surname: String = ""

  @Column(name = "zipcode")
  var zipcode: String = ""

  @OneToMany(mappedBy = "owner", cascade = Array(CascadeType.ALL), targetEntity = classOf[Tour])
  var tours: util.List[Tour] = new util.ArrayList[Tour]()

  @OneToMany(mappedBy = "owner", cascade = Array(CascadeType.ALL), targetEntity = classOf[BlogEntry])
  val blogEntries: util.List[BlogEntry] = new util.ArrayList[BlogEntry]()

  @OneToMany(mappedBy = "owner", cascade = Array(CascadeType.ALL))
  val pictures: util.List[Picture] = new util.ArrayList[Picture]()

  @ManyToMany(cascade = Array(CascadeType.ALL))
  @JoinTable(name = "member_roles", joinColumns = Array(new JoinColumn(name = "member", referencedColumnName = "id")), inverseJoinColumns = Array(new JoinColumn(name = "roles", referencedColumnName = "id")))
  val roles: util.List[Role] = new util.ArrayList[Role]()

  override def equals(that: Any): Boolean = that match {
    case other: Member => id == other.id
    case _ => false
  }
}
}
}