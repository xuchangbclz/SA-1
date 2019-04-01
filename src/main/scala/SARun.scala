import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import org.json4s.{DefaultFormats, NoTypeHints}
import org.json4s.jackson.{JsonMethods, Serialization}

case class ClassTeachingCount(classId: String, classNo: String, count: Int) {}

case class TeacherTeachingInfo(var courseIndex: String, var classList: List[ClassTeachingCount],
                               var arrangeCells: List[Int], var conflictCells: List[String],
                               var connectCells: List[String], var teacherId: String, var teacherName: String) {}

case class TeacherTeachInfo(jobId: String, teachInfos: List[TeachInfo]) {}

case class TeachInfo(gradeId: String, gradeName: String, classId: String, classesName: String,
                     teachInfos: Map[String, TeachPlanInfo])

case class TeachPlanInfo(courseId: String, courseName: String, count: Int, teacherId: String,
                         teacherName: String)

case class Course(id: String, name: String, important: Int)

object SARun {

  val NOARRAGNGECODE = "-"
  val CONNECTIONSLOTCODE = "-1"

  // important 1,2,3(体育),4
  val everySectionCostValue = Tuple4(
    Tuple7(1, 2, 3, 4, 5, 6, 7),
    Tuple7(2, 1, 2, 4, 2, 3, 6),
    Tuple7(10, 10, 5, 2, 10, 5, 1),
    Tuple7(2, 2, 2, 2, 2, 2, 2)
  )

  def main(args: Array[String]): Unit = {

    val everydaySection = 7
    val day = 5
    val courseInfo = initCourse()
    implicit val info = getResource().teachInfos
    val classList = info.map(_.classId).distinct
    val classSize = classList.size

    val classMap = (0 to classSize-1).toList.map(i=>(classList.apply(i),i)).toMap

    implicit val initResourceRes = initResource.sortWith((t1, t2) => t1.classList.map(_.count).sum - t2.classList.map(_.count).sum < 0)
    val allResource=Tuple4(everydaySection * day, classSize, courseInfo,classMap)
    val sheetInfo = generateSheetInfo(allResource)

  }

  def resolveConflict(): Unit ={




  }


  def generateSheetInfo(allResource:Tuple4[Int,Int,Map[String, Course],Map[String,Int]])
                       (implicit resource: List[TeacherTeachingInfo]): Array[Array[String]] = {


    val xLen = allResource._2
    val yLen = allResource._1
    val courseInfo=allResource._3
    val classMap=allResource._4
    val sheetInfo = Array.ofDim[String](xLen, yLen).map(a=>a.map(aSub=>NOARRAGNGECODE))
    val virtualTeacherNum=resource.size

    val lastSetIndx=collection.mutable.Map[Int,Int]()
    var teacherIndx=new AtomicInteger(0);
    resource.foreach(r=>{

                r.classList.foreach(r1=>{
                  val classIndx = classMap.get(r1.classId).get
                  (0 to r1.count-1).foreach(c=>{
                    val maybeInt = lastSetIndx.get(classIndx)
                    if(!maybeInt.isEmpty){
                      lastSetIndx(classIndx)=maybeInt.get+1
                      sheetInfo.apply(classIndx)(maybeInt.get+1)=teacherIndx.get().toString

                    }else{
                      lastSetIndx.put(classIndx,0)
                      sheetInfo.apply(classIndx)(0)=teacherIndx.get().toString
                    }

                  })


                })
          teacherIndx.incrementAndGet()
        })

    sheetInfo
  }

  def initResource(implicit resource: List[TeachInfo]): List[TeacherTeachingInfo] = {

    resource.flatMap(map => {
      val classId = map.classId
      val className = map.classesName
      map.teachInfos.map(map2 => {

        TeacherTeachingInfo(map2._2.courseId, List(ClassTeachingCount(classId, className, map2._2.count)), Nil, Nil, Nil, map2._2.teacherId, map2._2.teacherName)

      })

    }).groupBy(_.courseIndex).flatMap(m => {

      m._2.groupBy(_.teacherId).mapValues(ttis2 => ttis2.reduce((tti1, tti2) => {

        val unionClass = tti1.classList.union(tti2.classList)

        var tti3 = tti1.copy()
        tti3.classList = unionClass
        tti3
      })).values.toList

    }).toList

  }

  def getResource(): TeacherTeachInfo = {

    val json = JsonData().json
    implicit val formats = Serialization.formats(NoTypeHints)
    JsonMethods.parse(json).extract[TeacherTeachInfo]

  }

  def initCourse(): Map[String, Course] = {

    val course1 = Course("1800411635847618560", "语文", 1)
    val course2 = Course("1800411635847618561", "数学", 1)
    val course3 = Course("1800412185603432448", "外语", 1)
    val course4 = Course("1814955013587165889", "物理", 2)
    val course5 = Course("1800412185603432888", "化学", 2)
    val course6 = Course("1800412185603432449", "历史", 2)
    val course7 = Course("1814955013587165284", "体育", 3)
    val course8 = Course("1814955013587165890", "美术", 4)


    List(course1, course2, course3, course4, course5, course6, course7, course8).map(l => (l.id, l)).toMap

  }


}
