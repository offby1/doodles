def main():
    freshman = sophomore = junior = senior = 0
    firstMenu = 0
    secondMenu = 0
    total = 0
    testScore = 0
    studentCount = 0
    print("WELCOME TO THE STUDENT SURVEY!!\n")
    while firstMenu != 2:
        print("Please choose from the following menu: ")
        print("\t1) Enter Student Information\n\t2) Quit\n")

        firstMenu = int(input("Enter your choice here: "))
        if firstMenu == 1:
            print("\t1) Freshman\n\t2) Sophomore\n\t3) Junior\n\t4) Senior\n")
            gradeLevel = int(input("Enter a grade level here: "))
            if gradeLevel == 1:
                freshman += 1
            elif gradeLevel == 2:
                sophomore += 1
            elif gradeLevel == 3:
                junior += 1
            elif gradeLevel == 4:
                senior += 1
            else:
                print("Invalid. Choose a grade level between 1-4\n")
            testScore = -1
            testScore = int(input("Enter the test score: ")
            while testScore >= 0 or testScore <= 100:
                testScore = int(input("Enter the test score: ")
            else:
                print("Invalid test score. Must be between 0 and 100")
            while testScore >= 0 or testScore <= 100:
                total+=testScore

            while secondMenu >= 1 and secondMenu <= 4:
                studentCount += 1

        else:
            print("Good bye.\n")
            print("Total number of students entered:\t",studentCount,"\n")
            print("Average test score:\t",averageGrade)
            print("Freshman:\t", freshman)
            print("Sophomores:\t", sophomore)
            print("Juniors:\t", junior)
            print("Seniors:\t", senior)
main()
