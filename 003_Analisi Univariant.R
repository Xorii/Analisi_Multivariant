attach(df)


#ANÀLISI UNIVARIANT#

#Birth
hist(birth, ylab = "Any", main = "Any de naixement", col = "orange")

#COMPTE: hi ha naixements força sospitosos (inclús d'abans del 1900)


#Educ
pie(table(educ), main = "Nivell d'estudis")


#Status
barplot(table(status), main="Estat civil", 
        col = "orange")
pie(table(status), main ="Estat civil")

#COMPTE: hi ha respostes rares (tipus "YOLO", "Absurd" i alguna altra)


#Renda
hist(renda, xlab = "Unitats monetàries", main = "Renda anual", col = "orange")

#COMPTE: hi ha un outlier


#Kids
barplot(table(kids), main="Nombre de nens a la família (0-12 anys)", 
        col = "orange")
pie(table(kids), main="Nombre de nens a la família (0-11 anys)")

#NOTA: Nens no té en compta adolescents. Nens no són fills.


#Teens
barplot(table(teens), main="Nombre d'adolescents a la família (13-19 anys)", 
        col = "orange")
pie(table(teens), main="Nombre d'adolescents a la família (13-19 anys)")


#Recency
hist(recency, main = "Dies transcorreguts des de la darrera compra", 
     col = "orange", breaks = 100)


#Complain
pie(table(comp), main="Clients que han realitzat alguna queixa", 
    col = c("skyblue1","sienna1"))


#Wine
hist(wine, main = "Despesa en vi durant els darrers dos anys", 
     xlab = "Vi (Unitats Monetàries)", col = "orange")

#Fruit
hist(fruit, main = "Despesa en fruita durant els darrers dos anys", 
     xlab = "Fruita (Unitats Monetàries)", col = "orange")

#Meat
hist(meat, main = "Despesa en carn durant els darrers dos anys", 
     xlab = "Carn (Unitats Monetàries)", col = "orange")


#Fish
hist(fish, main = "Despesa en peix durant els darrers dos anys", 
     xlab = "Peix (Unitats Monetàries)", col = "orange")


#Sweet
hist(sweet, main = "Despesa en dolços durant els darrers dos anys", 
     xlab = "Dolços (Unitats Monetàries)", col = "orange")

#Or
hist(gold, main = "Despesa en or durant els darrers dos anys", 
     xlab = "Or (Unitats Monetàries)", col = "orange")

#FALTEN VARIABLES


detach(df)
