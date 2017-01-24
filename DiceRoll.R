# TODO: Add comment
# 
# Author: cartera
###############################################################################

#Players current place starting at "GO"
CurrSpace<-0

#Loading excel library, and data from excel sheet into a data frame
library("xlsx")
data <- read.xlsx("/home/cartera/Documents/MonopolyBoard.xlsx", sheetIndex = 1)
jailBreak <- 0

#Creating a dataframe to capture logs for auditing purposes
diceRoll_Log <- list()
rowForLog_Matrix <- matrix(,1,4)
rowForLog <- data.frame(Dice.Order=NA, Property=NA, Landed.On.Count=NA, Last_Roll=NA, stringsAsFactors=FALSE)

print("Before for loop")

#not sure why this function wont update the data frame when called. Research this.
#updateLandCount <- function(dataframe, currentSpace)
# {
#	print("In function")
#	print(currentSpace)
#	CountToUpdate <- dataframe$Landed.On.Count[dataframe$Dice.Order == currentSpace]
#	print(CountToUpdate)
#	UpdatedCount <- CountToUpdate+1
#	print(UpdatedCount)
#	dataframe <- within(dataframe, Landed.On.Count[Dice.Order == currentSpace] <- UpdatedCount)
#	print(dataframe$Landed.On.Count[dataframe$Dice.Order == currentSpace])
# }

#number of dice-rolls in the expiriment controlled by the for loop counter
for(i in 1:100000)
{
	dice_1 <- sample(1:6,1)
	dice_2 <- sample(1:6,1)
	roll <- dice_1 + dice_2

	#extra logic to traverse the board, once you pass go to realign. 
	if((CurrSpace + roll) > 40)
	{
		carryover = CurrSpace-40
		CurrSpace = carryover + roll		
	}
	#if a player lands on Go-to-Jail, move their piece to the In Jail space
	else if(CurrSpace == 30)
	{
		print("Go to Jail!")
		CurrSpace <- 10
		
		while(dice_1 != dice_2 || jailBreak <= 3)
		{
			jailBreak <- jailBreak +1
			dice_1 <- sample(1:6,1)
			dice_2 <- sample(1:6,1)
			print(jailBreak)
		}
		roll <- dice_1 + dice_2
		#roll <- 1
		CurrSpace=CurrSpace+roll
	}
	else
	{
	#simulating a players movement based on dice roll
	CurrSpace=CurrSpace+roll
	}
	#Update the count of how many times a player has landed on a property
	CountToUpdate <- data$Landed.On.Count[data$Dice.Order == CurrSpace]
	UpdatedCount <- CountToUpdate+1
	data <- within(data, Landed.On.Count[Dice.Order == CurrSpace] <- UpdatedCount)
	
	rowForLog <- subset(data, data$Dice.Order==CurrSpace, c(1,2,5))
	rowForLog[1,4]<-roll
	rowForLog_Matrix <- rbind(rowForLog_Matrix, as.matrix(rowForLog))
	
	print(CurrSpace)
}

result <- data.frame(data$Dice.Order,data$Property,data$Landed.On.Count)
print(result)


