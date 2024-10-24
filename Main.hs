-- car dealership manager
import Data.List(find)
import Text.Read (readMaybe)
-- defining the Car data type
data Car = Car{ carId :: Int, model :: String, price :: Float, year :: Int} deriving( Show, Eq)

--defining the User data type
data User = User{ name :: String, address :: String, customerId :: Int, carsPurchased :: [Car]} deriving( Show, Eq)

--defining the Sale data type
data Sale = Sale{ soldCar :: Car, buyer :: User} deriving (Show)

--defining type aliases for Inventory, users and Sales
type Inventory = [Car]
type Users = [User]
type Sales = [Sale]

--Function to add a car to inventory when it passes validation
addCar :: Inventory -> Car -> Inventory
addCar inventory car = car : inventory

-- function for validating user input and error prevention
addCarValidation :: String -> Inventory -> Users -> Sales -> IO ()
addCarValidation m inventory users sales = putStrLn "\nEnter unique Car ID number:" >>
                                           getLine >>= \i ->
                                           case readMaybe i :: Maybe Int of
                                               Just newCarId -> if any (\car -> carId car == newCarId) inventory then
                                                                putStrLn "Error: A car with this ID already exists." >>
                                                                addCarValidation m inventory users sales
                                                            else
                                                                putStrLn "\nEnter Price:" >>
                                                                getLine >>= \p ->
                                                                case readMaybe p :: Maybe Float of
                                                                    Just price -> putStrLn "\nEnter Year:" >>
                                                                                  getLine >>= \y ->
                                                                                  case readMaybe y :: Maybe Int of
                                                                                      Just year -> let newCar = Car newCarId m price year in
                                                                                                   let newInventory = addCar inventory newCar in
                                                                                                   putStrLn "\nCar successfully added to inventory." >>
                                                                                                   menu >>= \x -> return (x, newInventory, users, sales) >>= process
                                                                                      Nothing -> putStrLn "Error: Invalid year. Please enter a valid number." >>
                                                                                                 addCarValidation m inventory users sales
                                                                    Nothing -> putStrLn "Error: Invalid price. Please enter a valid number." >>
                                                                               addCarValidation m inventory users sales
                                               Nothing -> putStrLn "Error: Invalid car ID. Please enter a valid number." >>
                                                          addCarValidation m inventory users sales

-- function to add a user to list of users
addUser :: Users -> User -> IO Users
addUser users newUser = 
  if any (\user -> customerId user == customerId newUser)users then
  putStrLn "Error: A user with this ID already exists." >> return users
  else return (newUser: users)
addUserValidation :: String -> String -> Inventory -> Users -> Sales -> IO ()
addUserValidation n a inventory users sales = putStrLn "\nEnter Customer ID:" >>
                                              getLine >>= \c ->
                                              case readMaybe c :: Maybe Int of
                                                  Just customerId -> let newUser = User n a customerId [] in 
                                                                     addUser users newUser >>= \newUsers -> if length newUsers > length users then 
                                                                        putStrLn "\nUser successfully added." >>
                                                                         menu >>= \x -> return (x, inventory, newUsers, sales) >>= process
                                                                      else addUserValidation n a inventory users sales
                                                                 
                                                                     
                                                  Nothing -> putStrLn "Error : Invalid customer ID. Please enter a valid number" >> 
                                                             addUserValidation n a inventory users sales

-- function for removing a car from inventory 
removeCar :: Inventory -> Int -> (Inventory, Maybe Car)
removeCar inventory carIdToRemove =
    case find (\car -> carId car == carIdToRemove) inventory of 
        Just car -> (filter(\c -> c /= car) inventory, Just car)
        Nothing -> (inventory, Nothing)

-- function for selling a car to customer
sellCar :: Inventory -> Users -> Sales -> Int -> Int -> IO (Inventory, Users, Sales)
sellCar inventory users sales customerIdToSell carIdToSell = 
    let (newInventory, car) = removeCar inventory carIdToSell in
    case car of
        Just car -> case find (\user -> customerId user == customerIdToSell) users of
            Just user -> let newUser = user { carsPurchased = car : carsPurchased user }
                             newUsers = newUser : filter (\u -> customerId u /= customerIdToSell) users
                             newSale = Sale car user
                             newSales = newSale : sales in
                         putStrLn ("Car with ID " ++ show (carId car) ++ " sold to " ++ name user ++ ".") >>
                         return (newInventory, newUsers, newSales)
            Nothing -> putStrLn ("Error: User with ID " ++ show customerIdToSell ++ " not found.") >>
                       return (inventory, users, sales)
        Nothing -> putStrLn ("Error: Car with ID " ++ show carIdToSell ++ " not found in inventory.") >>
                   return (inventory, users, sales)

--function to view inventory
viewInventory :: Inventory -> IO ()
viewInventory [] = putStrLn "No cars found in inventory."
viewInventory inventory = mapM_ printCar inventory

-- helping function for viewInventory to print car details
printCar :: Car -> IO()
printCar car = putStrLn $ "Car ID:" ++ show (carId car) ++ ", Model: " ++ model car ++ ", Price: " ++ show (price car) ++ ", Year: " ++ show (year car)

-- function to view users
viewUsers :: Users -> IO ()
viewUsers [] = putStrLn "No users in database."
viewUsers users = mapM_ printUser users

-- assistant function to view users to print user details
printUser :: User -> IO()
printUser user = do
    putStrLn $ "Name: " ++ name user
    putStrLn $ "Address: " ++ address user
    putStrLn $ "Customer ID: " ++ show (customerId user) 
    putStrLn "Cars Purchased: "
    mapM_ printCar (carsPurchased user)


--function to view total sales
viewSales :: Sales -> IO()
viewSales [] = putStrLn "No sales have been made."
viewSales sales = mapM_ printSale sales

--helping function for viewSales to print sale details
printSale :: Sale -> IO()
printSale sale = do

    putStrLn "Sale Details:"
    putStrLn $ "Buyer Name: " ++ name (buyer sale)
    putStrLn $ "Buyer Address: " ++ address (buyer sale)
    putStrLn $ "Buyer Customer ID: " ++ show (customerId (buyer sale)) 
    
    putStrLn "Sold Car Details:"
    putStrLn $ "Car ID: " ++ show (carId (soldCar sale))
    putStrLn $ "Model: " ++ model (soldCar sale)
    putStrLn $ "Price: " ++ show (price (soldCar sale))
    putStrLn $ "Year: " ++ show (year (soldCar sale))

--function to search a car by model name
searchByModel :: Inventory -> String -> IO ()
searchByModel [] _ = putStrLn "No more cars found."
searchByModel (x:xs) modelToSearch 
    | model x == modelToSearch = printCar x >> searchByModel xs modelToSearch
    | otherwise                = searchByModel xs modelToSearch

--function for displaying menu and getting user's choice input
menu :: IO String 
menu = 
       putStrLn "\nChoose an option below" >>
       putStrLn "==============================" >>
       putStrLn "1. Add Car" >>
       putStrLn "2. Add a Customer" >>
       putStrLn "3. Sell Car" >>
       putStrLn "4. View Inventory" >>
       putStrLn "5. View All Users" >>
       putStrLn "6. Search Car by Model" >>
       putStrLn "7. View Sales" >>
       putStrLn "8. Exit" >>
       getLine

-- function to process user's choice input
process :: (String, Inventory, Users, Sales) -> IO()
process ("1", inventory, users, sales) = putStrLn "\nEnter Model name:" >>
                                         getLine >>= \m ->
                                         addCarValidation m inventory users sales

process ("2", inventory, users, sales) = putStrLn "\nEnter User Name:" >>
                                         getLine >>= \n ->
                                         putStrLn "\n Enter User Address">>
                                         getLine>>= \a -> addUserValidation n a inventory users sales

process ("3", inventory, users, sales) = putStrLn "\nEnter Car ID to Sell:" >>
                                         getLine >>= \c ->
                                         case readMaybe c :: Maybe Int of
                                             Just carId -> putStrLn "\nEnter Customer ID:" >>
                                                           getLine >>= \u ->
                                                           case readMaybe u :: Maybe Int of
                                                               Just customerId -> sellCar inventory users sales customerId carId >>= \(newInventory, newUsers, newSales) ->
                                                                                   menu >>= \x -> return (x, newInventory, newUsers, newSales) >>= process
                                                               Nothing -> putStrLn "Error: Invalid customer ID. Please enter a valid number." >>
                                                                          process ("3", inventory, users, sales)
                                             Nothing -> putStrLn "Error: Invalid car ID. Please enter a valid number." >>
                                                        process ("3", inventory, users, sales)

process ("4", inventory, users, sales) = viewInventory inventory >>
                                        menu >>= \x -> return (x, inventory, users, sales) >>= process


process ("5", inventory, users, sales) = viewUsers users >>
                                        menu >>= \x -> return (x, inventory, users, sales) >>= process

process("6", inventory, users, sales) = putStrLn "\nEnter Model to Search:" >> 
                                        getLine >>= \m -> 
                                        searchByModel inventory m >>
                                        menu >>= \x -> return (x, inventory, users, sales) >>= process

process("7", inventory, users, sales) = viewSales sales >> 
                                        menu >>= \x -> return (x, inventory, users, sales) >>= process

process("8", _, _ ,_) = putStrLn "Bye"

process(_, inventory, users, sales) = putStrLn "Invalid Option. Please try again." >>
                                        menu >>= \x -> return (x, inventory, users, sales) >>= process

-- main function used for starting the program initially
main :: IO()
main = menu >>=  \x-> return (x,[],[],[]) >>= process