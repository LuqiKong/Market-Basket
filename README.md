# Market-Basket
Uncover associations between items

## Data
The dataset used in this analysis is from the 'Groceries' data in 'arules' R package.
It was collected from a real-world grocery for one-month operation, containing 9835 transactions and 169 unique products.

Transactions could be written in two formats:

‘basket’ format - each row in the transaction data file represents a transaction where the items (item labels) are separated by characters such as comma, tab, etc. 

‘single’ format - each row corresponds to a single item, containing at least ids for the transaction and the item.

The two formats can be read using ```read.transactions(...,format = c('single','basket'))``` function in the 'arules' package.
For reference, please go to [R documentation page](https://www.rdocumentation.org/packages/arules/versions/1.5-5/topics/read.transactions)
