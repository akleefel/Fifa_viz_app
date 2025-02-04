# Fifa_viz_app


Link to shiny app :  [Fifa_viz_app](https://akleefel.shinyapps.io/fifa_viz_app/)

### App Description

This shiny app uses data from the `Kaggle Fifa 2018` data set and visualizes the relationships of selected football player attributes (`height`, `weight`, `passing`, `shooting`, ... ), as well as the distribution of selected attributes across different attributes across different countries and leagues. Assuming, that the player attributes in the Fifa computer game realistically reflect the real-world properties of many players some implications can be made on the real relationships between certain attributes.

### Usage Scenario

Let's say you would like to know whether taller defenders are generally better at shooting than smaller players and how this relationship holds across different leagues. Well, you could simply follow these steps:

1. Open the [Fifa_viz_app](https://akleefel.shinyapps.io/fifa_viz_app/) in your browser
2. Select `Defence` from the Position dropdown,  `Height (cm)` as the x-axis variable and `Shooting` as the y-axis variable
3. Check out the relationship of the variable:

![height_shoot.JPG](data/height_shoot.JPG)

4. Flip through different leagues and positions

Now, let's say you would like to find out whether defenders in England are heavier taller than players in Germany:

1. Open the [Fifa_viz_app](https://akleefel.shinyapps.io/fifa_viz_app/) in your browser
2. Select `Weight kg` as the x-axis variable
3. Select the "X-Variable Distribution" tab at the top of the page

![weight_distr.JPG](data/weight_distr.JPG)

4. Flip through different countries, positions and leagues to compare the distributions.

### Data Sources

|Dataset|Link|
|---|---|
|Kaggle Fifa '18 Data Set|[Kaggle_Fifa](https://www.kaggle.com/kevinmh/fifa-18-more-complete-player-dataset)|
