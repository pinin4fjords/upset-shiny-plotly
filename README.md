# upset-shiny-plotly

UpSet plots from a Shiny app, rendered with Plotly

## Description

A very simple Shiny dashboard, illustrating how to make an upset plot from scratch, rendering using the method of Lex, Gehlenborg et al [1]. It's much simpler than their interactive version, but is an improvement on the static plot produce by UpSetR for Shiny purposes. I've build a similar thing into a module in <a href = \"https://github.com/pinin4fjords/shinyngs">Shinyngs</a>, but thought someone might find this standalone code useful.

Additional to the traditional UpSet plot based on the areas of Venn/Euler diagrams where items are essentially assigned to the highest order interaction with which they're involved, this plot also allows a display where ALL items matching a given intersection can be included.

## References

* Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). <a href=\"http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html\">http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html</a>
* Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, <a href=\"https://CRAN.R-project.org/package=UpSetR\">https://CRAN.R-project.org/package=UpSetR</a>.
