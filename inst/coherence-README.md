# The `coherence` metric

In the application, when a data frame is provided as input, we study annotations using two metrics. One of them is the adjusted mutual information score, a well known [metric](https://en.wikipedia.org/wiki/Adjusted_mutual_information) for comparing classifications that ranges from 0 (random classification) to 1 (identical classification). The second metric that we use to assess how well a particular subset of cells is classified is what we call the coherence metric. A coherence metric is an attempt to understand the extent to which the two classifications are coherent or incoherent to each other. This begs the question: what does it mean to be coherent?

![](coherent.jpg)

In the figure above you can see two examples of a coherent classification, where the relationships are 1 to 1 and 1 to 2 in panels (a) and (b), respectively. That is, we can see that class 'C' in the left side of panel (a) is always linked to class 'c' on the right side, where class 'c' is not linked to any other class. Likewise, class 'C' in the left side of panel (b) is always linked to classes 'e' and 'd', where neither class is linked to any other class. Notice that the panel (b) would represent the comparison between perfect classifications that are defined at different resolutions. 

So, what does it mean to be incoherent?

![](incoherent.jpg)

Above is an example of two classification that are incoherent for certain classes. For example, class 'B' on the left side of the diagram is linked to either class 'b' or class 'a'; however, class 'a' on the right side is in turn associated to classes 'B' and 'A'. This is an incoherent classification because the differences reflected by the link 'B-a' (red link) where not picked up by either classification. Likewise, another example of these incoherences is the fact that classes 'D' and 'F' are both linked to classes 'c' and 'd', which implies that there are points for which the different classifications disagree (blue links).
