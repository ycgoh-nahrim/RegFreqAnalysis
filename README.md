# RegFreqAnalysis
Regional frequency analysis for annual maximum rainfall
<p></p>
<ul type="disc" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
 margin-top:0in;margin-bottom:0in">
    <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Ref</span></li>
    <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
  margin-top:0in;margin-bottom:0in">
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Hosking, J. R. M., &amp; Wallis, J. R. (1997). </span><span style="font-style:italic;font-family:
      Calibri;font-size:11.0pt">Regional Frequency Analysis: An Approach Based on L-Moments</span><span style="font-family:Calibri;font-size:11.0pt">. Cambridge University Press.</span></li>
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">HEC (Index flood regionalization Pt III &amp; IV)</span></li>
        <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
   margin-top:0in;margin-bottom:0in">
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><a href="https://www.youtube.com/watch?v=hjdnlbxrje8"><span style="font-family:Calibri;font-size:11.0pt">https://www.youtube.com/watch?v=hjdnlbxrje8</span></a></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><a href="https://www.youtube.com/watch?v=8HkztqnptIA"><span style="font-family:Calibri;font-size:11.0pt">https://www.youtube.com/watch?v=8HkztqnptIA</span></a></li>
        </ul>
    </ul>
    <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Objective</span></li>
    <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
  margin-top:0in;margin-bottom:0in">
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Identify homogeneous regions </span></li>
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Fit probability distribution for each region</span></li>
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Get quantile function for all stations</span></li>
    </ul>
    <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">For one duration each time, output in xlsx, then combine all durations in 1 xlsx</span></li>
    <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Input</span></li>
    <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
  margin-top:0in;margin-bottom:0in">
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Annual maximum rainfall data for every duration</span></li>
        <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
   margin-top:0in;margin-bottom:0in">
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Output from raindata processing: rainfall_hr_dataviz_dt6.R</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Select duration for analysis</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">May set analysis before certain year (eg flood year)</span></li>
        </ul>
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Station list</span></li>
        <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
   margin-top:0in;margin-bottom:0in">
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Coordinates (cleaned)</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Elevation (preferably)</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Boundary: Administrative/river basin (or any other boundaries as regions)</span></li>
        </ul>
    </ul>
    <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Output</span></li>
    <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
  margin-top:0in;margin-bottom:0in">
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Each duration, 1 xlsx</span></li>
        <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
   margin-top:0in;margin-bottom:0in">
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Region map</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Station</span></li>
            <ul type="square" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
    margin-top:0in;margin-bottom:0in">
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Regional L-moments</span></li>
            </ul>
            <ul type="square" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
    margin-top:0in;margin-bottom:0in">
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle;color:red"><span style="font-family:Calibri;font-size:11.0pt">max</span></li>
            </ul>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Discordancy - each station</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Heterogeneity for each region</span></li>
            <ul type="square" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
    margin-top:0in;margin-bottom:0in">
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">H1, H2, H3</span></li>
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Different values for every recalculation</span></li>
            </ul>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Z-score</span></li>
            <ul type="square" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
    margin-top:0in;margin-bottom:0in">
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Each region, each distribution (GLO, GEV, GNO, PE3, GPA)</span></li>
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Different values for every recalculation</span></li>
            </ul>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Prob distribution parameters values</span></li>
            <ul type="square" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
    margin-top:0in;margin-bottom:0in">
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Each region</span></li>
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Choose the best probability distribution based on Z score</span></li>
            </ul>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">L-moments for each region</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Index variable (rainfall) for each station</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Quantile</span></li>
            <ul type="square" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
    margin-top:0in;margin-bottom:0in">
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Each region</span></li>
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Can be set</span></li>
            </ul>
        </ul>
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Yaml file </span></li>
        <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
   margin-top:0in;margin-bottom:0in">
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Same info in xlsx file</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Each regions&apos;</span></li>
            <ul type="square" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
    margin-top:0in;margin-bottom:0in">
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">GEV parameters</span></li>
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">L-moments</span></li>
                <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Index variable (rainfall) for each station in the region</span></li>
            </ul>
        </ul>
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Rds file</span></li>
        <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
   margin-top:0in;margin-bottom:0in">
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Can be reimport inside script</span></li>
        </ul>
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Txt files - each region + all regions</span></li>
        <ul type="circle" style="margin-left:.375in;direction:ltr;unicode-bidi:embed;
   margin-top:0in;margin-bottom:0in">
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Same info in xlsx file</span></li>
            <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Each stations&apos; L-moments, discordancy</span></li>
        </ul>
        <li style="margin-top:0;margin-bottom:0;vertical-align:middle"><span style="font-family:Calibri;font-size:11.0pt">Combine all xlsx (all durations) into one xlsx</span></li>
    </ul>
</ul><br>
<p></p>
