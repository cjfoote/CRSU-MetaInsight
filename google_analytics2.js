(function(i,s,o,g,r,a,m){
  i['GoogleAnalyticsObject']=r;
  i[r]=i[r] || 
  function(){
    (i[r].q=i[r].q||[]).push(arguments);
  },i[r].l=1*new Date();
  a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;
  m.parentNode.insertBefore(a,m);
})(window,document,'script',
  'https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-135597033-7', 'auto');
ga('send', 'pageview');

// Event Tracking Code

$(document).on('shiny:inputchanged', function(event) {
  if(event.name == 'data' ){
    ga('send', 'event', 'input',
      'Upload data', event.name, event.value);
  }
});




