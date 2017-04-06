func<-function(n)
{
  a=0:5000;
  a=a/1000;
  
  w<-matrix(ncol=10,nrow=5001);
  for(j in 1:10)
  {
    for(i in 1:5000)
    {
      z<-rnorm(1,0,1);
      w[j][i+1]=w[j][i]+sqrt(a[i+1]-a[i]);
    }
  }
  png("brown1.png");
  plot(a,w[1]);
  par(new=TRUE);
  
}
func();
    
