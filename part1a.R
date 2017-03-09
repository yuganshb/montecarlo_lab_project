f<-function(g,l,a)
{
  a=1/a;
  return (log(1-g^a)*-1/l);
}

func<-function(l,a,n)
{
  f1<-vector(length=n);
  i=0;
  exp<-vector(length=n);
  while(i<n)
  {
    u<-runif(1,0,1);
    g=((1+l)-sqrt(1+l^2+2*l-4*u*l))/2;
    exp[i]=f(g,l,a);
    i=i+1;
  }
  png("exp.png");
  hist(exp,breaks=50);
}

func(0.5,2,1000);

