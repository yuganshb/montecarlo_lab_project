f<-function(g,l,a)
{
  a=1/a;
  return (log(1-g^a)*-1/l);
}

f1<-function(x,l,a)
{
  return ((1+l)-2*(1-exp(-1*l*x))^a);
}

func<-function(l,a,n)
{
  exp<-vector(length=n);
  i=0;
  while(i<n)
  {
    u1<-runif(1,0,1);
    u2<-runif(1,0,1);
    
    x=f(u1,l,a);
    c=1+l;
    r=f1(x,l,a);
    
    if(c*u2<r)
     {
       exp[i]=x;
       i=i+1;
     }
  }
  png("exp2.png");
  hist(exp,breaks=50);
}

func(0.5,2,1000);
