int c,j,b;main(){char*f[]={"bo","nu","ni","pi","ki",""},s[]={14,16,0,-14,-12};while(c=getchar()){for(b=j=0;j<10;++j){if(c=="aeiouAEIOU"[j]){c+=s[j%=5];b=1;break;}}printf("%c%s",c,f[b?j:5]);}}
