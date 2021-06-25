*          DATA SET DMTEST1    AT LEVEL 003 AS OF 08/14/13                      
*PHASE DMTEST1A                                                                 
*INCLUDE DMDDNAME   <====                                                       
*INCLUDE DMDMGR                                                                 
*INCLUDE DMDTFSN    <== COPY OF TEST SYSTEM VERSION                             
*INCLUDE DMDADDS                                                                
*INCLUDE DMDALINK                                                               
*INCLUDE DMDAPTRS                                                               
*INCLUDE DMENQDEQ                                                               
*INCLUDE DMISDDS                                                                
*INCLUDE DMDYNDD                                                                
*INCLUDE DMRCVR                                                                 
*INCLUDE DMWRKR                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'DMTEST1- TEST NEW DMDDNAME ROUTINE'                             
         PRINT NOGEN                                                            
DMTEST   CSECT                                                                  
         NBASE 0,DMTEST,WORK=A(WORK)                                            
         SPACE 2                                                                
INIT     L     RF,=V(UTL)                                                       
         MVI   4(RF),X'04'                                                      
         L     RF,=V(UPDID)                                                     
         MVC   0(2,RF),=C'TT'                                                   
         B     DOIT                                                             
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
DOIT     GOTO1 =V(CARDS),PLIST,C,=C'RE00'                                       
         CLC   C(2),=C'/*'                                                      
         BE    EXIT                                                             
*                                                                               
DOIT1    GOTO1 =V(DMDDNAME),DMCB,=C'DDNAME',C                                   
         L     RE,DMCB+8                                                        
         MVC   FILEINFO,0(RE)                                                   
         MVC   P(9),C                                                           
         GOTO1 =V(HEXOUT),PLIST,DMCB+8,P+10,1,=C'TOG'                           
         GOTO1 =V(HEXOUT),PLIST,DDNAFILE,P+13,16,=C'TOG'                        
         MVC   P+46(8),DDNADDN                                                  
         GOTO1 =V(HEXOUT),PLIST,DDNAASFL,P+54,6,=C'TOG'                         
         MVC   P+67(5),DDNASENA                                                 
         MVC   P+73(3),DDNASEN3                                                 
         MVC   P+77(3),DDNASYN3                                                 
         MVC   P+81(20),DDNADSN                                                 
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         B     DOIT                                                             
         EJECT                                                                  
DUB      DC    D'0'                                                             
PLIST    DC    6F'0'                                                            
DMCB     DC    6F'0'                                                            
DSKADR   DC    F'0'                                                             
*                                                                               
FILEINFO DS    0XL60                                                            
       ++INCLUDE DMDDNAMED                                                      
*                                                                               
C        DC    CL80' '                                                          
*                                                                               
PCC      DC    X'00'                                                            
P        DC    CL132' '                                                         
*                                                                               
REC      DS    CL2000                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORK     DC    1000D'0'                                                         
         SPACE 2                                                                
SSB      CSECT                                                                  
         DC    X'0000',X'FF',X'00'                                              
         DC    X'00000000'                                                      
         DC    CL8' '                                                           
         DC    A(0),A(0)                                                        
         DC    XL32'00'                                                         
         SPACE 2                                                                
UTL      CSECT                                                                  
         DC    10F'0'                                                           
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DMTEST1   08/14/13'                                      
         END                                                                    
