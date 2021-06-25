*          DATA SET EASICPY    AT LEVEL 016 AS OF 05/01/02                      
*PHASE EASICPY,*,NOAUTO                                                         
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'EASICPY - COPY AN UNDEFINED FORMAT EASI TAPE'                   
         PRINT NOGEN                                                            
EASICPY  CSECT                                                                  
         NBASE 0,EASICPY,=V(REGSAVE)                                            
         SPACE 2                                                                
         BAS   RE,PRNT                                                          
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
GET      DS    0H                                                               
         GET   IN,REC                                                           
         CLI   EOFSW,C'Y'                                                       
         BE    EOJ                                                              
*                                                                               
         AP    INCNT,=P'1'                                                      
         BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                                                               
EOF      DS    0H                                                               
         BAS   RE,PRNT                                                          
         LA    R3,COUNTS                                                        
         LA    R4,25                                                            
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
*                                                                               
         MVI   EOFSW,C'Y'                                                       
         XIT1                      RETURN TO GETREC CALL                        
*                                                                               
EOJ      DS    0H                                                               
         CLOSE (IN,,OUT,)                                                       
         XBASE                                                                  
         SPACE 2                                                                
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT3    MVC   PCOM,=C'BL03'                                                    
         AP    LNCNT,=P'3'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT2    MVC   PCOM,=C'BL02'                                                    
         AP    LNCNT,=P'2'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         B     XIT                                                              
         SPACE 3                                                                
PUTREC   NTR1                                                                   
         MVC   OUT+82(2),IN+82     SET OUTPUT LENGTH = INPUT LENGTH             
         PUT   OUT,REC                                                          
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               RECFM=U,                                                X        
               BLKSIZE=4000,                                           X        
               LRECL=4000,                                             X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
         EJECT                                                                  
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=U,                                                X        
               MACRF=PM                                                         
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
         DS    0F                                                               
WORK     DS    CL256                                                            
PCOM     DS    CL4                                                              
*                                                                               
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
EOFSW    DC    C'N'                                                             
DMPCNT   DC    PL5'1000'                                                        
CPYCNT   DC    PL5'99999999'                                                    
CPRESP   DC    H'110'          110 CARDS PER RESP                               
FRSTCRD  DC    H'1'                                                             
LASTCRD  DC    H'110'                                                           
CARD     DS    CL80                                                             
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4000X                                                            
         DS    D                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016EASICPY   05/01/02'                                      
         END                                                                    
