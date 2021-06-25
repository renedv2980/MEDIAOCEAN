*          DATA SET SPREPFXBS2 AT LEVEL 018 AS OF 01/09/97                      
*PHASE SPFX02V                                                                  
         TITLE 'SPFX02 - ADD MISSING BS BUY RECORDS TO SPTFIL2'                 
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       DS    0H                                                               
         XC    COUNT,COUNT                                                      
         OPEN  (FILEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX2      L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         GET   FILEIN,(R0)                                                      
*                                                                               
         L     R6,ADBUY                                                         
         CLI   RCWRITE,C'N'                                                     
         BE    FX10                                                             
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE',KEY+14,(R6),DMWORK           
*                                                                               
FX10     DS    0H                                                               
         MVC   P(5),=C'ADDED'                                                   
         GOTO1 HEXOUT,DMCB,(R6),P+6,20,=C'TOG'                                  
         GOTO1 REPORT                                                           
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         B     FX2                                                              
                                                                                
                                                                                
FX100    MVC   P(5),=C'COUNT'                                                   
         EDIT  (4,COUNT),(8,P+10)                                               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,LRECL=2008,             X        
               MACRF=GM,BLKSIZE=16000,EODAD=FX100                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
COUNT    DS    F                                                                
SAVEKEY  DS    XL18                                                             
ELCODE   DS    X                                                                
STAWORK  DS    XL31                                                             
BAGYTAB  DS    16XL4                                                            
         DS    F                                                                
RECORD   DS    CL50                                                             
SPACE    DS    XL2000                                                           
                                                                                
*                                                                               
AGENCYD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPREPFXBS201/09/97'                                      
         END                                                                    
