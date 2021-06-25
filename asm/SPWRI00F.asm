*          DATA SET SPWRI00F   AT LEVEL 005 AS OF 08/24/99                      
*          DATA SET SPWRI00    AT LEVEL 010 AS OF 08/19/99                      
*PHASE T20400C,*                                                                
         TITLE 'T20400 - SPOTPAK WRITER CONTROLLER'                             
*                                                                               
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
* TEMP VERSION TO FIX ISDDS BUG                                                 
* DO NOT MAKE THIS FUCKER LIVE OR EVAN WILL GO 'DAY-TRADER' ON YOUR ASS         
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
*                                                                               
T20400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20400,CLEAR=YES                                           
         USING WORKD,RC                                                         
         LM    R2,R4,8(R1)                                                      
         USING COMFACSD,R4         R4=A(COMFACS)                                
*                                                                               
         MVC   PLIST,=XL4'FEFFFFFF'                                             
         GOTO1 CSWITCH,PLIST                                                    
         ICM   R2,15,PLIST                                                      
         LA    R2,0(R2)            R2=A(SYSFACS)                                
         LTR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING SYSFACD,R2                                                       
*                                                                               
         GOTO1 CDATAMGR,PLIST,=C'DTFAD',=C'SPTDIR',0,0                          
         ICM   RF,15,PLIST+12                                                   
         ST    RF,ADTF                                                          
         ST    RF,P4               P4=A(DTF)                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P6,=XL4'2AB31300'   SCREWED UP D/A                               
*                                                                               
         LA    R3,IO                                                            
         ST    R3,AIO                                                           
         GOTO1 VDADDS,P1,01,AIO,0,,P6                                           
         OC    P3(2),P3                                                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   2(4,R3),=XL4'2AA51300'                                           
         XC    P3,P3                                                            
         MVC   P3+2(2),0(R3)                                                    
*                                                                               
         MVC   P6,=XL4'2AB31300'   SCREWED UP D/A                               
*                                                                               
         GOTO1 VDADDS,P1,04,(R3),0,,P6                                          
         OC    P3(2),P3                                                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XMOD1 ,                                                                
         LTORG                                                                  
*                                                                               
WORKD    DSECT                                                                  
ADTF     DS    A                                                                
AIO      DS    A                                                                
PLIST    DS    6F                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
IO       DS    8096C                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIFFD                                                       
         ORG     CONTAGH                                                        
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FATCB                                                          
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPWRI00F  08/24/99'                                      
         END                                                                    
