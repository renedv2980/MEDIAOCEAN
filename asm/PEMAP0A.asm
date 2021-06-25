*          DATA SET PEMAP0A    AT LEVEL 037 AS OF 08/22/00                      
*PHASE TE1B0AA                                                                  
         TITLE 'TE1B0A - TEST DRONE MODULE'                                     
TE1B0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1B0A**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO             RELOCATION FACTOR                            
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A39'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADRONE,DMCB         A(DRONE)                                     
*                                                                               
         MVI   BOMB,C'N'                                                        
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    VR                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       XC    DRGEN,DRGEN         INITIALIZE DRONE BLOCK FOR INPUT             
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VR20                                                             
         MVI   BOMB,C'Y'                                                        
         LA    R0,BUFFER                                                        
         ST    R0,DRSTBUF                                                       
         LA    R0,BUFFERX                                                       
         ST    R0,DRENDBUF                                                      
*                                                                               
VR20     MVI   DRWHO,DRNETWHO                                                   
         MVI   DRACTION,DRINIT                                                  
         MVC   DRDICT,=CL8'ALAN'                                                
         MVC   DRALTDIC,=CL8'ALANALT'                                           
         MVC   DRCOMFAC,ACOMFACS                                                
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
         LA    R2,TSTROWSH                                                      
         LA    R3,TSTRLSTH                                                      
         MVI   ROWFOUND,C'N'                                                    
*                                                                               
VR30     CLI   5(R2),0                                                          
         BE    VR40                                                             
         ST    R2,DRNETFLD                                                      
         MVI   DRACTION,DRROW                                                   
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
         MVI   ROWFOUND,C'Y'                                                    
*                                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VR40                                                             
         MVI   DRACTION,DRGENROW                                                
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
VR40     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R3                                                            
         BNH   VR30                                                             
*                                                                               
         CLI   ROWFOUND,C'Y'                                                    
         BE    *+18                                                             
         LA    R2,TSTROWSH                                                      
         MVC   DRERRMSG(25),=C'AT LEAST ONE ROW REQUIRED'                       
         B     ERR                                                              
*                                                                               
         LA    R2,TSTCOLSH                                                      
         LA    R3,TSTCLSTH                                                      
         MVI   COLFOUND,C'N'                                                    
*                                                                               
VR50     CLI   5(R2),0                                                          
         BE    VR60                                                             
         ST    R2,DRNETFLD                                                      
         MVI   DRACTION,DRCOL                                                   
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
         MVI   COLFOUND,C'Y'                                                    
*                                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         BNE   VR60                                                             
         MVI   DRACTION,DRGENCOL                                                
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
VR60     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R3                                                            
         BNH   VR50                                                             
*                                                                               
         CLI   COLFOUND,C'Y'                                                    
         BE    *+18                                                             
         LA    R2,TSTCOLSH                                                      
         MVC   DRERRMSG(28),=C'AT LEAST ONE COLUMN REQUIRED'                    
         B     ERR                                                              
*                                                                               
         MVI   DRACTION,DRWRAPUP                                                
         GOTO1 ADRONE,DMCB,DRGEN                                                
         CLI   DRERROR,0                                                        
         BNE   ERR                                                              
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
ERR      MVC   CONHEAD,DRERRMSG                                                 
         CLI   BOMB,C'Y'                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ERREX2                                                           
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
EXIT     XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
       ++INCLUDE PEMAPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPD8D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         DS    0D                                                               
RELO     DS    F                   RELOCATION FACTOR                            
ADRONE   DS    A                   A(DRONE)                                     
BOMB     DS    C                                                                
ROWFOUND DS    C                   A ROW WAS FOUND                              
COLFOUND DS    C                   A COLUMN WAS FOUND                           
         SPACE 5                                                                
       ++INCLUDE DRONEBLKD                                                      
         SPACE 5                                                                
BUFFER   DS    1024X                                                            
BUFFERX  EQU   *                                                                
         SPACE 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037PEMAP0A   08/22/00'                                      
         END                                                                    
