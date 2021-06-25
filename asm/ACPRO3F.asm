*          DATA SET ACPRO3F    AT LEVEL 008 AS OF 04/24/07                      
*PHASE T60B3FA                                                                  
         TITLE 'T60B3F - SCHEME/COPY'                                           
T60B3F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B3F**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VKEY               VALIDATE KEY FIELDS                           
         CLI   MODE,VALREC                                                      
         BE    COPY               COPY RECORDS                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FIELDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
VKEY     CLI   CALLSP,0            UNLESS WE CAME FROM A SCREEN...              
         BE    VKEY10              ...FORGET PF LINE                            
         XC    WORK,WORK                                                        
         LA    R0,AS$PFRET                                                      
         GOTO1 GETTXT,WORK,(R0),('PFLMAX',SCHPFAH),(C'S',0)                     
         OI    SCHPFAH+6,X'80'                                                  
*                                                                               
VKEY10   LA    R2,SCHFCODH                                                      
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   WORK(8),=CL8'ALL'                                                
         BE    ERREXIT                                                          
         MVI   ERROR,NOCOMMA                                                    
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         LA    R3,SCHFCOD                                                       
         CLI   0(R3),C','          ',' IS A KEY SEPARATOR                       
         BE    ERREXIT                                                          
         LA    R3,1(R3)                                                         
         BCT   R0,*-12                                                          
         LA    R3,KEY                                                           
         USING SCHRECD,R3                                                       
         XC    SCHKEY,SCHKEY                                                    
         MVI   SCHKTYP,SCHKTYPQ                                                 
         MVI   SCHKSUB,SCHKSUBQ                                                 
         MVC   SCHKCPY(L'CUL),CUL                                               
         MVC   SCHKCODE,WORK                                                    
         MVI   ERROR,NOTFOUND                                                   
         GOTO1 READ                                                             
         BNE   ERREXIT                                                          
         LA    R2,SCHFNAMH                                                      
         MVI   ELCODE,SCHELQ                                                    
         L     R3,AIO                                                           
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         USING SCHELD,R3                                                        
         MVC   SCHFNAM,SCHNAME                                                  
         OI    SCHFNAMH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         LA    R2,SCHTCODH                                                      
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   WORK(8),=CL8'ALL'                                                
         BE    ERREXIT                                                          
         MVI   ERROR,NOCOMMA                                                    
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         LA    R3,SCHTCOD                                                       
         CLI   0(R3),C','          ',' IS A KEY SEPARATOR                       
         BE    ERREXIT                                                          
         LA    R3,1(R3)                                                         
         BCT   R0,*-12                                                          
         OC    SCHTCOD,SPACES                                                   
*                                                                               
         MVC   AIO,AIO2            ATTEMPT TO READ NEW RECORD INTO IO2          
*                                                                               
         LA    R3,KEY                                                           
         USING SCHRECD,R3                                                       
         MVC   SCHKCODE,SCHTCOD                                                 
         MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         OI    DMINBTS,X'08'       READ FOR DELETES TOO                         
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'ACSHKEY),KEY  TEST RECORD FOUND                        
         BE    ERREXIT                                                          
         NI    DMINBTS,X'FF'-X'08' CLEAR READ FOR DELETES                       
*                                                                               
         LA    R2,SCHTNAMH                                                      
         GOTO1 ANY                                                              
         OC    SCHTNAM,SPACES                                                   
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* COPY ALL RECORDS FROM EXISTING SCHEME TO NEW SCHEME (AIO=AIO2)      *         
***********************************************************************         
         SPACE 1                                                                
O        USING SCHRECD,R3                                                       
         USING SCHRECD,R2                                                       
COPY     L     R3,AIO1                                                          
         L     R2,AIO2                                                          
         XC    0(256,R2),0(R2)                                                  
         MVC   SCHKEY,O.SCHKEY                                                  
         MVC   SCHKCODE,SCHTCOD                                                 
         MVC   SCHRSTA(1),O.SCHRSTA                                             
         LA    R4,ACCORFST(R2)                                                  
O        USING SCHELD,R3                                                        
         USING SCHELD,R4                                                        
         MVI   ELCODE,SCHELQ       GET SCHEME DEFINITION ELEMENT                
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,O.SCHLN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SCHEL(0),O.SCHEL                                                 
         MVC   SCHNAME,SCHTNAM                                                  
         LA    R4,1(RE,R4)                                                      
*                                                                               
         L     R3,AIO1                                                          
         MVI   ELCODE,SCSELQ       GET SCHEME CATEGORY SEQUENCE ELEMENT         
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
O        USING SCSELD,R3                                                        
         USING SCSELD,R4                                                        
         SR    RE,RE                                                            
         IC    RE,O.SCSLN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SCSEL(0),O.SCSEL                                                 
         LA    R4,1(RE,R4)                                                      
         MVI   0(R4),0                                                          
         LA    R4,1(R4)                                                         
         SR    R4,R2                                                            
         STCM  R4,3,SCHRECD+ACCORLEN                                            
         GOTO1 PERSIN              UPDATE PERSONAL ELEMENT                      
         GOTO1 ADD                 ADD NEW RECORD IN IO2                        
         GOTO1 VSAVPTRS,DMCB,(X'80',0),SCHPTRS                                  
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),SCHPTRS                                
*                                                                               
         MVC   AIO,AIO1            RESET IO AREA FOR GENERAL ROUTINES           
*                                                                               
         USING CATRECD,R2                                                       
O        USING CATRECD,R3                                                       
         LA    R2,KEY                                                           
         L     R3,AIO1                                                          
         MVC   CATKEY,O.CATKEY     START KEY FROM SCHEME RECORD                 
         MVI   CATKSUB,CATKSUBQ    CATEGORY SUB-RECORD                          
         MVC   CATKCODE,=X'0001'   WITH MINIMUM CATEGORY CODE                   
         MVC   SCATKEY,CATKEY      SAVE CATEGORY KEY                            
*                                                                               
COPY10   GOTO1 HIGH                READ HIGH FOR NEXT CATEGORY RECORD           
         CLC   O.CATRECD(CATKCODE-CATRECD),SCATKEY                              
         BNE   COPY50              EXIT AT END                                  
         MVC   SCATKEY,O.CATRECD   SAVE LATEST KEY                              
*                                                                               
         MVI   BYTE,0              CLEAR INDICATOR                              
         MVC   AIO,AIO3            READ FOR NEW CAT REC IN IO3                  
         LA    R2,KEY              READ FOR CATEGORY KEY TO COPY TO             
         XC    KEY,KEY                                                          
         MVC   CATKEY,O.CATKEY                                                  
         MVC   CATKSCH,SCHTCOD     NEW SCHEME CODE IN KEY                       
         GOTO1 HIGH                READ HIGH FOR NEXT CATEGORY RECORD           
         L     R2,AIO                                                           
         CLC   KEYSAVE(CATKCODE-CATRECD+L'CATKCODE),0(R2)                       
         BNE   COPY15                                                           
         CLC   CATKCODE,=X'FFFF'                                                
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT THE SLUCH ACCOUNT                 
         MVI   BYTE,X'80'          WRITE RECORD BACK - NOT ADD                  
*                                                                               
COPY15   L     R2,AIO2             BUILD COPY RECORD IN IO2                     
         MVC   CATKEY,O.CATKEY                                                  
         MVC   CATKSCH,SCHTCOD     NEW SCHEME CODE IN KEY                       
         LA    R3,ACCORFST(R3)                                                  
         LA    R4,ACCORFST(R2)                                                  
         SR    RE,RE                                                            
O        USING CADELD,R3                                                        
         USING CADELD,R4                                                        
COPY20   CLI   O.CADEL,0           NO MORE ELEMENTS I WANT                      
         BE    COPY40                                                           
         CLI   O.CADEL,CADELQ      COPY CATEGORY DESCRIPTION ELEMENT(S)         
         BE    *+12                                                             
         CLI   O.CADEL,CWKELQ      COPY CATEGORY WORKCODE ELEMENT(S)            
         BNE   COPY30                                                           
         IC    RE,O.CADLN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CADEL(0),O.CADEL                                                 
         LA    R4,1(RE,R4)                                                      
*                                                                               
COPY30   IC    RE,O.CADLN          NEXT EXISTING RECORD ELEMENT                 
         AR    R3,RE                                                            
         B     COPY20                                                           
*                                                                               
COPY40   MVI   0(R4),0             FINISH OFF RECORD                            
         LA    R4,1(R4)                                                         
         SR    R4,R2                                                            
         STCM  R4,3,CATRECD+ACCORLEN                                            
         MVC   AIO,AIO2            ADDRESS NEW RECORD AREA                      
         GOTO1 PERSIN              UPDATE PERSONAL ELEMENT                      
         L     RF,ADD              ADD NEW RECORD                               
         CLI   BYTE,X'80'                                                       
         BNE   *+8                                                              
         L     RF,WRITE            WRITE BACK EXISTING RECORD                   
         BASR  RE,RF                                                            
         GOTO1 VSAVPTRS,DMCB,(X'80',0),CATPTRS                                  
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),CATPTRS                                
*                                                                               
         MVC   AIO,AIO1            RESET IO AREA FOR GENERAL ROUTINES           
         SR    RF,RF                                                            
         ICM   RF,3,SCATKEY+(CATKCODE-CATRECD)                                  
         LA    RF,1(RF)                                                         
         CLM   RF,3,=AL2(0)        TEST ALL DONE                                
         BE    COPY50                                                           
         STCM  RF,3,SCATKEY+(CATKCODE-CATRECD)                                  
         LA    R2,KEY                                                           
         MVC   CATKEY,SCATKEY      SET NEXT KEY                                 
         L     R3,AIO1             R3=A(IO1)                                    
         B     COPY10                                                           
*                                                                               
COPY50   B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
ERREXIT  GOTO1 VERRCUR                                                          
*                                                                               
GETELIO  DS    0H                                                               
         GETEL (R3),DATADISP,ELCODE                                             
         DROP  R2,R4,O                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*  DDSPOOLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*  DDSPLWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACPROWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*  ACMSGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
*  ACDDEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROCFD                                                       
SCATKEY  DS    CL(L'CATKEY)                                                     
SCHPTRS  DS    XL(8*54+1)                                                       
CATPTRS  DS    XL(8*54+1)                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACPRO3F   04/24/07'                                      
         END                                                                    
