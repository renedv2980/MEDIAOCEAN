*          DATA SET TAREP07    AT LEVEL 009 AS OF 12/10/12                      
*PHASE T70307A,*                                                                
         TITLE 'T70307 - TALENT PAYROLL REGISTER - VALIDATE SCREEN'             
T70307   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70307                                                         
         L     RC,0(R1)            RC = A(GENCON WORKING STORAGE)               
         USING GEND,RC                                                          
         L     R8,ASPOOLD          R8 = A(SPOOL DSECT)                          
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD         R9 = A(CONTROLLER WORKING STORAGE)           
         USING SUBSYSD,R9                                                       
         LA    R7,BUFF             R7 = A(CHECK'S WORKING STORAGE)              
         LA    R7,8(R7)                                                         
         USING PAYROLLD,R7                                                      
         L     RA,ATWA             RA = A(TWA)                                  
         USING T703FFD,RA                                                       
         EJECT                                                                  
* MAIN ROUTINE                                                                  
*                                                                               
MAIN     DS    0H                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE SYSTEM WORKING STORAGE            
*                                                                               
         CLI   MODE,DISPREC        RECOGNIZE ONLY DISPLAY RECORD                
         BE    *+12                                                             
         CLI   MODE,VALREC         AND VALIDATE RECORD                          
         BNE   MX                                                               
*                                                                               
         LR    RE,R7               PRE-CLEAR CHECK PROGRAM WORKING STR          
         L     RF,=A(PAYROLLL)                                                  
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,SPREMPH          VALIDATE EMPLOYER (OPTIONAL)                 
         CLI   5(R2),0                                                          
         BE    M05                                                              
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),SPREMPNH                        
         MVC   REQEMP,TGEMP                                                     
*                                                                               
M05      LA    R2,SPRAGYH          VALIDATE AGENCY (OPTIONAL)                   
         CLI   5(R2),0                                                          
         BE    M10                                                              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SPRAGYNH                        
         MVC   REQAGY,TGAGY                                                     
*                                                                               
M10      LA    R2,SPRPERH          VALIDATE PERIOD                              
*                                                                               
         LA    R3,BLOCK            CALL PDVAL TO VALDATE PERIOD                 
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   REQSTART,PVALPSTA                                                
         MVC   REQEND,PVALPEND                                                  
*                                                                               
         LA    R2,SPRRUNH          VALIDATE RUN DATE PERIOD (OPTIONAL)          
*                                                                               
*                                  CALL PDVAL TO VALDATE PERIOD                 
         GOTO1 PDVAL,DMCB,(X'80',(R3))                                          
         MVC   REQRUNS,PVALPSTA                                                 
         MVC   REQRUNE,PVALPEND                                                 
         DROP  R3                                                               
*                                                                               
         BAS   RE,VALIOPT          VALIDATE OPTIONS FIELD                       
*                                                                               
MX       B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE OPTIONS FIELD                                      
*                                                                               
VALIOPT  NTR1                                                                   
         MVI   REQSORT,C'N'        DEFAULT SORT INDICATOR TO NAME               
         MVI   REQADJ,C'N'         DEFAULT ADJUSTMENT INDICATOR TO NO           
         MVI   REQTOA,C'N'         DEFAULT TO LOAD TAREPTOT                     
*                                                                               
         LA    R2,SPROPTH          IF NO OPTIONS THEN DONE                      
         CLI   5(R2),0                                                          
         BE    VO100                                                            
*                                                                               
         LA    R5,BLOCK            R5 = A(SCANNER BLOCK)                        
         USING SCAND,R5                                                         
*                                  SCAN FIELD INTO SCANNER BLOCK                
         GOTO1 SCANNER,DMCB,(R2),(R5),0                                         
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R6,4(R1)            R6 = # OF SCANNER BLOCK ENTRIES              
         SR    R3,R3               R3 = SCR FLD DISP TO CURRENT ENTRY           
*                                                                               
*                                  IF SORT BY INVOICE OPTION                    
VO10     CLC   SCDATA1(8),=C'INVOICE '                                          
         BNE   VO20                                                             
         MVI   REQSORT,C'I'        THEN SET SORT INDICATOR TO INVOICE           
         B     VO90                                                             
*                                  IF PRINT ONLY ADJUSTMENTS OPTION             
VO20     CLC   SCDATA1(10),=C'ADJUSTMENT'                                       
         BNE   VO25                                                             
         MVI   REQADJ,C'Y'         THEN SET ADJUSTMENT INDICATOR                
         B     VO90                                                             
*                                                                               
VO25     CLC   SCDATA1(8),=C'TAREPTOA'                                          
         BNE   FLDERR                                                           
         MVI   REQTOA,C'Y'         LOAD TAREPTOA INSTEAD OF TAREPTOT            
*                                                                               
VO90     ZIC   RF,SCLEN1           BUMP R3 TO NEXT ENTRY                        
         LA    R3,1(R3,RF)                                                      
         ZIC   RF,SCLEN2                                                        
         LA    R3,1(R3,RF)                                                      
*                                                                               
         LA    R5,SCANNEXT         BUMP R5 TO NEXT ENTRY                        
         BCT   R6,VO10             REPEAT UNTIL NO MORE ENTRIES                 
*                                                                               
*                                  IF TRACE FIELD STARTS WITH 'TRACE='          
VO100    CLC   SPRTRAC(6),=C'TRACE='                                            
         BNE   VO110                                                            
         MVC   REQTRACE,SPRTRAC+6  THEN SAVE TRACE BYTES                        
         B     VOX                                                              
*                                                                               
VO110    XC    SPRTRAC,SPRTRAC     ELSE CLEAR OUT FIELD                         
         OI    SPRTRACH+6,X'80'                                                 
*                                                                               
VOX      B     XIT                                                              
         EJECT                                                                  
FLDERR   L     RE,SYSPARMS         INVALID INPUT WITHIN FIELD                   
         ICM   RE,15,0(RE)                                                      
         BZ    INVERR                                                           
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,15,TIOBCURD                                                   
         STC   R3,TIOBCURI                                                      
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF7D                                                       
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*TAREPWORKD                                                                     
*TAREPPYRD                                                                      
*TASYSDSECT                                                                     
*FATIOB                                                                         
*DDPERVAD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE TAREPPYRD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009TAREP07   12/10/12'                                      
         END                                                                    
