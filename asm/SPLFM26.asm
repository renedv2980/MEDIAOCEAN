*          DATA SET SPLFM26    AT LEVEL 043 AS OF 05/01/02                      
*PHASE T21926A,*                                                                
*===============================================================*               
* THIS PROGRAM IS LIVE AT LEVEL 41 AS OF 08/18/83               *               
* SOMETIME IN 1987 THE E6 SCREEN WAS CHANGED TO SUPPORT 52      *               
* DEMOS, BUT IT REMAINED AS AN 'A' SCRGEN UNTIL 1997, WHEN I    *               
* CHANGED IT BACK TO THE 20 DEMOS IT WAS ACTUALLY SUPPORTING    *               
* MHER  24SEP97                                                 *               
*===============================================================*               
         TITLE 'SPLFM26 - DEMO MENU'                                            
         PRINT NOGEN                                                            
T21926   CSECT                                                                  
         NMOD1 0,T21926                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING DMENUREC,R8                                                      
         CLI   SVFMTSW,0           TEST IF FORMAT OR EDIT                       
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
* FMT - THIS SECTION DISPLAYS THE RECORD ON THE SCREEN *                        
         SPACE 1                                                                
FMT      DC    0H'0'                                                            
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
REFMT    LA    R4,REC+24                                                        
         CLC   =X'0105',0(R4)                                                   
         BE    *+6                                                              
         DC    H'0'                MUST FIND 01 ELEMENT                         
         LA    R4,5(R4)                                                         
         CLI   0(R4),X'05'                                                      
         BE    *+6                                                              
         DC    H'0'                MUST ALSO FIND AN 05 ELEMENT                 
         SPACE 1                                                                
         LA    R2,DEMF1H                                                        
REFMT00  CLC   8(10,R2),SPACES                                                  
         BE    REFMT05                                                          
         CLC   8(4,R2),=C'PF2='    DON'T CLEAR PF KEY                           
         BE    REFMT05                                                          
         OC    8(10,R2),8(R2)                                                   
         BZ    REFMT05                                                          
         XC    8(10,R2),8(R2)                                                   
         FOUT  (R2)                                                             
REFMT05  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT R2 TO NEXT SCREEN FIELD HEADER         
         CLI   0(R2),9             IS THIS THE END OF THE SCREEN?               
         BH    REFMT00              NO.                                         
*                                                                               
REFMT10  DC    0H'0'                                                            
         LA    R2,DEMF1H           R2 POINTS TO 1ST SCREEN FIELD HEADER         
         LA    R6,DMNEL05          R6 POINTS TO 1ST 05 ELEMENT                  
         USING DMNEL05,R6                                                       
REFMT20  DC    0H'0'                                                            
         MVC   8(7,R2),DMNRTG      PUT OUT DEMO NAME 7 CHARS                    
         CLC   DMNRTN(2),=X'0021'  CHK FOR USER DEMO                            
         BNE   REFMT23                                                          
         MVC   8(3,R2),=C'UN/'                                                  
         ZIC   R0,DMNRTN+2         USER DEMO NUMBER                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  9(1,R2),DUB                                                      
         MVC   11(7,R2),DMNRTG     USER DEMO NAME                               
REFMT23  FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT R2 TO NEXT SCREEN FIELD HEADER         
         CLI   0(R2),9             IS THIS THE END OF THE SCREEN?               
         BNH   EXXMOD               YES.                                        
         ZIC   R0,DMN05LEN                                                      
         AR    R6,R0                                                            
         CLI   0(R6),5             IS THERE ANOTHER 05 ELEMENT?                 
         BE    REFMT20              YES.                                        
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
* EDT - THIS SECTION EITHER ADDS A RECORD OR CHANGES THE 01 ELEMENT *           
         SPACE 1                                                                
EDT      DC    0H'0'                                                            
         LA    R5,REC+24                                                        
         LA    R4,5(R5)            R4 POINTS TO 1ST 05 ELEMENT                  
         CLI   SVACT,C'A'                                                       
         BE    EDT10               ADD A NEW RECORD                             
         MVC   KEY,SVKEY           CHANGE RECORD                                
         GOTO1 GETREC                                                           
         SPACE 1                                                                
* UPDATE THE ACTIVITY DATE IN THE 01 ELEMENT *                                  
         SPACE 1                                                                
         GOTO1 VDATCON,DMCB,(5,WORK),(3,DUB)                                    
         MVC   2(3,R5),DUB         SET THE ACTIVITY DATE - YMD                  
         SPACE 1                                                                
* DELETE THE OLD '05' ELEMENTS *                                                
         SPACE 1                                                                
EDT05    DC    0H'0'                                                            
         GOTO1 VRECUP,DMCB,(0,REC),0(R4),0                                      
         CLI   0(R4),5             IS THIS AN 05 ELEMENT?                       
         BE    EDT05                YES. DELETE IT                              
         B     EDT15                                                            
         SPACE 1                                                                
* BUILD AN 01 ELEMENT *                                                         
         SPACE 1                                                                
EDT10    DC    0H'0'                                                            
         MVC   0(2,R5),=X'0105'                                                 
         GOTO1 VDATCON,DMCB,(5,WORK),(3,DUB)                                    
         MVC   2(3,R5),DUB         SET THE ACTIVITY DATE - YMD                  
         LA    RF,29                                                            
         STCM  RF,3,REC+13         SET THE RECORD LENGTH                        
         SPACE 1                                                                
* BUILD AN 05 ELEMENT *                                                         
         SPACE 1                                                                
EDT15    DC    0H'0'                                                            
         LA    R2,DEMF1H           R2 POINTS TO FIELD HEADER                    
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING DMNEL05,R6                                                       
EDT20    DC    0H'0'                                                            
         CLI   5(R2),0             CHK FOR INPUT                                
         BE    EDT30                                                            
         BAS   RE,EDTVAL           VALIDATE THE RATING GROUP                    
         XC    ELEM,ELEM                                                        
         MVI   DMNEL05,X'05'       BUILD AN 05 ELEMENT                          
         MVI   DMN05LEN,X'0C'      ELEMENT 05 LENGTH = 12                       
         MVC   DMNRTN,WORK2        DEMO CODE                                    
         MVC   DMNRTG,WORK2+6      7 CHAR NAME                                  
*                                                                               
*                                  CHK FOR DUPLICATE ENTRY                      
         LA    R5,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
EDT22    BAS   RE,NEXTEL                                                        
         BNE   EDT25                                                            
         CLC   2(3,R5),DMNRTN                                                   
         BNE   EDT22                                                            
         MVI   ERRCD,DUPENTRY                                                   
         B     LFMERR                                                           
*                                                                               
         SPACE 1                                                                
* ADD THE NEW 05 ELEMENT *                                                      
         SPACE 1                                                                
EDT25    GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R4)                                   
EDT30    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             IS THIS THE END OF THE SCREEN?               
         BE    OUTPUT               YES. WRITE NEW RECORD                       
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT R2 TO NEXT SCREEN FIELD HEADER         
         CLI   5(R2),0             IS THERE ANOTHER MENU TO DO?                 
         BE    EDT30               NO - SKIP TO NEXT DEMO                       
         ZIC   R0,DMN05LEN                                                      
         AR    R4,R0                                                            
         B     EDT20                                                            
         SPACE 1                                                                
* WRITE THE NEW RECORD *                                                        
         SPACE 1                                                                
OUTPUT   LA    R2,DEMF1H                                                        
         OC    DMNRTN,DMNRTN       SEE IF I DID AT LEAST ONE DEMO               
         BNZ   OUTP5                                                            
         MVI   ERRCD,MSSNGERR                                                   
         B     LFMERR                                                           
         DROP  R6                                                               
*                                                                               
OUTP5    MVC   KEY,SVKEY                                                        
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         CLI   SVACT,C'A'          IS THIS AN ADD?                              
         BNE   OUT10                NO.                                         
         MVC   REC(13),SVKEY                                                    
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         GOTO1 CNADDSPT                                                         
         B     REFMT                                                            
         SPACE 1                                                                
OUT10    DC    0H'0'                                                            
         GOTO1 PUTREC                                                           
         GOTO1 CNCHASPT                                                         
         B     REFMT                                                            
         EJECT                                                                  
* EDTVAL - THIS ROUTINE VALIDATES EACH DEMO                                     
         SPACE 1                                                                
EDTVAL   NTR1                                                                   
         XC    WORK2(20),WORK2                                                  
         CLI   8(R2),C'U'          CHK FOR USER DEMOS                           
         BNE   EDTV2                                                            
         CLI   9(R2),C'1'                                                       
         BL    EDTV2                                                            
         CLI   9(R2),C'4'                                                       
         BH    EDTV2                                                            
         CLI   10(R2),C'/'                                                      
         BNE   EDTV2                                                            
         CLI   5(R2),4                                                          
         BL    EDTVERR                                                          
         MVC   WORK2(3),=X'002100'                                              
         PACK  DUB,9(1,R2)                                                      
         CVB   R0,DUB                                                           
         STC   R0,WORK2+2                                                       
         ZIC   R9,5(R2)            INPUT LENGHT                                 
         SH    R9,=H'4'            SUBTRACT FOR UN/                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+6(0),11(R2)   MOVE USER DEMO NAME                          
         OC    WORK2+6(7),SPACES                                                
         B     EDTVX                                                            
*                                                                               
EDTV2    CLI   5(R2),7             INPUT TOO LONG                               
         BH    EDTVERR                                                          
         LA    R9,REC2                                                          
         USING DBLOCK,R9                                                        
         XC    DBLOCK,DBLOCK                                                    
         L     R7,VCOMFACS                                                      
         USING COMFACSD,R7                                                      
         ST    R7,DBCOMFCS                                                      
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'T'                                                    
EDTV3    GOTO1 CDEMOVAL,DMCB,(R2),(1,WORK2),(C'S',DBLOCK),0                     
         CLI   4(R1),0                                                          
         BNE   EDTV5                                                            
EDTVERR  MVI   ERRCD,DEMINV        INVALID DEMO                                 
         B     LFMERR                                                           
*                                                                               
EDTV5    MVC   DBFILE,=C'TP '                                                   
         MVC   DMCB+4(4),=X'D9000AE0' DEMOCON                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,WORK2),(2,WORK2+6),(C'S',DBLOCK),0                  
EDTVX    XIT1                                                                   
         DROP  R7                                                               
         DROP  R9                                                               
         SPACE 1                                                                
* LFM ERROR ROUTINE *                                                           
         SPACE 1                                                                
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
*                                                                               
NEXTEL   ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BER   RE                                                               
         CLI   0(R5),0             END OF REC                                   
         BNE   NEXTEL                                                           
*                                                                               
         LTR   R5,R5               SET CONDITION CODE NE                        
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
* SPLFME6 - DEMO MENU DSECT                                                     
       ++INCLUDE SPLFME6D                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDMN                                                       
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPLFM26   05/01/02'                                      
         END                                                                    
