*          DATA SET SPLFM1E    AT LEVEL 019 AS OF 05/01/02                      
*PHASE T2191EA                                                                  
         TITLE 'SPLFM1E - EXTENDED DEMO PROGRAM'                                
         PRINT NOGEN                                                            
T2191E   CSECT                                                                  
         NMOD1 0,T2191E                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING USRDREC,R8                                                       
*                                                                               
         OC    DEMF1,SPACES        ELIMINATE NULLS IN INPUT FIELDS              
         OC    DEMF2,SPACES                                                     
         OC    DEMF3,SPACES                                                     
*                                                                               
         CLI   SVFMTSW,0           TEST IF FORMAT OR EDIT                       
         BE    FMT                                                              
         B     EDT                                                              
         SPACE 2                                                                
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
* FMT - THIS SECTION DISPLAYS THE RECORD ON THE SCREEN *                        
         SPACE 1                                                                
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         CLC   DEMF1,SPACES                                                     
         BNE   EDTINV5             CANNOT HAVE A DEMO                           
         CLC   DEMF2,SPACES                                                     
         BNE   EDTINV6             CANNOT HAVE A NUMBER                         
*                                                                               
REFMT    DS    0H                                                               
         STM   2,5,WORK2           ROUTINE TO CLEAR REC2                        
         LA    2,REC2                                                           
         LA    3,2000                                                           
         LA    4,*                                                              
         XR    5,5                 TO CLEAR TO ZEROS (FILL CHAR=0)              
         MVCL  2,4                                                              
         LM    2,5,WORK2                                                        
*                                                                               
         LA    R5,REC2             WORK AREA TO BUILD TABLE                     
         LA    R6,USRDEMEL                                                      
         USING USRDEMEL,R6                                                      
         XR    R7,R7                                                            
         SPACE 1                                                                
*  BUILD  ELEMENT  TABLE  IN  REC2  AND  COUNT  ELEMENTS  *                     
         SPACE 1                                                                
REFMT1   DS    0H                                                               
         CLI   0(R6),0             LAST ELEMENT                                 
         BE    REFMT2                                                           
         LA    R7,1(R7)            ELEMENT CNTR                                 
         STC   R7,0(R5)                                                         
         MVC   1(7,R5),2(R6)       MOVE DEMO DESC'S INTO TABLE                  
         LA    R5,8(R5)            POINT TO NEXT AVAILABLE SLOT IN TBL          
         ZIC   R0,1(R6)            ELEMENT LENGTH                               
         AR    R6,R0               POINT TO NEXT ELEMENT                        
         B     REFMT1                                                           
         SPACE 1                                                                
REFMT2   DS    0H                                                               
         LA    R5,REC2                                                          
         GOTO1 VCALLOV,DMCB,0,X'D900A12'  GET ADDR OF XSORT                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R5),(R7),8,7,1   SORT RECORDS                         
         CLI   DEMF3H+5,0          IS A START POINT DESIGNATED                  
         BE    REFMT4              NO. DISPLAY FROM BEGINNING                   
         LA    R2,DEM4H                                                         
*                                                                               
REFMT3   DS    0H                                                               
         CLC   DEMF3,1(R5)         FIND STARTING POINT FOR DISPLAY              
         BNH   REFMT00                                                          
         LA    R5,8(R5)                                                         
         CLI   0(R5),0             END OF TABLE                                 
         BNE   REFMT3                                                           
         MVC   LFMMSG(18),=C'*** DEMO NOT FOUND'                                
         MVI   ERRAREA,X'FF'       TELL BASE THAT MSG PRESENT                   
         OI    DEMF3H+6,X'40'                                                   
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
*                                                                               
REFMT4   LA    R2,DEM4H            POINT TO 1ST OUTPUT FLD HDR                  
         LA    R5,REC2                                                          
*                                                                               
REFMT00  DS    0H                  'SOFT' ROUTINE TO CLEAR SCREEN               
         ZIC   RE,0(R2)            FIELD LENGTH (HDR+MAX DATA)                  
         S     RE,=F'9'            8 (HDR) + 1                                  
         EX    RE,CLCIT                                                         
         BE    REFMT10                                                          
         EX    RE,OCIT                                                          
         BZ    REFMT10                                                          
         EX    RE,XCIT                                                          
         FOUT  (R2)                                                             
         B     REFMT10                                                          
*                                                                               
CLCIT    CLC   8(0,R2),SPACES                                                   
OCIT     OC    8(0,R2),8(R2)                                                    
XCIT     XC    8(0,R2),8(R2)                                                    
*                                                                               
REFMT10  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT R2 TO NEXT SCREEN FIELD HEADER         
         CLI   0(R2),9             IS THIS THE END OF THE SCREEN?               
         BH    REFMT00              NO.                                         
*                                                                               
         XR    R9,R9                                                            
         LA    R2,DEM4H                                                         
REFMT15  DS    0H                                                               
         LA    R9,8(R2)            R9 POINTS TO OUTPUT FIELD                    
         LA    R4,6                FOR 6 RTG GROUPS ON A LINE                   
REFMT20  FOUT  (R2)                                                             
         ZIC   R3,0(R5)                                                         
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R9),DUB                                                      
         MVC   4(7,R9),1(R5)                                                    
         LA    R9,13(R9)           POINT TO NEXT FIELD ON OUTPUT LINE           
         LA    R5,8(R5)            POINT TO NEXT TABLE ENTRY                    
         CLI   0(R5),0             IS THERE ANOTHER 05 ELEMENT?                 
         BE    EXXMOD              NO.                                          
         BCT   R4,REFMT20                                                       
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT R9 TO NEXT SCREEN FIELD HEADER         
         CLI   0(R2),9             IS THIS THE END OF THE SCREEN?               
         BH    REFMT15             NO.                                          
         CLC   LFMACT(3),=C'DIS'                                                
         BE    REFMT25                                                          
         CLI   SVFMTSW,0                                                        
         BE    EXXMOD                                                           
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(27),=C'*** RECORD HAS BEEN CHANGED'                       
         MVI   ERRAREA,X'FF'                                                    
         OI    DEMF1H+6,X'40'      POSITION CURSOR TO DEMO NAME FLD             
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
*                                                                               
REFMT25  DS    0H                  MORE DEMOS BUT NO MORE ROOM ON SCR           
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(40),=C'*** MORE DEMOS TO DISPLAY STARTING WITH '          
         MVC   LFMMSG+40(7),1(R5)                                               
         MVI   ERRAREA,X'FF'       TELL BASE THAT MSG PRESENT                   
         OI    DEMF3H+6,X'40'      POSITION CURSOR TO DISPLAY START FLD         
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
* EDT - THIS SECTION EITHER ADDS OR CHANGES A RECORD *                          
         SPACE 1                                                                
EDT      DS    0H                                                               
         MVI   ELCODE,5                                                         
         CLI   SVACT,C'A'                                                       
         BE    EDT20               ADD A NEW RECORD                             
EDT01    DS    0H                                                               
         MVC   KEY,SVKEY           CHANGE RECORD                                
         GOTO1 GETREC                                                           
*                                                                               
         CLC   DEMF1,SPACES        IS THERE A DEMO                              
         BE    EDTINV3             NO. INVALID. MUST HAVE A DEMO                
         SPACE 1                                                                
* UPDATE THE ACTIVITY DATE IN THE 01 ELEMENT *                                  
         SPACE 1                                                                
         BAS   RE,TODAY            GET TODAY'S DATE                             
         LA    R5,REC+24           POINT TO 01 ELEMENT                          
         MVC   5(3,R5),DUB         SET THE ACTIVITY DATE - YMD                  
         ZIC   R0,1(R5)                                                         
         AR    R5,R0               POINT TO FIRST 05 ELEMENT                    
EDT02    DS    0H                                                               
         BAS   RE,NEXTEL           THIS LOOP CHECKS THAT A DEMO TO BE           
         BNE   EDT03                ADDED OR CHANGED DOES NOT ALREADY           
         CLC   2(7,R5),DEMF1        EXIST                                       
         BE    EDTINV1                                                          
         ZIC   R0,1(R5)                                                         
         AR    R5,R0               POINT TO NEXT ELEMENT                        
         B     EDT02                                                            
*                                                                               
EDT03    DS    0H                  YES.                                         
         CLC   DEMF2,SPACES        IS THERE A DEMO NUMBER FOR CHANGE            
         BE    EDT30               NO. THEN JUST ADD DEMO                       
         LA    R2,DEMF2H           GET NUMBER OF DEMO TO CHANGE                 
         GOTO1 PACK                                                             
         LR    R3,R0                                                            
         LA    R7,1                                                             
         LA    R5,REC+24                                                        
         ZIC   R0,1(R5)                                                         
         AR    R5,R0               R5 POINTS TO FIRST 05 ELEMENT                
         MVI   ELCODE,5                                                         
EDT05    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   EDTINV2             NO MORE ELEMENTS                             
         CR    R7,R3               ELEMENT TO BE CHANGED                        
         BE    EDT10                                                            
         ZIC   R0,1(R5)                                                         
         AR    R5,R0               POINT TO NEXT ELEMENT                        
         LA    R7,1(R7)                                                         
         B     EDT05                                                            
         SPACE 1                                                                
*  ROUTINE TO FIND AN ELEMENT  *                                                
         SPACE 1                                                                
NEXTEL   DS    0H                                                               
         CLI   0(R5),0             CHECK IF LAST ELEMENT                        
         BE    NEXTELX                                                          
         XR    R0,R0                                                            
         IC    R0,1(R5)            ELEMENT LENGTH                               
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                MUST FIND A LENGTH                           
         CLC   0(1,R5),ELCODE                                                   
         BNE   NEXTEL                                                           
         BR    RE                  FOUND. EXIT WITH CC EQUAL                    
NEXTELX  LTR   RE,RE               NOT FOUND. SET CC NOT EQUAL                  
         BR    RE                  EXIT WITH CC NOT EQUAL                       
         SPACE 1                                                                
*  UNACCEPTABLE   INPUT   MESSAGES  *                                           
         SPACE 1                                                                
EDTINV1  DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(23),=C'*** DEMO ALREADY EXISTS'                           
         MVI   ERRAREA,X'FF'       TELL BASE THAT MSG PRESENT                   
         OI    DEMF1H+6,X'40'      POSITION CURSOR TO INVALID FIELD             
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
*                                                                               
EDTINV2  DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(23),=C'*** INVALID DEMO NUMBER'                           
         MVI   ERRAREA,X'FF'       TELL BASE THAT MSG PRESENT                   
         OI    DEMF2H+6,X'40'      POSITION CURSOR TO INVALID FIELD             
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
*                                                                               
EDTINV3  DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(14),=C'*** ENTER DEMO'                                    
         MVI   ERRAREA,X'FF'                                                    
         OI    DEMF1H+6,X'40'                                                   
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
*                                                                               
EDTINV4  DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(37),=C'*** NUMBER INVALID WHEN ADDING RECORD'             
         MVI   ERRAREA,X'FF'                                                    
         OI    DEMF2H+6,X'40'                                                   
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
*                                                                               
EDTINV5  DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(44),=C'*** CANNOT ENTER DEMO WHEN ACTION IS DISPL*        
               AY'                                                              
         MVI   ERRAREA,X'FF'       TELL BASE THAT MSG PRESENT                   
         OI    DEMF1H+6,X'40'      POSITION CURSOR                              
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
*                                                                               
EDTINV6  DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(46),=C'*** CANNOT ENTER NUMBER WHEN ACTION IS DIS*        
               PLAY'                                                            
         MVI   ERRAREA,X'FF'       TELL BASE THAT MSG PRESENT                   
         OI    DEMF2H+6,X'40'      POSITION CURSOR                              
         FOUT  LFMMSGH                                                          
         B     EXXMOD                                                           
         SPACE 1                                                                
*  CHANGE 05 ELEMENT  *                                                         
         SPACE 1                                                                
EDT10    DS    0H                                                               
         MVC   2(7,R5),DEMF1                                                    
         B     OUTPUT                                                           
         SPACE 1                                                                
* BUILD AN 01 ELEMENT *                                                         
         SPACE 1                                                                
EDT20    DS    0H                                                               
         CLC   DEMF1,SPACES        IS THERE A DEMO                              
         BE    EDTINV3             MUST HAVE DEMO TO ADD                        
         CLC   DEMF2,SPACES        IS THERE A NUMBER                            
         BNE   EDTINV4             MUST NOT HAVE A NUMBER                       
         LA    R5,REC+24           POINT TO 01 ELEMENT                          
         MVC   0(2,R5),=X'010C'                                                 
         BAS   RE,TODAY            GET TODAY'S DATE                             
         MVC   2(3,R5),DUB         SET THE CREATION DATE - YMD                  
         MVC   5(3,R5),DUB         SET THE ACTIVITY DATE - YMD                  
         LA    RF,36                                                            
         STCM  RF,3,REC+13         SET THE RECORD LENGTH                        
         ZIC   R0,1(R5)                                                         
         AR    R5,R0               POINT TO FIRST 05 ELEMENT                    
         SPACE 1                                                                
* BUILD AN 05 ELEMENT *                                                         
         SPACE 1                                                                
EDT30    DS    0H                                                               
         LA    R6,ELEM                                                          
         USING USRDEMEL,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   USRDEMEL,X'05'      BUILD AN 05 ELEMENT                          
         MVI   USRDEMEL+1,X'09'    ELEMENT 05 LENGTH = 09                       
         MVC   USRDEMS,DEMF1       DEMO DESCRIPTION                             
         OC    USRDEMS,SPACES      TO ELIMINATE NULLS                           
EDT40    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    EDT50               MUST BE POINTING TO INSERTION                
         DC    H'0'                 LOCATION                                    
         SPACE 1                                                                
* ADD THE NEW 05 ELEMENT *                                                      
         SPACE 1                                                                
EDT50    GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R5)                                   
         SPACE 2                                                                
* WRITE THE NEW RECORD *                                                        
         SPACE 1                                                                
OUTPUT   MVC   KEY,SVKEY                                                        
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         CLI   SVACT,C'A'          IS THIS AN ADD?                              
         BNE   OUT10                NO.                                         
         MVC   REC(13),SVKEY                                                    
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         B     REFMT                                                            
         SPACE 1                                                                
OUT10    DC    0H'0'                                                            
         GOTO1 PUTREC                                                           
         B     REFMT                                                            
         SPACE 2                                                                
* TODAY - THIS ROUTINE GETS TODAY'S DATE *                                      
         SPACE 1                                                                
TODAY    NTR1                                                                   
         MVC   DUB,SPACES                                                       
         GOTO1 VDATCON,DMCB,(5,0),(3,DUB)                                       
EXIT     XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 2                                                                
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
* SPLFMFE - SCREEN FOR SPLFM1E                                                  
       ++INCLUDE SPLFMFED                                                       
         EJECT                                                                  
USRDRECD DSECT                                                                  
       ++INCLUDE SPGENUSRD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPLFM1E   05/01/02'                                      
         END                                                                    
