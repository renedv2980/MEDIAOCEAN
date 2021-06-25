*          DATA SET SPLFM27    AT LEVEL 021 AS OF 05/01/02                      
*PHASE T21927A,*                                                                
*INCLUDE PERVERT                                                                
*INCLUDE XSORT                                                                  
         TITLE 'SPLFM27 - CHILD SPOT FLIGHT MENU'                               
T21927   CSECT                                                                  
         NMOD1 0,T21927                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         SPACE 1                                                                
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING FLTREC,R8                                                        
         CLI   SVFMTSW,0           TEST IF FORMAT OR EDIT                       
         BE    FMT                                                              
         B     EDT                                                              
         SPACE 1                                                                
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
* FMT - THIS SECTION DISPLAYS THE RECORD ON THE SCREEN *                        
         SPACE 1                                                                
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
REFMT    DS    0H                                                               
         LA    R4,REC+24                                                        
         CLC   =X'0108',0(R4)                                                   
         BE    *+6                                                              
         DC    H'0'                MUST FIND 01 ELEMENT                         
         LA    R4,8(R4)                                                         
         CLI   0(R4),X'05'                                                      
         BE    *+6                 MUST ALSO FIND 05 ELEMENT                    
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    R2,FLTF1H           R2 POINTS TO SCREEN FIELD HEADER             
REFMT00  DS    0H                                                               
         XC    8(13,R2),8(R2)      CLEAR THE SCREEN FIELDS                      
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT R2 TO NEXT SCREEN FIELD HEADER         
         CLI   0(R2),9             IS THIS END OF SCREEN?                       
         BH    REFMT00              NO.                                         
         SPACE 1                                                                
REFMT10  DS    0H                                                               
         LA    R2,FLTF1H           R2 POINTS TO SCREEN FIELD HEADER             
         LA    R3,FLTF1            R3 POINTS TO SCREEN FIELD                    
         LA    R6,FLTEL05          R6 POINTS TO 1ST 05 ELEMENT                  
         USING FLTEL05,R6                                                       
         SPACE 1                                                                
REFMT20  DS    0H                                                               
         GOTO1 VDATCON,DMCB,(2,FLTST),(5,(R3))                                  
         MVI   8(R3),C'-'                                                       
         GOTO1 VDATCON,DMCB,(2,FLTST),DATE1                                     
         GOTO1 VDATCON,DMCB,(2,FLTEND),DATE2                                    
         GOTO1 =V(PERVERT),DMCB,DATE1,DATE2,RR=YES                              
         LH    R7,DMCB+12          GET NUMBER OF WEEKS IN FLIGHT                
         CVD   R7,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  9(2,R3),DUB                                                      
         MVI   11(R3),C'W'                                                      
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT R2 TO NEXT SCREEN FIELD HEADER         
         CLI   0(R2),9             IS THIS END OF SCREEN?                       
         BNH   EXXMOD               YES                                         
         LA    R3,8(R2)            POINT R3 TO NEXT SCREEN FIELD.               
         ZIC   R0,FLT05LEN                                                      
         AR    R6,R0                                                            
         CLI   0(R6),5             IS THERE ANOTHER 05 ELEMENT?                 
         BE    REFMT20              YES.                                        
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
* EDT - THIS SECTION EITHER ADDS A RECORD OR CHANGES THE 05 ELEMENT *           
         SPACE 1                                                                
EDT      DS    0H                                                               
         LA    R5,REC+24                                                        
         LA    R4,8(R5)            R4 POINTS TO 05 ELEMENT                      
         LA    R9,0                                                             
         CLI   SVACT,C'A'                                                       
         BE    EDT10               ADD A NEW RECORD                             
         MVC   KEY,SVKEY           CHANGE EXISTING RECORD                       
         GOTO1 GETREC                                                           
         SPACE 1                                                                
* UPDATE THE ACTIVITY DATE IN 01 ELEMENT *                                      
         SPACE 1                                                                
         GOTO1 VDATCON,DMCB,(5,WORK),(3,DUB)                                    
         MVC   5(3,R5),DUB         SET NEW ACTIVITY DATE - YMD                  
         SPACE 1                                                                
* DELETE THE OLD 05 ELEMENT *                                                   
         SPACE 1                                                                
         USING FLTEL05,R4                                                       
EDT05    DS    0H                                                               
         GOTO1 VRECUP,DMCB,(0,REC),0(R4),0                                      
         CLI   0(R4),5             IS THIS AN 05 ELEMENT?                       
         BE    EDT05                YES. DELETE IT                              
         B     EDT15                                                            
         DROP  R4                                                               
         SPACE 1                                                                
* BUILD AN 01 ELEMENT *                                                         
         SPACE 1                                                                
EDT10    DS    0H                                                               
         MVC   0(2,R5),=X'0108'                                                 
         GOTO1 VDATCON,DMCB,(5,WORK),(3,DUB)                                    
         MVC   2(3,R5),DUB         SET CREATION DATE - YMD                      
         MVC   5(3,R5),DUB         SET ACTIVITY DATE - YMD                      
         LA    RF,32                                                            
         STCM  RF,3,REC+13         SET THE RECORD LENGTH                        
         EJECT                                                                  
* BUILD AN 05 ELEMENT *                                                         
         SPACE 1                                                                
EDT15    DS    0H                                                               
         LA    R2,FLTF1H           R2 POINTS TO SCREEN FIELD HEADER             
         LA    R3,FLTF1            R3 POINTS TO SCREEN FIELD                    
         LA    R4,8(R5)            R4 POINTS TO 05 ELEMENT IN RECORD            
         LA    R6,ELEM                                                          
         USING FLTEL05,R6                                                       
         SR    R7,R7               R7 HAS DUMMY FLIGHT NUMBER                   
EDT20    DS    0H                                                               
         GOTO1 ANY                                                              
         BAS   RE,FLTVAL           VALIDATE 1ST FLIGHT                          
         XC    ELEM,ELEM                                                        
         MVI   FLTEL05,X'05'       BUILD AN 05 ELEMENT                          
         MVI   FLT05LEN,X'07'      ELEMENT 05 LENGTH = 7                        
         STC   R7,FLTNO            SET DUMMY FLIGHT NUMBER                      
         MVC   FLTST(4),STDATE     FLIGHT START & END DATES                     
         LA    R9,1(R9)                                                         
         SPACE 1                                                                
* ADD THE NEW 05 ELEMENT *                                                      
         SPACE 1                                                                
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R4)                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             IS THIS THE END OF THE SCREEN?               
         BE    SORT                 YES. SORT '05' ELEMENTS                     
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT R2 TO NEXT SCREEN FIELD HEADER         
         CLI   5(R2),0             IS THERE ANOTHER FLIGHT TO DO?               
         BE    EDT25                NO. CHECK NEXT FIELD                        
         LA    R3,8(R2)            POINT R3 TO NEXT SCREEN FIELD                
         ZIC   R0,FLT05LEN                                                      
         AR    R4,R0               R4 POINTS TO NEXT 05 ELEMENT                 
         B     EDT20                                                            
         EJECT                                                                  
EDT25    DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BE    SORT                                                             
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BE    EDT25                                                            
         LA    R3,8(R2)                                                         
         B     EDT20                                                            
         DROP  R6                                                               
         SPACE 1                                                                
* SORT THE FLIGHTS INTO CHRONOLOGICAL ORDER *                                   
         SPACE 1                                                                
SORT     DS    0H                                                               
         GOTO1 =V(XSORT),DMCB,(0,REC+32),(R9),7,4,3,RR=YES                      
         EJECT                                                                  
         LA    R4,8(R5)            R4 POINTS TO 1ST FLIGHT                      
         USING FLTEL05,R4                                                       
         XC    ENDDATE,ENDDATE                                                  
SORT10   DS    0H                                                               
         LA    R7,1(R7)                                                         
         STC   R7,FLTNO            SET FLIGHT NUMBER                            
         CLC   FLTST,ENDDATE       FLIGHTS MUST NOT OVERLAP                     
         BNH   ERROVR                                                           
         MVC   ENDDATE,FLTEND                                                   
         ZIC   R0,FLT05LEN                                                      
         AR    R4,R0                                                            
         CLI   FLTEL05,5                                                        
         BE    SORT10                                                           
         DROP  R4                                                               
         SPACE 1                                                                
* WRITE THE NEW RECORD *                                                        
         SPACE 1                                                                
         MVC   KEY,SVKEY                                                        
         CLI   SVACT,C'A'          IS THIS AN ADD?                              
         BNE   OUT10                NO.                                         
         MVC   REC(13),SVKEY                                                    
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         B     REFMT                                                            
         SPACE 1                                                                
OUT10    DS    0H                                                               
         GOTO1 PUTREC                                                           
         B     REFMT                                                            
         EJECT                                                                  
* FLTVAL - THIS ROUTINE VALIDATES THE FLIGHT START & END DATES *                
         SPACE 1                                                                
FLTVAL   NTR1                                                                   
         MVI   ERRCD,DATERR                                                     
         GOTO1 VDATVAL,DMCB,(R3),DATE1                                          
         OC    0(4,R1),0(R1)                                                    
         BZ    LFMERR              NOT VALID DATE                               
         A     R3,0(R1)            POINT R3 PAST THE DATE                       
         GOTO1 VGETDAY,DMCB,DATE1,MON                                           
         CLI   0(R1),1                                                          
         BNE   LFMERR              DOES NOT START ON A MONDAY                   
         CLI   0(R3),C'-'                                                       
         BNE   LFMERR                                                           
         CLI   2(R3),C'W'                                                       
         BNE   FLTV10              TEST IF 10 TO 53 WEEKS                       
         SPACE 1                                                                
         CLI   1(R3),C'1'          TEST IF 1 TO 9 WEEKS                         
         BL    LFMERR                                                           
         CLI   1(R3),C'9'                                                       
         BH    LFMERR                                                           
         PACK  DUB,1(1,R3)                                                      
         B     FLTV20                                                           
         SPACE 1                                                                
FLTV10   DS    0H                                                               
         CLI   3(R3),C'W'                                                       
         BNE   LFMERR                                                           
         CLC   =C'01',1(R3)                                                     
         BH    LFMERR                                                           
         CLC   =C'53',1(R3)                                                     
         BL    LFMERR                                                           
         PACK  DUB,1(2,R3)                                                      
         SPACE 1                                                                
FLTV20   DS    0H                                                               
         CVB   RF,DUB                                                           
         MH    RF,=H'7'                                                         
         BCTR  RF,0                                                             
         ST    RF,DMCB+8           SET NUMBER OF DAYS FOR ADDAY                 
         GOTO1 VADDAY,DMCB,DATE1,DATE2                                          
         GOTO1 VDATCON,DMCB,DATE1,(2,STDATE)                                    
         GOTO1 VDATCON,DMCB,DATE2,(2,ENDDATE)                                   
         XIT1                                                                   
         SPACE 1                                                                
* LFM ERROR ROUTINES *                                                          
         SPACE 1                                                                
LFMERR   GOTO1 ERROR                                                            
         SPACE 1                                                                
ERROVR   DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(34),=C'* ERROR - FLIGHT PERIODS OVERLAP *'                
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXXMOD                                                           
         EJECT                                                                  
MON      DC    CL3'MON'                                                         
         SPACE 1                                                                
         LTORG                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
* SPLFME7 - CHILD SPOT FLIGHT MENU                                              
       ++INCLUDE SPLFME7D                                                       
         SPACE 2                                                                
GENOLD   DSECT                                                                  
         ORG   REC2                                                             
         DS    0H                                                               
STDATE   DS    H                                                                
ENDDATE  DS    H                                                                
DATE1    DS    CL6                                                              
DATE2    DS    CL6                                                              
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE SPGENFLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPLFM27   05/01/02'                                      
         END                                                                    
