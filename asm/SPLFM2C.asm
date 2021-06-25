*          DATA SET SPLFM2C    AT LEVEL 070 AS OF 05/01/02                      
*PHASE T2192CA                                                                  
         TITLE 'T2192C - DEMO OVERRIDE VALUE SCREEN'                            
T2192C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRK2CX-WRK2C,T2192C,RR=R9,CLEAR=YES                              
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         LR    RE,RC                                                            
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    RE,ELEM             POETIC JUSTICE                               
*                                                                               
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING DOVRECD,R8                                                       
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         BAS   RE,BLDTAB           ALWAYS BUILD TABLE OF STATIONS               
*                                  AND SPILL MKTS                               
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         LA    R7,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,NEXTEL                                                        
         BE    REBUILD                                                          
         BAS   RE,ADDELS      ADD ELEM TO RECORD                                
         B     WRITEBK             WRITE BACK UPDATED REC                       
         EJECT                                                                  
REBUILD  DS    0H                  REREAD OLD REC INTO REC2                     
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
*                                                                               
REB5     LA    R7,REC+24                                                        
         MVI   ELCODE,X'05'        DELETE OLD 05 ELEMS FROM REC                 
         BAS   RE,NEXTEL                                                        
         BNE   REB10                                                            
         GOTO1 VRECUP,DMCB,(0,REC),0(R7),0                                      
         B     REB5                                                             
*                                                                               
REB10    DS    0H                                                               
         BAS   RE,CLEAREC          ZERO END OF REC                              
         BAS   RE,ADDELS           ADD NEW 05 ELEMS                             
*              FOR EACH NEW 05 ELEM SEARCH OLD REC FOR VALUES TO SAVE           
         LA    R2,REC2+24                                                       
         LA    R7,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
REB15    BAS   RE,NEXTEL                                                        
         BNE   REBX                END OF NEW REC                               
*                                                                               
         CLI   2(R7),0             TEST SPILL OVERRIDE                          
         BE    REB18               YES                                          
* STATION - FIND ELEM IN OLD RECORD                                             
         XC    FULL,FULL                                                        
         LA    R2,REC2+24          START OF OLD RECORD                          
         BAS   RE,NEXTEL2                                                       
         BNE   REB15               NOT FOUND                                    
         CLC   2(3,R7),2(R2)       RIGHT STATION                                
         BNE   *-14                NO                                           
         ST    R2,FULL             SAVE STATION ELEM ADDRESS                    
         B     REB25               AND MOVE VALUES                              
*                                                                               
REB18    ICM   R2,15,FULL          GET STATION ELEM ADDRESS                     
         BZ    REB15               SKIP IF NONE                                 
*                                                                               
REB20    BAS   RE,NEXTEL2                                                       
         BNE   REB15                                                            
         CLI   2(R2),0             TEST NEW STATION                             
         BNE   REB15               YES - SPILL MKT NOT FOUND                    
         CLC   2(3,R7),2(R2)       TEST RIGHT MARKET                            
         BNE   REB20                                                            
*                                                                               
REB25    ZIC   R5,1(R7)                                                         
         SH    R5,=H'6'                                                         
         BM    REB15               NO VALUES TO STORE                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R7),5(R2)       MOVE OLD VALUES                              
         B     REB15                                                            
*                                                                               
REBX     DS    0H                  SEE IF RECORD WAS CHANGED                    
         BAS   RE,CLEAREC                                                       
         SR    R5,R5                                                            
         SR    R7,R7                                                            
         LA    R4,REC                                                           
         ICM   R5,3,REC+13                                                      
         LA    R6,REC2                                                          
         ICM   R7,3,REC2+13                                                     
         CLCL  R4,R6                                                            
         BE    FMTSTAS             NO NEED TO WRITE BACK                        
*                                                                               
WRITEBK  GOTO1 PUTREC                                                           
         B     FMTSTAS                                                          
         EJECT                                                                  
FMTSTAS  DS    0H                                                               
         LA    R7,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
         USING DOVEL05,R7                                                       
         LA    R2,DMVHD1H                                                       
         LA    R5,MAXLINES                                                      
*                                                                               
FMTS     LR    R3,R2                                                            
         LA    R4,DMVIMP1H-DMVHD1H(R2)                                          
         XC    8(L'DMVHD1,R3),8(R3)                                             
         FOUT  (R3)                                                             
         LA    R9,MAXCOLS                                                       
*                                                                               
FMTS1    BAS   RE,NEXTEL                                                        
         BE    FMTS1C                                                           
FMTS1A   XC    8(L'DMVIMP1,R4),8(R4)                                            
         FOUT  (R4)                                                             
         B     FMTS6                                                            
*                                                                               
FMTS1C   CLI   DOVSTA,0                                                         
         BE    FMTS2                                                            
         GOTO1 VMSUNPK,DMCB,0(R7),WORK,8(R3)                                    
         MVI   12(R3),C' '           BLANK N                                    
         B     FMTS4                                                            
*                                                                               
FMTS2    DS    0H                                                               
         CLI   LFMKEXP+20,C'R'     SEE IF DOING A RATING                        
         BE    FMTS3               NO - THEN NO SPILL                           
         CLI   LFMKEXP+20,C'E'     SEE IF DOING AN EXTENDED DEMO                
         BNE   FMTS1A              NO - THEN NO SPILL                           
FMTS3    MVC   HALF,DOVMKT         SPILL MARKET                                 
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  9(4,R3),DUB                                                      
         MVI   8(R3),C'*'                                                       
*                                                                               
FMTS4    DS    0H                                                               
         ZIC   R0,SVEST            SVEST HAS DEMO DISPLACEMENT                  
         SLL   R0,1                X 2                                          
         LA    R6,DOVDEMV                                                       
         AR    R6,R0                                                            
         OC    0(2,R6),0(R6)                                                    
         BZ    FMTS1A                                                           
*                                                                               
FMTS4B   DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,3,0(R6)          CHECK IF AMOUNT OVER 99.9                    
         CH    R1,=H'999'                                                       
         BH    FMTS4C                                                           
         EDIT  (B2,0(R6)),(5,WORK),1,ALIGN=LEFT                                 
         B     FMTS4D                                                           
*                                                                               
FMTS4C   DS    0H                                                               
         SR    R0,R0               GET RID OF DECIMAL                           
         D     R0,=F'10'                                                        
         ST    R1,FULL                                                          
         EDIT  FULL,(5,WORK),ALIGN=LEFT                                         
*                                                                               
FMTS4D   MVC   8(4,R4),WORK                      EDIT W/O DECIMAL               
*                                                                               
FMTS4X   FOUT  (R4)                                                             
*                                                                               
FMTS6    LA    R3,7(R3)            NEXT PLACE IN HDR LINE                       
         LA    R4,UNPLEN(R4)       NEXT IMP LINE                                
         BCT   R9,FMTS1            SAME LINE                                    
         LA    R2,PLEN(R2)         GO TO NEXT HDR LINE                          
         BCT   R5,FMTS                                                          
*                                                                               
FMTSX    B     EXXMOD                                                           
         DROP  R7                                                               
         EJECT                                                                  
EDT      DS    0H                                                               
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         LA    R7,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
         USING DOVEL05,R7                                                       
         LA    R2,DMVIMP1H         FIRST IMP FLD                                
EDT2     BAS   RE,NEXTEL                                                        
         BE    EDT4                                                             
*                                                                               
*                            END OF RECORD                                      
         CLI   5(R2),0             SHOULD BE NO INPUT                           
         BE    NEXTUN                                                           
EDTINV   MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
EDT4     IF    LFMKEXP+20,EQ,C'R',OR,DOVSTA,NE,X'00',EDT6                       
         IF    LFMKEXP+20,EQ,C'E',EDT6                                          
         CLI   5(R2),0             NO INPUT ALLOWED FOR SPILL MKTS              
         BE    NEXTUN              IF DOING IMPS                                
         B     EDTINV                                                           
*                                                                               
*                                                                               
EDT6     CLI   5(R2),0                                                          
         BNE   EDT7                                                             
         ZAP   DUB(6),=P'0'    NO INPUT -  OK                                   
         B     EDT7G                                                            
*                                                                               
*                                                                               
EDT7     DS    0H                                                               
         ZIC   R3,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,8(R2)),(R3)                                     
         CLI   DMCB,X'FF'          INVALID                                      
         BNE   EDT7B                                                            
EDT7ERR  MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
EDT7B    L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
*                                                                               
EDT7D    DP    DUB,=P'10'                                                       
EDT7E    CP    DUB+6(2),=P'0'      MUST GET 0 REMAINDER                         
         BNE   EDT7ERR                                                          
         CP    DUB(6),=P'0'                                                     
         BL    EDT7ERR             CAN'T BE NEGATIVE                            
EDT7G    LA    R4,DOVDEMV                                                       
         ZIC   R0,SVEST                                                         
         SLL   R0,1                X 2                                          
         AR    R4,R0                                                            
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         C     R0,=F'32768'            MAX IS X'8000'                           
         BNL   EDT7ERR                                                          
         ST    R0,DMCB                                                          
         MVC   0(2,R4),DMCB+2      STORE DEMO VALUE                             
         B     NEXTUN              GO DO NEXT FIELD                             
*                                                                               
EDTX     DS    0H                  SET LAST ACTIVITY DATE                       
         LA    R2,REC+24                                                        
         CLI   0(R2),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,TODAY                                                         
         MVC   DOVADAT-DOVEL01(3,R2),DUB                                        
         LA    R4,DOVDLSTC                                                      
         ZIC   R0,SVEST            GET DISPLACEMENT                             
         MH    R0,=H'3'                                                         
         AR    R4,R0                                                            
         OI    0(R4),X'80'         SET ON VALUES INPUT - X'80' IN               
*                                  DEMO NUMBER                                  
*                                                                               
         MVC   KEY,SVKEY                                                        
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         GOTO1 GETREC              REREAD BEFORE WRITE                          
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         GOTO1 PUTREC                                                           
         B     EXXMOD                                                           
*                                                                               
NEXTUN   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BE    EDTX                                                             
         CLI   0(R2),9             TAB LINE                                     
         BE    EDTX                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BNZ   NEXTUN              GO TO NEXT FLD                               
         B     EDT2                                                             
*                                                                               
NEXTEL   ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLC   ELCODE,0(R7)                                                     
         BCR   8,RE                                                             
         CLI   0(R7),0             END OF REC                                   
         BNE   *-18                                                             
         LTR   R7,R7                                                            
         BR    RE                                                               
         SPACE 2                                                                
NEXTEL2  ZIC   R0,1(R2)            NEXT ELEM USING R2                           
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BCR   8,RE                                                             
         CLI   0(R2),0                                                          
         BNE   *-18                                                             
         LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLEAREC  DS    0H                                                               
         MVC   HALF,REC+13                                                      
         LH    R0,HALF                                                          
         LA    R4,REC                                                           
         AR    R4,R0                                                            
         XC    0(2,R4),0(R4)       ZERO END OF RECORD                           
         BR    RE                                                               
         EJECT                                                                  
TODAY    NTR1                                                                   
         GOTO1 VDATCON,DMCB,(5,0),(3,DUB)                                       
         XIT1                                                                   
         EJECT                                                                  
BLDTAB   NTR1                BUILD TABLE OF STA AND SPILL MKTS                  
         MVC   KEY+14(4),SVPRDDA  NETW DISK ADDR WAS SAVED IN SVPRDDA           
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R7,REC2+24                                                       
         MVI   ELCODE,X'02'                                                     
         CLI   0(R7),X'02'                                                      
         BE    BLDT5                                                            
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND NETWORK CODE ELEM                  
*                                                                               
BLDT5    MVC   NTCD,2(R7)          SAVE CODE                                    
**NOP    LA    R2,ELEM                                                          
*                                                                               
         L     R2,ELEM                                                          
         LA    R2,TABELEM-WRK2C(R2)                                             
*                                                                               
         LA    R9,MAXELS           SET MAXIMUM                                  
         LA    R7,REC2+24                                                       
         MVI   ELCODE,X'01'                                                     
         CLI   0(R7),X'01'                                                      
         BE    BLD2                                                             
BLD1     BAS   RE,NEXTEL                                                        
         BNE   BLDX                DONE                                         
*                                                                               
         USING NDEFEL01,R7                                                      
BLD2     DS    0H                                                               
         CLC   NDEFSTA,=C'ZZZZ'    NO ZZZZ 05 ELEMS                             
         BE    BLD1                                                             
         MVC   WORK(4),NDEFSTA                                                  
         MVI   WORK+4,C'N'                                                      
         MVC   WORK+10(4),=C'0000'                                              
         GOTO1 VMSPACK,DMCB,WORK+10,WORK,WORK+15                                
         MVC   0(3,R2),WORK+17                                                  
         LA    R2,3(R2)                                                         
         BCT   R9,*+8                                                           
         B     MAXERR                                                           
*                                                                               
*                                                                               
         ST    R7,SAVER7           SAVE NETWORK ELEM ADDR                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D13'     CHECK FOR SPILL RECS                         
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(1),SVKEY+11      RATING SERVICE                            
         MVC   KEY+5(4),WORK                                                    
         GOTO1 HIGH                WORK HAS STATION                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLD8                NO SPILLS                                    
         LA    R7,REC2+1000                                                     
         LA    R7,250(R7)                                                       
         LA    R7,250(R7)                                                       
         ST    R7,AREC             READ INTO REC2+1500                          
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'05'                                                     
         LA    R7,24(R7)           REC2+1524                                    
         CLI   0(R7),X'05'                                                      
         BE    BLD4                                                             
BLD3     BAS   RE,NEXTEL                                                        
         BNE   BLD8                                                             
*                                                                               
BLD4     DS    0H                                                               
         USING SDEFEL05,R7                                                      
         MVC   1(2,R2),SDEFAMKT       STORE SPILL MKT                           
         LA    R2,3(R2)                                                         
         BCT   R9,*+8                                                           
         B     MAXERR                                                           
         B     BLD3                                                             
*                                                                               
BLD8     L     R7,SAVER7           RESTORE R7                                   
         MVI   ELCODE,X'01'        RESET ELCODE                                 
         B     BLD1                                                             
BLDX     XIT1                                                                   
*                                                                               
*AXERR   DC    H'0'                                                             
MAXERR   MVI   ERRCD,TABERR                                                     
         B     LFMERR                                                           
*                                                                               
SAVER7   DS    F                                                                
*                                                                               
NTCD     DS    CL1                 NETWORK CODE - FROM 02 ELEM                  
*                                                                               
         EJECT                                                                  
ADDELS   NTR1                                                                   
*                                                                               
*              BUILD BLANK 05 ELMES AND ADD THEM TO RECORD                      
*              TABELEM CONTAINS LIST OF STATIONS AND SPILL MKTS                 
*                                                                               
         XC    WORK(30),WORK                                                    
         MVI   WORK,X'05'                                                       
         ZIC   R5,REC+25           LENGTH OF 01 ELEM                            
         SH    R5,=H'12'                                                        
         SR    R4,R4                                                            
         D     R4,=F'3'            R5 NOW HAS NUMBER OF DEMOS                   
         SLL   R5,1                TIMES 2 BYTES PER DEMO                       
         AH    R5,=H'5'                                                         
         STC   R5,WORK+1           SET ELEM LENGTH                              
*                                                                               
         L     R2,ELEM                                                          
         LA    R2,TABELEM-WRK2C(R2)                                             
*                                                                               
         LA    R9,MAXELS                                                        
ADDE2    OC    0(3,R2),0(R2)       END OF ELEMS                                 
         BE    ADDE10                                                           
         LA    R7,REC+24                                                        
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   *+6                 GETS ME TO END OF REC                        
         DC    H'0'                SOMETHING SCREWY                             
         MVC   WORK+2(3),0(R2)                                                  
         GOTO1 VRECUP,DMCB,(0,REC),WORK,0(R7)                                   
         LA    R2,3(R2)            NEXT ELEM                                    
         BCT   R9,ADDE2                                                         
         DC    H'0'                TOO MANY ELEMS                               
*                                                                               
ADDE10   DS    0H                                                               
         XIT1                                                                   
LFMERR   GOTO1 ERROR                                                            
*                                                                               
MAXLINES EQU   8                                                                
MAXCOLS  EQU   11                                                               
MAXELS   EQU   88                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
*                                                                               
         ORG   LFMTABH                                                          
*SPLFMEC                                                                        
       ++INCLUDE SPLFMECD                                                       
         EJECT                                                                  
*                                                                               
PLEN     EQU   DMVHD2H-DMVHD1H                                                  
UNPLEN   EQU   DMVIMP2H-DMVIMP1H                                                
*                                                                               
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
*                                                                               
WRK2C    DSECT                                                                  
TABELEM  DS    (MAXELS+1)CL3                                                    
WRK2CX   EQU   *                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSDEF                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENDOV                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070SPLFM2C   05/01/02'                                      
         END                                                                    
